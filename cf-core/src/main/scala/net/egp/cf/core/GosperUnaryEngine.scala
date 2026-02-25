// GosperUnaryEngine.scala v8
package net.egp.cf.core

import scala.collection.mutable

/**
 * GosperUnaryEngine
 *
 * Streaming continued fraction transformation using UnaryTransform.
 *
 * Diagnostics (opt-in, no ZIO dependency):
 *   - SCALACF_DEBUG=1 enables diagnostics.
 *   - SCALACF_TRACE_EVERY=N prints snapshot every N steps (0 disables).
 *   - SCALACF_TRACE_FIRST=N prints snapshots for first N steps (default 50 when debug on).
 *   - SCALACF_SLOW_STEP_MS=M fails fast if any single step takes > M ms (default 1000 when debug on).
 *
 * Bounded mode:
 *   - maxSteps (hard cap)
 *   - stagnationLimit (steps without emission)
 *   - cycle detection based on (transform state + xRange) using cheap signatures
 *
 * IMPORTANT (correct semantics):
 *   - xRange tracks the *input operand remainder* range.
 *   - On INGEST of input term t:
 *       xRange := xRange.reciprocalSubtract(t)
 *   - On EMIT of output digit a:
 *       z := z.emit(a)
 *       xRange is NOT changed (input operand did not change)
 *
 * Additional safety:
 *   - After the first emitted digit, all subsequent digits must be >= 1 (standard CF terms).
 */
object GosperUnaryEngine:

  ////////////////////////////////////////////////////////////
  // Diagnostics
  ////////////////////////////////////////////////////////////

  private val DebugEnabled: Boolean =
    sys.env.get("SCALACF_DEBUG").exists(_.trim == "1")

  private val TraceEvery: Int =
    sys.env.get("SCALACF_TRACE_EVERY").flatMap(s => s.trim.toIntOption).getOrElse(0)

  private val TraceFirst: Int =
    sys.env.get("SCALACF_TRACE_FIRST").flatMap(s => s.trim.toIntOption).getOrElse(if DebugEnabled then 50 else 0)

  private val SlowStepMs: Long =
    sys.env.get("SCALACF_SLOW_STEP_MS").flatMap(s => s.trim.toLongOption).getOrElse(if DebugEnabled then 1000L else 0L)

  private def dbg(msg: => String): Unit =
    if DebugEnabled then Console.err.println(msg)

  private def trace(msg: => String): Unit =
    if DebugEnabled then Console.err.println(msg)

  private final case class Ctrs(steps: Int, emitted: Int, ingX: Int, extractNone: Int)

  private def fmtBigInt(b: BigInt): String =
    val bl = b.bitLength
    val lo = java.lang.Long.toHexString(b.longValue)
    s"bl=$bl lo=0x$lo"

  private def fmtRat(r: Rational): String =
    if r.isPosInf then "+Inf"
    else if r.isNegInf then "-Inf"
    else s"n(${fmtBigInt(r.n)}) d(${fmtBigInt(r.d)})"

  private def fmtRange(r: Range): String =
    s"[min=${fmtRat(r.min)} max=${fmtRat(r.max)}]"

  ////////////////////////////////////////////////////////////
  // Cheap signatures for cycle detection
  ////////////////////////////////////////////////////////////

  private def sigBigInt(b: BigInt): Long =
    val sign = b.signum.toLong & 0x3L
    val bits = b.bitLength.toLong & 0x3FFFL
    val low  = b.longValue
    (sign << 62) ^ (bits << 48) ^ low

  private def sigRational(r: Rational): Long =
    if r.isPosInf then 0x7ff0_0000_0000_0001L
    else if r.isNegInf then 0x7ff0_0000_0000_0002L
    else sigBigInt(r.n) ^ java.lang.Long.rotateLeft(sigBigInt(r.d), 17)

  private def sigRange(rr: Range): Long =
    sigRational(rr.min) ^ java.lang.Long.rotateLeft(sigRational(rr.max), 23)

  private def sigTransform(z: AnyRef): Long =
    val s = z.toString
    val n = math.min(128, s.length)
    var h = 0x9E3779B97F4A7C15L
    var i = 0
    while i < n do
      h = (h ^ s.charAt(i).toLong) * 0xBF58476D1CE4E5B9L
      i += 1
    h ^ (h >>> 31)

  private def fingerprintCheap(z: AnyRef, xRange: Range): Long =
    var h = 0x9E3779B97F4A7C15L
    def mix(x: Long): Unit =
      h ^= x
      h *= 0xBF58476D1CE4E5B9L
      h ^= (h >>> 31)

    mix(sigTransform(z))
    mix(sigRange(xRange))
    h

  ////////////////////////////////////////////////////////////
  // Public API
  ////////////////////////////////////////////////////////////

  def run[Z <: UnaryTransform[Z]](
    z0: Z,
    xTerms0: LazyList[BigInt],
    xRange0: Range,
    maxDigits: Int,
    maxSteps: Int
  ): LazyList[BigInt] =
    require(z0 != null)
    require(xTerms0 != null)
    require(xRange0 != null)
    require(maxDigits >= 0)
    require(maxSteps >= 0)

    val stagnationLimit = 400
    val cycleWindow     = 512
    val fpStride        = 8

    dbg(s"[GUE] run maxDigits=$maxDigits maxSteps=$maxSteps z=${z0.getClass.getSimpleName} traceFirst=$TraceFirst traceEvery=$TraceEvery slowStepMs=$SlowStepMs")

    val seen = new mutable.LinkedHashSet[Long]()

    def remember(fp: Long): Unit =
      seen.add(fp)
      if seen.size > cycleWindow then
        val it = seen.iterator
        if it.hasNext then seen.remove(it.next())

    def failDiag(msg: String, c: Ctrs, z: Z, xRange: Range): Nothing =
      val fp = fingerprintCheap(z.asInstanceOf[AnyRef], xRange)
      throw new AssertionError(
        s"$msg steps=${c.steps} emitted=${c.emitted} ingX=${c.ingX} extractNone=${c.extractNone} " +
          s"fp=0x${java.lang.Long.toHexString(fp)} xRange=${fmtRange(xRange)} zClass=${z.getClass.getName}"
      )

    def shouldTraceStep(step: Int): Boolean =
      (DebugEnabled && step < TraceFirst) ||
        (DebugEnabled && TraceEvery > 0 && (step % TraceEvery) == 0)

    def traceSnapshot(c: Ctrs, z: Z, xRange: Range, mode: String, digitOpt: Option[BigInt], widthStr: String): Unit =
      if shouldTraceStep(c.steps) then
        val digitStr = digitOpt.map(d => s" digit=$d").getOrElse("")
        trace(
          s"[GUE] step=${c.steps} mode=$mode$digitStr emitted=${c.emitted} ingX=${c.ingX} none=${c.extractNone} $widthStr"
        )

    def guardSlow(op: String, startedNs: Long, c: Ctrs, z: Z, xRange: Range): Unit =
      if SlowStepMs > 0 then
        val elapsedMs = (System.nanoTime() - startedNs) / 1_000_000L
        if elapsedMs > SlowStepMs then
          failDiag(s"GosperUnaryEngine: slow step: $op took ${elapsedMs}ms", c, z, xRange)

    def go(
      z: Z,
      xs: LazyList[BigInt],
      xRange: Range,
      digitsLeft: Int,
      stepsLeft: Int,
      c: Ctrs,
      sinceEmit: Int
    ): LazyList[BigInt] =

      if digitsLeft == 0 then LazyList.empty
      else
        if (c.steps % fpStride) == 0 then
          val fp = fingerprintCheap(z.asInstanceOf[AnyRef], xRange)
          if seen.contains(fp) then failDiag("GosperUnaryEngine: cycle detected", c, z, xRange)
          remember(fp)

        if stepsLeft == 0 then failDiag("GosperUnaryEngine: exceeded maxSteps", c, z, xRange)
        if sinceEmit >= stagnationLimit then failDiag("GosperUnaryEngine: stagnation (no emission)", c, z, xRange)

        // extraction step (may be expensive)
        val t0 = System.nanoTime()
        val ext = z.extractSafe(xRange)
        guardSlow("extractSafe", t0, c, z, xRange)

        ext match

          case Some(digit) =>
            // CF legality: after first term, digits must be >= 1
            if c.emitted >= 1 && digit < 1 then
              failDiag(s"GosperUnaryEngine: illegal CF digit $digit (must be >= 1 after first term)", c, z, xRange)

            val widthStr =
              if shouldTraceStep(c.steps) then
                val t1 = System.nanoTime()
                val out = z.range(xRange)
                guardSlow("range(after extractSafe=Some)", t1, c, z, xRange)
                val w = out.width
                if w.isInfinite then "width=Inf" else s"width=${fmtRat(w)}"
              else "width=?"

            traceSnapshot(c, z, xRange, mode = "emit", digitOpt = Some(digit), widthStr = widthStr)

            val t2 = System.nanoTime()
            val newZ = z.emit(digit)
            guardSlow("emit", t2, c, z, xRange)

            // IMPORTANT: do NOT alter xRange on emit
            digit #:: go(
              newZ,
              xs,
              xRange,
              digitsLeft - 1,
              stepsLeft - 1,
              c.copy(steps = c.steps + 1, emitted = c.emitted + 1),
              sinceEmit = 0
            )

          case None =>
            val widthStr =
              if shouldTraceStep(c.steps) then
                val t1 = System.nanoTime()
                val out = z.range(xRange)
                guardSlow("range(after extractSafe=None)", t1, c, z, xRange)
                val w = out.width
                if w.isInfinite then "width=Inf" else s"width=${fmtRat(w)}"
              else "width=?"

            traceSnapshot(c, z, xRange, mode = "ingX", digitOpt = None, widthStr = widthStr)

            xs match
              case head #:: tail =>
                val t2 = System.nanoTime()
                val newZ = z.ingestX(head)
                guardSlow("ingestX", t2, c, z, xRange)

                val t3 = System.nanoTime()
                val newXRange = xRange.reciprocalSubtract(head)
                guardSlow("xRange.reciprocalSubtract(on ingest)", t3, c, z, xRange)

                go(
                  newZ,
                  tail,
                  newXRange,
                  digitsLeft,
                  stepsLeft - 1,
                  c.copy(steps = c.steps + 1, ingX = c.ingX + 1, extractNone = c.extractNone + 1),
                  sinceEmit = sinceEmit + 1
                )

              case _ =>
                LazyList.empty

    go(
      z0,
      xTerms0,
      xRange0,
      digitsLeft = maxDigits,
      stepsLeft  = maxSteps,
      c          = Ctrs(steps = 0, emitted = 0, ingX = 0, extractNone = 0),
      sinceEmit  = 0
    )

// GosperUnaryEngine.scala v8