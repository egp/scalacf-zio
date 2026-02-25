// GosperBinaryEngine.scala v8
package net.egp.cf.core

import scala.collection.mutable

/**
 * GosperBinaryEngine
 *
 * Streaming continued fraction arithmetic using Bilinear Fractional Transformations (BLFT).
 *
 * Diagnostics (opt-in, no ZIO dependency):
 *   - Set env SCALACF_DEBUG=1 to print key branch decisions + periodic step snapshots.
 *   - Set env SCALACF_TRACE_EVERY=N (e.g. 128, 256) to print a snapshot every N steps.
 *
 * Bounded mode (used by vectors):
 *   - maxSteps
 *   - stagnationLimit (steps without emission)
 *   - cycle detection based on (BLFT coeffs + xRange + yRange) using cheap signatures
 *
 * IMPORTANT:
 *   Emitting an output digit updates only the transform via z.emit(digit).
 *   It MUST NOT modify xRange/yRange.
 */
object GosperBinaryEngine:

  ////////////////////////////////////////////////////////////
  // Diagnostics
  ////////////////////////////////////////////////////////////

  private val DebugEnabled: Boolean =
    sys.env.get("SCALACF_DEBUG").exists(_.trim == "1")

  private val TraceEvery: Int =
    sys.env.get("SCALACF_TRACE_EVERY").flatMap(s => s.trim.toIntOption).getOrElse(0)

  private def dbg(msg: => String): Unit =
    if DebugEnabled then Console.err.println(msg)

  private def trace(msg: => String): Unit =
    if DebugEnabled && TraceEvery > 0 then Console.err.println(msg)

  private final case class Ctrs(steps: Int, emitted: Int, ingX: Int, ingY: Int, extractNone: Int)

  private def fmtBigInt(b: BigInt): String =
    // Never dump full BigInt; bitLength + low 64 bits is enough for diagnostics.
    val bl = b.bitLength
    val lo = java.lang.Long.toHexString(b.longValue)
    s"bl=$bl lo=0x$lo"

  private def fmtRat(r: Rational): String =
    if r.isPosInf then "+Inf"
    else if r.isNegInf then "-Inf"
    else s"n(${fmtBigInt(r.n)}) d(${fmtBigInt(r.d)})"

  private def fmtRange(r: Range): String =
    // Assumes Range exposes min/max rationals (as in your codebase).
    s"[min=${fmtRat(r.min)} max=${fmtRat(r.max)}]"

  private def fmtZMaxBits(z: BLFT): String =
    val mx = List(z.a, z.b, z.c, z.d, z.e, z.f, z.g, z.h).map(_.bitLength).max
    s"zMaxBitLen=$mx"

  ////////////////////////////////////////////////////////////
  // ULFT specialization when one operand is a finite Rational
  ////////////////////////////////////////////////////////////

  /**
   * Substitute y = r into:
   *   (a x y + b x + c y + d) / (e x y + f x + g y + h)
   *
   * Result is a unary LFT in x:
   *   ((a*r + b) x + (c*r + d)) / ((e*r + f) x + (g*r + h))
   */
  private def specializeY(z: BLFT, r: Rational): ULFT =
    ULFT(
      a = (r * z.a) + z.b,
      b = (r * z.c) + z.d,
      c = (r * z.e) + z.f,
      d = (r * z.g) + z.h
    )

  /**
   * Substitute x = r; result is unary LFT in y:
   *   ((a*r + c) y + (b*r + d)) / ((e*r + g) y + (f*r + h))
   */
  private def specializeX(z: BLFT, r: Rational): ULFT =
    ULFT(
      a = (r * z.a) + z.c,
      b = (r * z.b) + z.d,
      c = (r * z.e) + z.g,
      d = (r * z.f) + z.h
    )

  /**
   * Wrap ULFT into UnaryTransform without changing ULFT’s core API.
   */
  private final case class ZU(z: ULFT) extends UnaryTransform[ZU]:
    override def extractSafe(xRange: Range): Option[BigInt] = z.extractSafe(xRange)
    override def emit(digit: BigInt): ZU                   = ZU(z.emit(digit))
    override def ingestX(term: BigInt): ZU                 = ZU(z.ingestX(term))
    override def range(xRange: Range): Range               = z.range(xRange)

  ////////////////////////////////////////////////////////////
  // choose ingestion operand (unbounded heuristic)
  ////////////////////////////////////////////////////////////

  /**
   * Choose whether to ingest X or Y based on uncertainty reduction.
   * true  → ingest X
   * false → ingest Y
   */
  private def chooseOperandByRangeReduction(
    z: BLFT,
    xHead: Option[BigInt],
    yHead: Option[BigInt],
    xRange: Range,
    yRange: Range
  ): Boolean =

    val xWidth =
      xHead match
        case Some(digit) =>
          val newZ = z.ingestX(digit)
          newZ.range(xRange.reciprocalSubtract(digit), yRange).width
        case None =>
          Rational.PositiveInfinity

    val yWidth =
      yHead match
        case Some(digit) =>
          val newZ = z.ingestY(digit)
          newZ.range(xRange, yRange.reciprocalSubtract(digit)).width
        case None =>
          Rational.PositiveInfinity

    xWidth <= yWidth

  ////////////////////////////////////////////////////////////
  // unbounded loop (library semantics)
  ////////////////////////////////////////////////////////////

  private def loop(
    z: BLFT,
    xs: LazyList[BigInt],
    ys: LazyList[BigInt],
    xRange: Range,
    yRange: Range
  ): LazyList[BigInt] =

    z.extractSafe(xRange, yRange) match

      case Some(digit) =>
        val out = z.range(xRange, yRange)
        require(!out.min.isInfinite && !out.max.isInfinite, "extractSafe emitted on unbounded range")
        require(out.min.floor == out.max.floor, "extractSafe emitted without common floor")
        require(out.min.floor == digit, "extractSafe emitted digit inconsistent with floor")

        val newZ = z.emit(digit)

        // CRITICAL: emitting output does not alter operand ranges
        digit #:: loop(newZ, xs, ys, xRange, yRange)

      case None =>
        val ingestX =
          chooseOperandByRangeReduction(z, xs.headOption, ys.headOption, xRange, yRange)

        if ingestX then
          xs match
            case head #:: tail =>
              val newZ      = z.ingestX(head)
              val newXRange = xRange.reciprocalSubtract(head)
              loop(newZ, tail, ys, newXRange, yRange)
            case _ =>
              LazyList.empty
        else
          ys match
            case head #:: tail =>
              val newZ      = z.ingestY(head)
              val newYRange = yRange.reciprocalSubtract(head)
              loop(newZ, xs, tail, xRange, newYRange)
            case _ =>
              LazyList.empty

  ////////////////////////////////////////////////////////////
  // bounded loop (vectors / DfT) with cheap cycle detection
  ////////////////////////////////////////////////////////////

  // cheap signature helpers (avoid BigInt.hashCode on huge values)
  private def sigBigInt(b: BigInt): Long =
    val sign = b.signum.toLong & 0x3L
    val bits = b.bitLength.toLong & 0x3FFFL
    val low  = b.longValue // low 64 bits
    (sign << 62) ^ (bits << 48) ^ low

  private def sigRational(r: Rational): Long =
    if r.isPosInf then 0x7ff0_0000_0000_0001L
    else if r.isNegInf then 0x7ff0_0000_0000_0002L
    else sigBigInt(r.n) ^ java.lang.Long.rotateLeft(sigBigInt(r.d), 17)

  private def sigRange(rr: Range): Long =
    sigRational(rr.min) ^ java.lang.Long.rotateLeft(sigRational(rr.max), 23)

  private def fingerprintCheap(z: BLFT, xRange: Range, yRange: Range): Long =
    var h = 0x9E3779B97F4A7C15L
    def mix(x: Long): Unit =
      h ^= x
      h *= 0xBF58476D1CE4E5B9L
      h ^= (h >>> 31)

    mix(sigBigInt(z.a)); mix(sigBigInt(z.b)); mix(sigBigInt(z.c)); mix(sigBigInt(z.d))
    mix(sigBigInt(z.e)); mix(sigBigInt(z.f)); mix(sigBigInt(z.g)); mix(sigBigInt(z.h))
    mix(sigRange(xRange))
    mix(sigRange(yRange))
    h

  private def loopBounded(
    z0: BLFT,
    xs0: LazyList[BigInt],
    ys0: LazyList[BigInt],
    xRange0: Range,
    yRange0: Range,
    maxSteps: Int,
    stagnationLimit: Int = 400,
    cycleWindow: Int = 512,
    fpStride: Int = 8
  ): LazyList[BigInt] =

    require(maxSteps >= 0)
    require(stagnationLimit > 0)
    require(cycleWindow > 0)
    require(fpStride > 0)

    val seen = new mutable.LinkedHashSet[Long]()

    def remember(fp: Long): Unit =
      seen.add(fp)
      if seen.size > cycleWindow then
        val it = seen.iterator
        if it.hasNext then seen.remove(it.next())

    def failDiag(msg: String, c: Ctrs, z: BLFT, xRange: Range, yRange: Range): Nothing =
      val fp = fingerprintCheap(z, xRange, yRange)
      throw new AssertionError(
        s"$msg steps=${c.steps} emitted=${c.emitted} ingX=${c.ingX} ingY=${c.ingY} extractNone=${c.extractNone} " +
          s"fp=0x${java.lang.Long.toHexString(fp)} ${fmtZMaxBits(z)} xRange=${fmtRange(xRange)} yRange=${fmtRange(yRange)}"
      )

    def maybeTraceSnapshot(
      c: Ctrs,
      z: BLFT,
      xRange: Range,
      yRange: Range,
      mode: String,
      digitOpt: Option[BigInt]
    ): Unit =
      if DebugEnabled && TraceEvery > 0 && (c.steps % TraceEvery) == 0 then
        val out = z.range(xRange, yRange)
        val outWidth = out.width
        val outWidthStr =
          if outWidth.isInfinite then "width=Inf"
          else s"width=${fmtRat(outWidth)}"

        val digitStr =
          digitOpt match
            case Some(d) => s"digit=$d"
            case None    => ""

        trace(
          s"[GBE] step=${c.steps} mode=$mode $digitStr emitted=${c.emitted} ingX=${c.ingX} ingY=${c.ingY} " +
            s"sinceNone=${c.extractNone} $outWidthStr ${fmtZMaxBits(z)}"
        )

    def go(
      z: BLFT,
      xs: LazyList[BigInt],
      ys: LazyList[BigInt],
      xRange: Range,
      yRange: Range,
      stepsLeft: Int,
      c: Ctrs,
      sinceEmit: Int
    ): LazyList[BigInt] =

      if (c.steps % fpStride) == 0 then
        val fp = fingerprintCheap(z, xRange, yRange)
        if seen.contains(fp) then failDiag("GosperBinaryEngine: cycle detected", c, z, xRange, yRange)
        remember(fp)

      if stepsLeft == 0 then failDiag("GosperBinaryEngine: exceeded maxSteps", c, z, xRange, yRange)
      if sinceEmit >= stagnationLimit then failDiag("GosperBinaryEngine: stagnation (no emission)", c, z, xRange, yRange)

      z.extractSafe(xRange, yRange) match

        case Some(digit) =>
          maybeTraceSnapshot(c, z, xRange, yRange, mode = "emit", digitOpt = Some(digit))

          val out = z.range(xRange, yRange)
          require(!out.min.isInfinite && !out.max.isInfinite, "extractSafe emitted on unbounded range")
          require(out.min.floor == out.max.floor, "extractSafe emitted without common floor")
          require(out.min.floor == digit, "extractSafe emitted digit inconsistent with floor")

          val newZ = z.emit(digit)

          digit #:: go(
            newZ,
            xs,
            ys,
            xRange,
            yRange,
            stepsLeft - 1,
            c.copy(steps = c.steps + 1, emitted = c.emitted + 1),
            sinceEmit = 0
          )

        case None =>
          maybeTraceSnapshot(c, z, xRange, yRange, mode = "ing?", digitOpt = None)

          val ingestX =
            chooseOperandByRangeReduction(z, xs.headOption, ys.headOption, xRange, yRange)

          if ingestX then
            xs match
              case head #:: tail =>
                val newZ      = z.ingestX(head)
                val newXRange = xRange.reciprocalSubtract(head)
                go(
                  newZ,
                  tail,
                  ys,
                  newXRange,
                  yRange,
                  stepsLeft - 1,
                  c.copy(steps = c.steps + 1, ingX = c.ingX + 1, extractNone = c.extractNone + 1),
                  sinceEmit = sinceEmit + 1
                )
              case _ =>
                LazyList.empty
          else
            ys match
              case head #:: tail =>
                val newZ      = z.ingestY(head)
                val newYRange = yRange.reciprocalSubtract(head)
                go(
                  newZ,
                  xs,
                  tail,
                  xRange,
                  newYRange,
                  stepsLeft - 1,
                  c.copy(steps = c.steps + 1, ingY = c.ingY + 1, extractNone = c.extractNone + 1),
                  sinceEmit = sinceEmit + 1
                )
              case _ =>
                LazyList.empty

    go(
      z0,
      xs0,
      ys0,
      xRange0,
      yRange0,
      maxSteps,
      Ctrs(steps = 0, emitted = 0, ingX = 0, ingY = 0, extractNone = 0),
      sinceEmit = 0
    )

  ////////////////////////////////////////////////////////////
  // public entrypoints
  ////////////////////////////////////////////////////////////

  /**
   * Unbounded run.
   */
  def run(z: BLFT, x: ContinuedFraction, y: ContinuedFraction): ContinuedFraction =
    require(z != null && x != null && y != null)

    val terms =
      loop(z, x.terms, y.terms, x.range, y.range)

    val out =
      ContinuedFraction.fromTerms(terms)

    require(out.terms.nonEmpty, "GosperBinaryEngine produced empty CF")
    out

  /**
   * Bounded run used by vector tests.
   *
   * Includes specialization fast-path when exactly one operand is a finite Rational.
   */
  def runBounded(
    z: BLFT,
    x: ContinuedFraction,
    y: ContinuedFraction,
    maxDigits: Int,
    maxSteps: Int
  ): ContinuedFraction =
    require(z != null && x != null && y != null)
    require(maxDigits >= 0 && maxSteps >= 0)

    val xOpt = x.toRationalOption()
    val yOpt = y.toRationalOption()

    if DebugEnabled then
      dbg(
        s"[GBE] runBounded maxDigits=$maxDigits maxSteps=$maxSteps " +
          s"xOpt=${xOpt.isDefined} yOpt=${yOpt.isDefined} op=${z.toString}"
      )

    val terms: LazyList[BigInt] =
      (xOpt, yOpt) match
        case (Some(rx), Some(ry)) =>
          dbg("[GBE] branch=(Some,Some) exact eval -> CF")
          z.eval(rx, ry).toContinuedFraction.terms.take(maxDigits)

        case (None, Some(ry)) =>
          dbg("[GBE] branch=(None,Some) specializeY -> unary engine")
          val u = specializeY(z, ry)
          GosperUnaryEngine.run(
            z0        = ZU(u),
            xTerms0   = x.terms,
            xRange0   = x.range,
            maxDigits = maxDigits,
            maxSteps  = maxSteps
          )

        case (Some(rx), None) =>
          dbg("[GBE] branch=(Some,None) specializeX -> unary engine")
          val u = specializeX(z, rx)
          GosperUnaryEngine.run(
            z0        = ZU(u),
            xTerms0   = y.terms,
            xRange0   = y.range,
            maxDigits = maxDigits,
            maxSteps  = maxSteps
          )

        case (None, None) =>
          dbg("[GBE] branch=(None,None) full binary bounded loop")
          loopBounded(
            z0 = z,
            xs0 = x.terms,
            ys0 = y.terms,
            xRange0 = x.range,
            yRange0 = y.range,
            maxSteps = maxSteps
          ).take(maxDigits)

    val out =
      ContinuedFraction.fromTerms(terms)

    require(out.terms.nonEmpty, "GosperBinaryEngine.runBounded produced empty CF")
    out

// GosperBinaryEngine.scala v8