
---

## `ULFT.scala v8` — integer-matrix GCD normalization

**What it does:** after `emit(k)` and `ingestX(t)`, if `a,b,c,d` are all **finite** and all have **denominator == 1**, it divides the 4 numerators by `gcd(|a|,|b|,|c|,|d|)` when that gcd > 1. This keeps coefficient growth under control without changing the transform.

```scala
// ULFT.scala v8
package net.egp.cf.core

/**
 * ULFT — Unary Linear Fractional Transform
 *
 * Represents the Mobius transform:
 *
 *   f(x) = (a*x + b) / (c*x + d)
 *
 * where a,b,c,d are Rational (extended with ±∞ as used by Rational in this codebase).
 *
 * Design goals:
 *   - total Range propagation (never throws; conservative widening is allowed)
 *   - emission/ingestion are explicit Mobius compositions (Gosper-smile)
 *   - extractSafe is conservative: returns Some(d) only when provably safe over the whole range
 *
 * Performance / normalization:
 *   - After emit/ingestX, we apply integer-matrix gcd normalization when possible:
 *       if a,b,c,d are finite and all denominators are 1, divide by gcd(|a|,|b|,|c|,|d|).
 *     This does not change the transform (global scalar factor cancels) but prevents coefficient blow-up.
 *
 * Optional diagnostics (stderr):
 *   - SCALACF_DEBUG=1 enables ULFT trace prints from range() and intervalContainsZero().
 */
final case class ULFT(
  a: Rational,
  b: Rational,
  c: Rational,
  d: Rational
):
  require(a != null && b != null && c != null && d != null, "ULFT coefficients must not be null")

  ////////////////////////////////////////////////////////////
  // Diagnostics (opt-in)
  ////////////////////////////////////////////////////////////

  private val DebugEnabled: Boolean =
    sys.env.get("SCALACF_DEBUG").exists(_.trim == "1")

  private def dbg(msg: => String): Unit =
    if DebugEnabled then Console.err.println(msg)

  ////////////////////////////////////////////////////////////
  // evaluation
  ////////////////////////////////////////////////////////////

  /** Returns f(x) if defined; None for true undefined forms (0/0). */
  def evalOption(x: Rational): Option[Rational] =
    require(x != null, "x must not be null")
    try
      val num = a * x + b
      val den = c * x + d

      // If denominator is exactly zero:
      if den.isFinite && den.numerator == 0 then
        // 0/0 is undefined; otherwise treat as ±∞ (sign from numerator).
        if num.isFinite && num.numerator == 0 then None
        else if num > Rational(0, 1) then Some(PosInf)
        else if num < Rational(0, 1) then Some(NegInf)
        else None
      else
        Some(num / den)
    catch
      // Rational may throw on undefined ∞ forms; treat as "undefined".
      case _: Throwable => None

  /** Total evaluation: returns ±∞ for denom=0 (unless 0/0, then returns PosInf conservatively). */
  private def evalTotal(x: Rational): Rational =
    evalOption(x) match
      case Some(v) => v
      case None    => PosInf // conservative "unknown" -> tends to widen ranges

  ////////////////////////////////////////////////////////////
  // normalization (integer-matrix gcd)
  ////////////////////////////////////////////////////////////

  private def normalizeIntsIfPossible(u: ULFT): ULFT =
    // Only normalize when all coefficients are finite integers (denominator == 1).
    (u.a, u.b, u.c, u.d) match
      case (Finite(an, ad), Finite(bn, bd), Finite(cn, cd), Finite(dn, dd))
          if ad == 1 && bd == 1 && cd == 1 && dd == 1 =>
        val g0 = an.abs.gcd(bn.abs).gcd(cn.abs).gcd(dn.abs)
        if g0 > 1 then
          val out = ULFT(
            a = Rational(an / g0, 1),
            b = Rational(bn / g0, 1),
            c = Rational(cn / g0, 1),
            d = Rational(dn / g0, 1)
          )
          dbg(s"[ULFT.norm] applied gcd=$g0")
          out
        else u

      case _ =>
        u

  ////////////////////////////////////////////////////////////
  // CF mechanics: emission / ingestion as Mobius compositions
  ////////////////////////////////////////////////////////////

  /**
   * Emit CF digit k from the output, i.e. transform y := f(x) into:
   *
   *   y' = 1 / (y - k)
   *
   * This is left-composition with E_k(y) = 1/(y-k), matrix:
   *   [ 0  1 ]
   *   [ 1 -k ]
   *
   * So:
   *   E_k ∘ f  =>  [0 1; 1 -k] * [a b; c d] = [c d; a-kc b-kd]
   */
  def emit(k: BigInt): ULFT =
    val kk = Rational(k, 1)
    val raw =
      ULFT(
        a = c,
        b = d,
        c = a - kk * c,
        d = b - kk * d
      )
    normalizeIntsIfPossible(raw)

  /**
   * Ingest next CF term t from the input x, i.e. substitute:
   *
   *   x = t + 1/x'
   *
   * This is right-composition with I_t(x') = t + 1/x', matrix:
   *   [ t  1 ]
   *   [ 1  0 ]
   *
   * So:
   *   f ∘ I_t  =>  [a b; c d] * [t 1; 1 0] = [a t + b, a; c t + d, c]
   */
  def ingestX(t: BigInt): ULFT =
    val tt = Rational(t, 1)
    val raw =
      ULFT(
        a = a * tt + b,
        b = a,
        c = c * tt + d,
        d = c
      )
    normalizeIntsIfPossible(raw)

  ////////////////////////////////////////////////////////////
  // Range propagation
  ////////////////////////////////////////////////////////////

  /**
   * Conservative image of the input range under this ULFT.
   *
   * Policy:
   *  - exact input -> exact output (when defined; otherwise widest outside)
   *  - inside interval without pole -> inside of endpoint images (ordered)
   *  - inside interval with pole -> outside, conservatively widened (often all reals)
   *  - outside input -> conservative (outside) by mapping the two "ends"
   */
  def range(x: Range): Range =
    require(x != null, "x must not be null")

    if x.isExact then
      val vOpt = evalOption(x.min)
      vOpt match
        case Some(v) =>
          val out = Range.exact(v)
          dbg(s"[ULFT.range] exact x=$x -> out=$out ulft=$this")
          out
        case None =>
          // totally undefined -> widest
          val out = Range.outside(PosInf, NegInf)
          dbg(s"[ULFT.range] exact x=$x -> undefined -> out=$out ulft=$this")
          out

    else if x.isInside then
      // Check whether a pole lies in [min,max] by testing whether 0 is contained in denom range.
      val denAtMin = evalDen(x.min)
      val denAtMax = evalDen(x.max)

      val polePossible = intervalContainsZero(denAtMin, denAtMax, ctx = s"inside x=$x ulft=$this")

      if polePossible then
        // Discontinuity likely. Safest is "outside all reals" (i.e., unknown but correct).
        val out = Range.outside(PosInf, NegInf)
        dbg(s"[ULFT.range] inside x=$x den(min)=$denAtMin den(max)=$denAtMax polePossible=true -> out=$out ulft=$this")
        out
      else
        val fMin = evalTotal(x.min)
        val fMax = evalTotal(x.max)
        val out = Range.inside(minOf(fMin, fMax), maxOf(fMin, fMax))
        dbg(s"[ULFT.range] inside x=$x den(min)=$denAtMin den(max)=$denAtMax polePossible=false f(min)=$fMin f(max)=$fMax -> out=$out ulft=$this")
        out

    else
      // Outside: x ∈ (-∞, min] ∪ [max, +∞). Conservatively map the endpoints.
      val fMin = evalTotal(x.min)
      val fMax = evalTotal(x.max)
      val out = Range.outside(maxOf(fMin, fMax), minOf(fMin, fMax))
      dbg(s"[ULFT.range] outside x=$x f(min)=$fMin f(max)=$fMax -> out=$out ulft=$this")
      out

  private def evalDen(x: Rational): Rational =
    try c * x + d
    catch case _: Throwable => PosInf

  /**
   * Conservative test: does the interval between u and v (as reals) contain 0?
   *
   * Notes:
   *  - If either endpoint is infinite/unknown-ish, we bias toward "true" (might contain 0).
   *  - If u and v straddle 0 (or one equals 0), return true.
   */
  private def intervalContainsZero(u: Rational, v: Rational, ctx: String): Boolean =
    val zero = Rational(0, 1)

    val result: Boolean =
      if u == zero || v == zero then true
      else if !u.isFinite && !v.isFinite then
        // Any span involving infinities is treated as potentially containing 0.
        true
      else if !u.isFinite && v.isFinite then
        // (-∞ .. v) or (+∞ .. v) as a conservative envelope:
        // contains 0 iff v has opposite sign or is 0 (already handled).
        v <= zero
      else if u.isFinite && !v.isFinite then
        u <= zero
      else
        // both finite
        (u < zero && v > zero) || (v < zero && u > zero)

    dbg(s"[ULFT.zero?] u=$u v=$v -> $result :: $ctx")
    result

  ////////////////////////////////////////////////////////////
  // Safe digit extraction (CF term emission)
  ////////////////////////////////////////////////////////////

  /**
   * Returns Some(k) only if the next CF digit is provably the same for all values
   * in the output range of this transform applied to xRange.
   *
   * Conservative rule:
   *   - Compute r = range(xRange).
   *   - If r is inside/exact and both endpoints are finite and floor(min)==floor(max),
   *     then that floor is safe.
   *   - Otherwise None.
   */
  def extractSafe(xRange: Range): Option[BigInt] =
    val out = range(xRange)

    if out.isExact then
      if out.min.isFinite then Some(out.min.floor) else None

    else if out.isInside then
      if out.min.isFinite && out.max.isFinite then
        val lo = out.min.floor
        val hi = out.max.floor
        if lo == hi then Some(lo) else None
      else None

    else
      None

  ////////////////////////////////////////////////////////////
  // small helpers
  ////////////////////////////////////////////////////////////

  private def minOf(x: Rational, y: Rational): Rational = if x <= y then x else y
  private def maxOf(x: Rational, y: Rational): Rational = if x >= y then x else y

object ULFT:
  /** Identity transform: f(x)=x */
  val identity: ULFT =
    ULFT(Rational(1, 1), Rational(0, 1), Rational(0, 1), Rational(1, 1))

// ULFT.scala v8