// Range.scala v6
package net.egp.cf.core

/**
 * Range over Rational, including infinity.
 *
 * Encoding invariant:
 *
 *   min == max  → exact
 *   min < max   → inside interval
 *   min > max   → outside interval
 *
 * Outside intervals represent complement intervals:
 *   (-∞, max] ∪ [min, +∞)
 *
 * IMPORTANT:
 *   "All reals" must be represented as inside(NegInf, PosInf),
 *   NOT as outside(PosInf, NegInf). The latter does not behave like
 *   universal coverage under this encoding and will poison downstream logic.
 */
final case class Range(
  min: Rational,
  max: Rational
) extends Ordered[Range]:

  require(min != null, "min cannot be null")
  require(max != null, "max cannot be null")

  private def assertEncoding(): Unit =
    require(
      (min == max) || (min < max) || (min > max),
      s"Range encoding invariant violated: min=$min max=$max"
    )

  assertEncoding()

  ////////////////////////////////////////////////////////////
  // classification
  ////////////////////////////////////////////////////////////

  def isExact: Boolean = min == max
  def isInside: Boolean = min < max
  def isOutside: Boolean = min > max

  ////////////////////////////////////////////////////////////
  // width
  ////////////////////////////////////////////////////////////

  def width: Rational =
    val w =
      if isExact then
        Rational(0, 1)
      else if min.isInfinite || max.isInfinite then
        PosInf
      else
        if max >= min then max - min else min - max

    require(w.isPosInf || w >= Rational(0, 1), s"width must be non-negative, found $w")
    w

  ////////////////////////////////////////////////////////////
  // containment
  ////////////////////////////////////////////////////////////

  def contains(x: Rational): Boolean =
    require(x != null)

    if isExact then
      x == min
    else if isInside then
      x >= min && x <= max
    else
      // Outside: (-∞, max] ∪ [min, +∞)
      x <= max || x >= min

  private def containsZero: Boolean =
    contains(Rational(0, 1))

  ////////////////////////////////////////////////////////////
  // reciprocal
  ////////////////////////////////////////////////////////////

  /**
   * Reciprocal mapping: 1 / x over this range.
   *
   * Correct interval behavior:
   *   - If an inside range contains 0, 1/x is disconnected → outside range.
   *   - If an inside range excludes 0, reciprocal stays inside with ordered endpoints.
   *   - Exact 0 maps to exact +Inf (consistent with Rational.reciprocal: 0 -> +Inf).
   *
   * For outside ranges, we must not collapse to "exact 0" when both boundary reciprocals
   * happen to be 0 due to infinities; instead, we conservatively widen.
   */
  def reciprocal: Range =
    val zero = Rational(0, 1)

    val r: Range =
      if isExact then
        Range.exact(min.reciprocal)

      else if isInside then
        if containsZero then
          // Disconnected image; represent as outside using reciprocal of endpoints.
          val rMin = min.reciprocal
          val rMax = max.reciprocal
          Range.outside(maxOf(rMin, rMax), minOf(rMin, rMax))
        else
          val r1 = min.reciprocal
          val r2 = max.reciprocal
          Range.inside(minOf(r1, r2), maxOf(r1, r2))

      else
        // Outside: (-∞, max] ∪ [min, +∞)
        //
        // If the outside set includes 0, reciprocal is also disconnected and effectively
        // spans both signs with an asymptote at 0. The safest encoding for "very wide"
        // is ALL REALS, which MUST be inside(NegInf, PosInf).
        if containsZero then
          Range.allReals
        else
          // Outside set excludes 0. Reciprocal remains outside, but be careful:
          // If boundary reciprocals are equal (can happen with infinities -> 0),
          // do NOT collapse to exact; widen conservatively.
          val r1 = min.reciprocal
          val r2 = max.reciprocal
          if r1 == r2 then
            Range.allReals
          else
            Range.outside(maxOf(r1, r2), minOf(r1, r2))

    r.assertEncoding()
    r

  ////////////////////////////////////////////////////////////
  // reciprocalSubtract: 1 / (range - digit)
  ////////////////////////////////////////////////////////////

  def reciprocalSubtract(digit: BigInt): Range =
    val d = Rational(digit, 1)
    val shifted = Range(min - d, max - d)
    val result = shifted.reciprocal
    result.assertEncoding()
    result

  ////////////////////////////////////////////////////////////
  // uncertainty ordering
  ////////////////////////////////////////////////////////////

  override def compare(that: Range): Int =
    require(that != null)

    if this == that then 0
    else if this.isExact then -1
    else if that.isExact then 1
    else if this.isInside && that.isOutside then -1
    else if this.isOutside && that.isInside then 1
    else if this.isInside && that.isInside then
      this.width.compare(that.width)
    else
      // outside vs outside: wider is less uncertain
      that.width.compare(this.width)

  ////////////////////////////////////////////////////////////
  // small helpers
  ////////////////////////////////////////////////////////////

  private def minOf(x: Rational, y: Rational): Rational = if x <= y then x else y
  private def maxOf(x: Rational, y: Rational): Rational = if x >= y then x else y

////////////////////////////////////////////////////////////
// companion
////////////////////////////////////////////////////////////

object Range:

  def exact(x: Rational): Range =
    require(x != null)
    val r = Range(x, x)
    require(r.isExact)
    r

  def inside(min: Rational, max: Rational): Range =
    require(min != null)
    require(max != null)
    val r = Range(min, max)
    require(r.isInside || r.isExact)
    r

  def outside(min: Rational, max: Rational): Range =
    require(min != null)
    require(max != null)
    val r = Range(min, max)
    require(r.isOutside || r.isExact)
    r

  /** Canonical "all reals" range. */
  val allReals: Range =
    inside(NegInf, PosInf)

  /**
   * If CF terminates, represent it exactly; else bound via convergents.
   */
  def fromCF(cf: ContinuedFraction): Range =
    require(cf != null)

    cf.toRationalOption() match
      case Some(r) =>
        val ex = exact(r)
        require(ex.isExact)
        ex

      case None =>
        val conv = cf.convergents.take(10).toList

        conv match
          case Nil =>
            exact(Rational(0, 1))

          case x :: Nil =>
            exact(x)

          case xs =>
            inside(xs.min, xs.max)

// Range.scala v6