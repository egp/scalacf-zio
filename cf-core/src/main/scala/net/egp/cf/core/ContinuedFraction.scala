// ContinuedFraction.scala
package net.egp.cf.core

/**
 * Continued fraction represented as LazyList of terms.
 *
 * Invariants:
 *
 *   a0 ∈ Z
 *   ai ≥ 1 for i ≥ 1
 *
 * IMPORTANT:
 * Invariants are enforced lazily to support non-terminating continued fractions.
 */
final class ContinuedFraction private (
  rawTerms: LazyList[BigInt]
) extends TermSource:

  require(rawTerms != null, "terms cannot be null")
  require(rawTerms.nonEmpty, "terms cannot be empty")

  /**
   * Enforce CF legality lazily:
   * - a0 can be any integer
   * - for i>=1, ai >= 1
   *
   * This must NOT traverse an infinite LazyList.
   */
  private val checkedTerms: LazyList[BigInt] =
    rawTerms match
      case a0 #:: rest =>
        require(a0 != null, "continued fraction term a0 cannot be null")
        a0 #:: rest.map { t =>
          require(t != null, "continued fraction term cannot be null")
          require(t >= 1, s"continued fraction invariant violated: term $t < 1")
          t
        }
      case _ =>
        // unreachable because of require(nonEmpty), but explicit for totality
        throw new IllegalArgumentException("terms cannot be empty")

  override val terms: LazyList[BigInt] =
    checkedTerms


  ////////////////////////////////////////////////////////////
  // convergents
  ////////////////////////////////////////////////////////////

  lazy val convergents: LazyList[Rational] =

    def loop(
      hPrev: BigInt,
      hPrevPrev: BigInt,
      kPrev: BigInt,
      kPrevPrev: BigInt,
      rest: LazyList[BigInt]
    ): LazyList[Rational] =

      rest match

        case LazyList() =>
          LazyList.empty

        case a #:: tail =>

          val h = a*hPrev + hPrevPrev
          val k = a*kPrev + kPrevPrev

          Rational(h,k) #:: loop(
            h,
            hPrev,
            k,
            kPrev,
            tail
          )

    loop(
      1,
      0,
      0,
      1,
      terms
    )


  ////////////////////////////////////////////////////////////
  // exact conversion helpers
  ////////////////////////////////////////////////////////////

  /**
   * Convert to Rational if the CF terminates.
   * For non-terminating CFs, returns None.
   *
   * DfT: bounded term limit prevents accidental non-termination.
   */
  def toRationalOption(maxTerms: Int = 10_000): Option[Rational] =
    require(maxTerms > 0, "maxTerms must be positive")

    // Walk convergents; if terms terminate, convergents terminates too.
    // If terms do not terminate, we stop at maxTerms and return None.
    val conv = convergents.take(maxTerms).toList
    if conv.isEmpty then None
    else
      // Heuristic: if the underlying terms are finite, convergents will end.
      // We can detect termination by comparing take(maxTerms) length vs maxTerms
      // along with checking whether terms.take(maxTerms+1) has more.
      val moreTerms =
        terms.drop(maxTerms).headOption.isDefined
      if moreTerms then None else Some(conv.last)

  /**
   * Convert to Rational, requiring termination.
   */
  def toRational: Rational =
    toRationalOption() match
      case Some(r) => r
      case None    => throw new IllegalStateException("continued fraction is non-terminating")


  ////////////////////////////////////////////////////////////
  // REQUIRED by GosperBinaryEngine
  ////////////////////////////////////////////////////////////

  lazy val range: Range =
    Range.fromCF(this)


  ////////////////////////////////////////////////////////////
  // arithmetic
  ////////////////////////////////////////////////////////////

  def +(that: ContinuedFraction): ContinuedFraction =
    GosperBinaryEngine.run(
      BLFT.add,
      this,
      that
    )

  def -(that: ContinuedFraction): ContinuedFraction =
    GosperBinaryEngine.run(
      BLFT.subtract,
      this,
      that
    )

  def *(that: ContinuedFraction): ContinuedFraction =
    GosperBinaryEngine.run(
      BLFT.multiply,
      this,
      that
    )

  def /(that: ContinuedFraction): ContinuedFraction =
    GosperBinaryEngine.run(
      BLFT.divide,
      this,
      that
    )


////////////////////////////////////////////////////////////
// companion object
////////////////////////////////////////////////////////////

object ContinuedFraction:

  def fromTerms(
    terms: LazyList[BigInt]
  ): ContinuedFraction =

    require(terms != null, "terms cannot be null")
    require(terms.nonEmpty, "terms cannot be empty")

    new ContinuedFraction(terms)

  def fromRational(r: Rational): ContinuedFraction =

    require(r != null, "r cannot be null")
    require(r.isFinite, "fromRational requires finite Rational")

    def loop(x: Rational): LazyList[BigInt] =

      val a = x.floor
      val frac = x - Rational(a,1)

      if frac == Rational(0,1) then
        LazyList(a)
      else
        a #:: loop(frac.reciprocal)

    new ContinuedFraction(loop(r))

  def apply(xs: BigInt*): ContinuedFraction =
    require(xs != null, "xs cannot be null")
    require(xs.nonEmpty, "must provide at least one term")
    new ContinuedFraction(xs.to(LazyList))


// EOF: ContinuedFraction.scala