// Rational.scala v6
package net.egp.cf.core

/**
 * Unified Rational supporting:
 *
 *   • finite values
 *   • +∞
 *   • −∞
 *
 * Invariants (finite only):
 *
 *   gcd(num, den) = 1
 *   den > 0
 *
 * Special values:
 *
 *   n/0, n>0  → +∞
 *   n/0, n<0  → −∞
 *   0/0       → illegal
 *
 * This class is the single numeric foundation for:
 *
 *   Range
 *   BLFT
 *   ContinuedFraction
 *   GosperBinaryEngine
 */
sealed trait Rational extends Ordered[Rational]:

  ////////////////////////////////////////////////////////////
  // classification
  ////////////////////////////////////////////////////////////

  def isFinite: Boolean
  def isPosInf: Boolean
  def isNegInf: Boolean

  final def isInfinite: Boolean =
    isPosInf || isNegInf


  ////////////////////////////////////////////////////////////
  // finite accessors
  ////////////////////////////////////////////////////////////

  def numerator: BigInt
  def denominator: BigInt

  // Common aliases (kept additive for compatibility)
  final def n: BigInt = numerator
  final def d: BigInt = denominator
  final def num: BigInt = numerator
  final def den: BigInt = denominator


  ////////////////////////////////////////////////////////////
  // arithmetic
  ////////////////////////////////////////////////////////////

  def unary_- : Rational
  def +(that: Rational): Rational

  def -(that: Rational): Rational =
    this + (-that)

  def *(that: Rational): Rational
  def /(that: Rational): Rational
  def reciprocal: Rational

  // BigInt-right ops (new)
  final def +(k: BigInt): Rational = this + Rational(k, 1)
  final def -(k: BigInt): Rational = this - Rational(k, 1)
  final def *(k: BigInt): Rational = this * Rational(k, 1)
  final def /(k: BigInt): Rational = this / Rational(k, 1)

  final def +(k: Long): Rational = this + BigInt(k)
  final def -(k: Long): Rational = this - BigInt(k)
  final def *(k: Long): Rational = this * BigInt(k)
  final def /(k: Long): Rational = this / BigInt(k)

  def abs: Rational =
    if this < Rational(0,1) then -this else this


  ////////////////////////////////////////////////////////////
  // continued fraction support
  ////////////////////////////////////////////////////////////

  /**
   * Mathematical floor for finite rationals.
   *
   * Preconditions:
   *   isFinite
   */
  def floor: BigInt

  /**
   * Exact continued fraction expansion for finite rationals.
   *
   * IMPORTANT:
   * Uses Euclidean division at each step so that for i>=1:
   *   a_i >= 1
   *
   * Preconditions:
   *   isFinite
   */
  def toContinuedFraction: ContinuedFraction =
    require(isFinite, "Cannot convert infinity to continued fraction")

    val n0 = numerator
    val d0 = denominator

    require(d0 > 0, "finite rational must have positive denominator")

    // Euclidean division with remainder:
    //   n = q*d + r, with 0 <= r < d
    def divModEuclid(n: BigInt, d: BigInt): (BigInt, BigInt) =
      require(d > 0, "divisor must be positive")
      var q = n / d
      var r = n % d
      if r < 0 then
        q = q - 1
        r = r + d
      require(r >= 0 && r < d, s"Euclidean remainder out of range: r=$r d=$d")
      (q, r)

    def loop(n: BigInt, d: BigInt): LazyList[BigInt] =
      if d == 0 then
        LazyList.empty
      else
        val (q, r) = divModEuclid(n, d)
        if r == 0 then
          LazyList(q)
        else
          q #:: loop(d, r)

    ContinuedFraction.fromTerms(loop(n0, d0))


////////////////////////////////////////////////////////////
// finite rational
////////////////////////////////////////////////////////////

final case class Finite(
  numerator: BigInt,
  denominator: BigInt
) extends Rational:

  require(denominator > 0, "finite denominator must be positive")
  require(numerator.gcd(denominator) == 1, "finite rational must be normalized (gcd=1)")

  override val isFinite = true
  override val isPosInf = false
  override val isNegInf = false


  ////////////////////////////////////////////////////////////

  override def unary_- : Rational =
    Finite(-numerator, denominator)


  ////////////////////////////////////////////////////////////

  override def +(that: Rational): Rational =
    require(that != null)

    val result =
      that match
        case Finite(n2, d2) =>
          Rational(
            numerator * d2 + n2 * denominator,
            denominator * d2
          )

        case PosInf => PosInf
        case NegInf => NegInf

    require(result != null)
    result


  ////////////////////////////////////////////////////////////

  override def *(that: Rational): Rational =
    require(that != null)

    val result =
      that match

        case Finite(n2, d2) =>
          Rational(
            numerator * n2,
            denominator * d2
          )

        case PosInf =>
          if numerator == 0 then
            throw new ArithmeticException("0 * ∞ undefined")
          else if numerator > 0 then PosInf else NegInf

        case NegInf =>
          if numerator == 0 then
            throw new ArithmeticException("0 * ∞ undefined")
          else if numerator > 0 then NegInf else PosInf

    require(result != null)
    result


  ////////////////////////////////////////////////////////////

  override def /(that: Rational): Rational =
    require(that != null)

    val result =
      that match

        case Finite(n2, d2) =>
          require(n2 != 0, "division by zero")
          Rational(
            numerator * d2,
            denominator * n2
          )

        case PosInf | NegInf =>
          Rational(0, 1)

    require(result != null)
    result


  ////////////////////////////////////////////////////////////

  override def reciprocal: Rational =
    val result =
      if numerator == 0 then
        PosInf
      else
        Rational(denominator, numerator)

    require(result != null)
    result


  ////////////////////////////////////////////////////////////

  /**
   * Mathematical floor for rationals.
   *
   * For negative values, BigInt division truncates toward zero,
   * so we adjust when there is a remainder.
   */
  override def floor: BigInt =
    val q = numerator / denominator
    val r = numerator % denominator
    if r == 0 then q
    else if numerator >= 0 then q
    else q - 1


  ////////////////////////////////////////////////////////////

  override def compare(that: Rational): Int =
    require(that != null)

    that match
      case Finite(n2, d2) =>
        (numerator * d2).compare(n2 * denominator)

      case PosInf => -1
      case NegInf => 1


////////////////////////////////////////////////////////////
// positive infinity
////////////////////////////////////////////////////////////

case object PosInf extends Rational:

  override val isFinite = false
  override val isPosInf = true
  override val isNegInf = false

  override def numerator =
    throw new UnsupportedOperationException

  override def denominator =
    throw new UnsupportedOperationException

  override def unary_- = NegInf

  override def +(that: Rational) =
    require(that != null)
    that match
      case NegInf =>
        throw new ArithmeticException("∞ − ∞ undefined")
      case _ => PosInf

  override def *(that: Rational) =
    require(that != null)
    that match
      case Finite(n, _) =>
        if n > 0 then PosInf
        else if n < 0 then NegInf
        else throw new ArithmeticException("∞ * 0 undefined")

      case PosInf => PosInf
      case NegInf => NegInf

  override def /(that: Rational) =
    require(that != null)
    that match
      case Finite(n, _) =>
        require(n != 0, "∞ / 0 undefined")
        if n > 0 then PosInf else NegInf

      case _ =>
        throw new ArithmeticException("∞ / ∞ undefined")

  override def reciprocal =
    Rational(0,1)

  override def floor =
    throw new UnsupportedOperationException

  override def compare(that: Rational) =
    require(that != null)
    if that == PosInf then 0 else 1


////////////////////////////////////////////////////////////
// negative infinity
////////////////////////////////////////////////////////////

case object NegInf extends Rational:

  override val isFinite = false
  override val isPosInf = false
  override val isNegInf = true

  override def numerator =
    throw new UnsupportedOperationException

  override def denominator =
    throw new UnsupportedOperationException

  override def unary_- = PosInf

  override def +(that: Rational) =
    require(that != null)
    that match
      case PosInf =>
        throw new ArithmeticException("∞ − ∞ undefined")
      case _ => NegInf

  override def *(that: Rational) =
    require(that != null)
    that match
      case Finite(n, _) =>
        if n > 0 then NegInf
        else if n < 0 then PosInf
        else throw new ArithmeticException("∞ * 0 undefined")

      case PosInf => NegInf
      case NegInf => PosInf

  override def /(that: Rational) =
    require(that != null)
    that match
      case Finite(n, _) =>
        require(n != 0, "∞ / 0 undefined")
        if n > 0 then NegInf else PosInf

      case _ =>
        throw new ArithmeticException("∞ / ∞ undefined")

  override def reciprocal =
    Rational(0,1)

  override def floor =
    throw new UnsupportedOperationException

  override def compare(that: Rational) =
    require(that != null)
    if that == NegInf then 0 else -1


////////////////////////////////////////////////////////////
// companion
////////////////////////////////////////////////////////////

object Rational:

  val PositiveInfinity: Rational = PosInf
  val NegativeInfinity: Rational = NegInf

  /**
   * Normalizing constructor.
   *
   * Postconditions (finite):
   *   - denominator > 0
   *   - gcd(numerator, denominator) == 1
   */
  def apply(n: BigInt, d: BigInt): Rational =

    if d == 0 then
      if n > 0 then PosInf
      else if n < 0 then NegInf
      else throw new IllegalArgumentException("0/0 undefined")

    else
      val g = n.gcd(d)
      val nn = n / g
      val dd = d / g

      val f: Finite =
        if dd > 0 then Finite(nn, dd)
        else Finite(-nn, -dd)

      // DfT postconditions for finite results
      require(f.denominator > 0, "normalized denominator must be positive")
      require(f.numerator.gcd(f.denominator) == 1, "normalized finite must have gcd=1")

      f

  // Symmetric BigInt-left ops (new)
  extension (k: BigInt)
    def +(r: Rational): Rational = Rational(k, 1) + r
    def -(r: Rational): Rational = Rational(k, 1) - r
    def *(r: Rational): Rational = Rational(k, 1) * r
    def /(r: Rational): Rational = Rational(k, 1) / r

  extension (k: Long)
    def +(r: Rational): Rational = BigInt(k) + r
    def -(r: Rational): Rational = BigInt(k) - r
    def *(r: Rational): Rational = BigInt(k) * r
    def /(r: Rational): Rational = BigInt(k) / r


// Rational.scala v6