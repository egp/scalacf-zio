// BLFT.scala
package net.egp.cf.core

/**
 * BLFT
 *
 * Bilinear Linear Fractional Transform:
 *
 *   z(x,y) =
 *
 *      axy + bx + cy + d
 *      ------------------
 *      exy + fx + gy + h
 *
 * All coefficients are exact integers.
 *
 * This implementation is invariant-checked and safe
 * for conservative infinite interval arithmetic via Rational.
 */
final case class BLFT(
  a: BigInt,
  b: BigInt,
  c: BigInt,
  d: BigInt,
  e: BigInt,
  f: BigInt,
  g: BigInt,
  h: BigInt
):

  ////////////////////////////////////////////////////////////
  // eval
  ////////////////////////////////////////////////////////////

  /**
   * Evaluate BLFT at exact Rational inputs.
   *
   * Preconditions:
   *   x != null
   *   y != null
   *
   * Postconditions:
   *   result != null
   */
  def eval(
    x: Rational,
    y: Rational
  ): Rational =

    require(x != null, "x cannot be null")
    require(y != null, "y cannot be null")

    val num =
      Rational(a,1)*x*y +
      Rational(b,1)*x +
      Rational(c,1)*y +
      Rational(d,1)

    val den =
      Rational(e,1)*x*y +
      Rational(f,1)*x +
      Rational(g,1)*y +
      Rational(h,1)

    val result =
      num / den

    require(result != null, "BLFT.eval returned null")
    result


  ////////////////////////////////////////////////////////////
  // range propagation
  ////////////////////////////////////////////////////////////

  /**
   * Propagate Range through transform.
   *
   * Uses corner evaluation. If any corner evaluation is undefined
   * under extended-real arithmetic (0*∞, ∞-∞, 0/0, etc.), we widen
   * conservatively to [-∞, +∞].
   *
   * Preconditions:
   *   xr != null
   *   yr != null
   *
   * Postconditions:
   *   result != null
   */
  def range(
    xr: Range,
    yr: Range
  ): Range =

    require(xr != null, "xr cannot be null")
    require(yr != null, "yr cannot be null")

    def safeEval(x: Rational, y: Rational): Option[Rational] =
      try
        Some(eval(x, y))
      catch
        case _: ArithmeticException =>
          None
        case _: IllegalArgumentException =>
          None

    val corners =
      List(
        (xr.min, yr.min),
        (xr.min, yr.max),
        (xr.max, yr.min),
        (xr.max, yr.max)
      )

    val valuesOpt =
      corners.map { case (x, y) => safeEval(x, y) }

    val result =
      if valuesOpt.exists(_.isEmpty) then
        // Conservative widening: guarantee safety
        Range.inside(NegInf, PosInf)
      else
        val values = valuesOpt.flatten
        Range(values.min, values.max)

    require(result != null, "BLFT.range returned null")
    result


  ////////////////////////////////////////////////////////////
  // extractSafe
  ////////////////////////////////////////////////////////////

  /**
   * Extract digit if guaranteed safe.
   *
   * Rule: extract iff output range has finite endpoints and
   * floor(min) == floor(max).
   *
   * Preconditions:
   *   xr != null
   *   yr != null
   *
   * Postconditions:
   *   result is Some(d) ⇒ d == floor(z(range)) everywhere
   */
  def extractSafe(
    xr: Range,
    yr: Range
  ): Option[BigInt] =

    require(xr != null, "xr cannot be null")
    require(yr != null, "yr cannot be null")

    val r =
      range(xr, yr)

    if r.min.isInfinite || r.max.isInfinite then
      None
    else
      val f1 = r.min.floor
      val f2 = r.max.floor
      if f1 == f2 then Some(f1) else None


  ////////////////////////////////////////////////////////////
  // emit
  ////////////////////////////////////////////////////////////

  /**
   * Emit digit q.
   *
   * Postconditions:
   *   result != null
   */
  def emit(
    q: BigInt
  ): BLFT =

    val result =
      BLFT(
        e,
        f,
        g,
        h,
        a - q*e,
        b - q*f,
        c - q*g,
        d - q*h
      )

    require(result != null, "emit produced null")
    result


  ////////////////////////////////////////////////////////////
  // ingestX
  ////////////////////////////////////////////////////////////

  /**
   * Ingest term t from X stream.
   *
   * Postconditions:
   *   result != null
   */
  def ingestX(
    t: BigInt
  ): BLFT =

    val result =
      BLFT(
        a*t + c,
        b*t + d,
        a,
        b,
        e*t + g,
        f*t + h,
        e,
        f
      )

    require(result != null, "ingestX produced null")
    result


  ////////////////////////////////////////////////////////////
  // ingestY
  ////////////////////////////////////////////////////////////

  /**
   * Ingest term t from Y stream.
   *
   * Postconditions:
   *   result != null
   */
  def ingestY(
    t: BigInt
  ): BLFT =

    val result =
      BLFT(
        a*t + b,
        a,
        c*t + d,
        c,
        e*t + f,
        e,
        g*t + h,
        g
      )

    require(result != null, "ingestY produced null")
    result


  ////////////////////////////////////////////////////////////
  // constant detection
  ////////////////////////////////////////////////////////////

  /**
   * True iff transform is constant (independent of x,y):
   *
   *   (d / h), requiring h != 0.
   */
  def isConstant: Boolean =
    a == 0 &&
    b == 0 &&
    c == 0 &&
    e == 0 &&
    f == 0 &&
    g == 0 &&
    h != 0

  def constantValue: Rational =
    require(isConstant, "constantValue requires isConstant")
    val result = Rational(d, h)
    require(result != null)
    result


////////////////////////////////////////////////////////////
// companion object
////////////////////////////////////////////////////////////

object BLFT:

  ////////////////////////////////////////////////////////////
  // identity transforms
  ////////////////////////////////////////////////////////////

  val identityX =
    BLFT(0,1,0,0, 0,0,0,1)

  val identityY =
    BLFT(0,0,1,0, 0,0,0,1)


  ////////////////////////////////////////////////////////////
  // arithmetic transforms
  ////////////////////////////////////////////////////////////

  val add =
    BLFT(0,1,1,0, 0,0,0,1)

  val subtract =
    BLFT(0,1,-1,0, 0,0,0,1)

  val multiply =
    BLFT(1,0,0,0, 0,0,0,1)

  val divide =
    BLFT(0,1,0,0, 0,0,1,0)


// EOF: BLFT.scala