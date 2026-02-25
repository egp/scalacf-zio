// BEGIN FILE: Constants.scala
package net.egp.cf.core

/**
 * Constants
 *
 * Well-known continued fractions used as gold references for correctness tests.
 *
 * Notes:
 *  - sqrt2 / phi / e are truly infinite continued fractions with principled generators.
 *  - pi is currently a finite prefix (OEIS A001203). Later we can replace with a spigot.
 */
object Constants:

  ////////////////////////////////////////////////////////////
  // √2
  // √2 = [1; 2, 2, 2, 2, ...]
  ////////////////////////////////////////////////////////////

  def sqrt2: ContinuedFraction =
    ContinuedFraction.fromTerms(
      LazyList(BigInt(1)) #::: LazyList.continually(BigInt(2))
    )

  ////////////////////////////////////////////////////////////
  // φ (golden ratio)
  // φ = [1; 1, 1, 1, 1, ...]
  ////////////////////////////////////////////////////////////

  def phi: ContinuedFraction =
    ContinuedFraction.fromTerms(
      LazyList.continually(BigInt(1))
    )

  ////////////////////////////////////////////////////////////
  // e
  // e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ...]
  // Pattern after the leading 2:
  //   repeating blocks: (1, 2k, 1) for k = 1,2,3,...
  ////////////////////////////////////////////////////////////

  def e: ContinuedFraction =
    ContinuedFraction.fromTerms(
      LazyList(BigInt(2)) #:::
        LazyList
          .from(1)
          .map(k => BigInt(2) * BigInt(k))
          .flatMap { twoK =>
            LazyList(BigInt(1), twoK, BigInt(1))
          }
    )

  ////////////////////////////////////////////////////////////
  // π
  // π = [3; 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, ...]
  // OEIS A001203 (partial quotients for pi).
  //
  // For now: use a finite prefix as a deterministic gold reference.
  ////////////////////////////////////////////////////////////

  // 98-term prefix from OEIS A001203 (as provided)
  val piTermsPrefix98: Vector[BigInt] =
    Vector(
      3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1, 1, 2, 2, 2, 2, 1, 84, 2, 1, 1,
      15, 3, 13, 1, 4, 2, 6, 6, 99, 1, 2, 2, 6, 3, 5, 1, 1, 6, 8, 1, 7, 1, 2, 3, 7,
      1, 2, 1, 1, 12, 1, 1, 1, 3, 1, 1, 8, 1, 1, 2, 1, 6, 1, 1, 5, 2, 2, 3, 1, 2, 4,
      4, 16, 1, 161, 45, 1, 22, 1, 2, 2, 1, 4, 1, 2, 24, 1, 2, 1, 3, 1, 2, 1
    ).map(BigInt(_))

  def piPrefix: ContinuedFraction =
    ContinuedFraction.fromTerms(
      LazyList.from(piTermsPrefix98)
    )

// END FILE: Constants.scala
// EOF: Constants.scala