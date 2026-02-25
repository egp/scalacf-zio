////////////////////////////////////////////////////////////
// BEGIN FILE: CFPublished.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

/**
 * CFPublished
 *
 * Published reference sequences used for regression tests.
 * These are typically finite prefixes copied from OEIS / literature.
 */
object CFPublished:

  /**
   * π continued fraction terms (prefix) as published by OEIS A001203.
   * https://oeis.org/A001203
   *
   * NOTE: This is intentionally a *finite* prefix for verification.
   * Later we can replace/add a true generator (if/when we implement one).
   */
  object PiA001203:

    // Put your prefix here as BigInt values.
    // Example:
    // val terms: LazyList[BigInt] =
    //   LazyList(3, 7, 15, 1, 292, ...).map(BigInt(_))

    val terms: LazyList[BigInt] =
      LazyList(3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1, 1, 2, 2, 2, 2, 1, 84, 2, 1, 1, 15, 3, 
        13, 1, 4, 2, 6, 6, 99, 1, 2, 2, 6, 3, 5, 1, 1, 6, 8, 1, 7, 1, 2, 3, 7, 1, 2, 1, 1, 12, 1, 1, 1, 
        3, 1, 1, 8, 1, 1, 2, 1, 6, 1, 1, 5, 2, 2, 3, 1, 2, 4, 4, 16, 1, 161, 45, 1, 22, 1, 2, 2, 1, 4, 
        1, 2, 24, 1, 2, 1, 3, 1, 2, 1).map(BigInt(_))


    def cf: ContinuedFraction =
      ContinuedFraction.fromTerms(terms)

////////////////////////////////////////////////////////////
// END FILE: CFPublished.scala
////////////////////////////////////////////////////////////