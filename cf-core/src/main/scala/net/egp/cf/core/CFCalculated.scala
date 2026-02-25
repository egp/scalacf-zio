////////////////////////////////////////////////////////////
// BEGIN FILE: CFCalculated.scala
////////////////////////////////////////////////////////////

package net.egp.cf.core

/**
 * CFCalculated
 *
 * True infinite continued-fraction term sources derived from mathematics.
 *
 * These are *generators* of CF terms (never terminate):
 *
 *   √2 = [1; 2, 2, 2, 2, ...]
 *   φ  = [1; 1, 1, 1, 1, ...]
 *   e  = [2; 1,2,1, 1,4,1, 1,6,1, ...]
 * TODO ideas
 *   √ 7 = [2,(1, 1, 1, 4)]
 *   √ 11 = [3,(3, 6)]
 */
object CFCalculated:

  /**
   * √2 = [1; 2,2,2,...]
   */
  object Sqrt2 extends TermSource:
    override val terms: LazyList[BigInt] =
      BigInt(1) #:: LazyList.continually(BigInt(2))

  /**
   * φ = [1; 1,1,1,...]
   */
  object Phi extends TermSource:
    override val terms: LazyList[BigInt] =
      BigInt(1) #:: LazyList.continually(BigInt(1))

  /**
   * e = [2; 1,2,1, 1,4,1, 1,6,1, ...]
   *
   * For n >= 1 (0-indexed with a0 = 2):
   *   a_n = 2k  when n = 3k-1 (i.e. n % 3 == 2)
   *   a_n = 1   otherwise
   */
  object E extends TermSource:

    private def tailFrom(n: Int): LazyList[BigInt] =
      val term: BigInt =
        if (n % 3 == 2) BigInt(2) * BigInt((n + 1) / 3)
        else BigInt(1)

      term #:: tailFrom(n + 1)

    override val terms: LazyList[BigInt] =
      BigInt(2) #:: tailFrom(1)

  ////////////////////////////////////////////////////////////
  // Convenience: ContinuedFraction views
  ////////////////////////////////////////////////////////////

  def sqrt2CF: ContinuedFraction =
    ContinuedFraction.fromTerms(Sqrt2.terms)

  def phiCF: ContinuedFraction =
    ContinuedFraction.fromTerms(Phi.terms)

  def eCF: ContinuedFraction =
    ContinuedFraction.fromTerms(E.terms)

////////////////////////////////////////////////////////////
// END FILE: CFCalculated.scala
////////////////////////////////////////////////////////////