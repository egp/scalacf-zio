// ContinuedFractionSpecZio.scala
package net.egp.cf.core

import zio.test.*

object ContinuedFractionSpecZio extends ZIOSpecDefault:

  private def r(n: Int, d: Int): Rational =
    Rational(BigInt(n), BigInt(d))

  private def assertLegalTerms(cf: ContinuedFraction, maxCheck: Int = 50): TestResult =
    val ts = cf.terms.take(maxCheck).toList
    assertTrue(ts.nonEmpty) &&
    assertTrue(ts.drop(1).forall(_ >= 1))

  def spec: Spec[Any, Nothing] =
    suite("ContinuedFractionSpec")(

      test("Rational → CF → Rational round-trip exact") {
        val values =
          List(
            r(0, 1),
            r(1, 1),
            r(3, 2),
            r(13, 5),
            r(415, 93),
            r(355, 113),
            r(-1, 1),
            r(-3, 2),
            r(-7, 3),
            r(-415, 93)
          )

        val checks =
          values.map { v =>
            val cf = v.toContinuedFraction
            val back = cf.toRational
            assertLegalTerms(cf) && assertTrue(back == v)
          }

        checks.reduce(_ && _)
      },

      test("terms correct for 415/93") {
        val cf =
          Rational(415, 93).toContinuedFraction

        val expected =
          List(BigInt(4), BigInt(2), BigInt(6), BigInt(7))

        assertTrue(cf.terms.take(4).toList == expected) &&
        assertLegalTerms(cf)
      },

      test("convergents exact for 415/93") {
        val cf =
          Rational(415, 93).toContinuedFraction

        val expected =
          List(
            Rational(4, 1),
            Rational(9, 2),
            Rational(58, 13),
            Rational(415, 93)
          )

        assertTrue(cf.convergents.take(4).toList == expected)
      },

      test("single term convergent") {
        val cf =
          Rational(5, 1).toContinuedFraction

        assertTrue(cf.convergents.head == Rational(5, 1))
      },

      test("two-term convergent correct") {
        val cf =
          Rational(7, 3).toContinuedFraction

        assertTrue(cf.convergents.last == Rational(7, 3))
      },

      test("negative rational produces legal continued fraction terms") {
        val values =
          List(
            Rational(-1, 2),
            Rational(-3, 2),
            Rational(-7, 3),
            Rational(-22, 7),
            Rational(-355, 113)
          )

        val checks =
          values.map { v =>
            val cf = v.toContinuedFraction
            assertLegalTerms(cf) &&
            assertTrue(cf.toRational == v)
          }

        checks.reduce(_ && _)
      },

      test("zero produces single-term continued fraction [0]") {
        val cf = Rational(0, 1).toContinuedFraction
        assertTrue(cf.terms.take(5).toList == List(BigInt(0))) &&
        assertTrue(cf.toRational == Rational(0, 1))
      }
    )

// EOF: ContinuedFractionSpecZio.scala