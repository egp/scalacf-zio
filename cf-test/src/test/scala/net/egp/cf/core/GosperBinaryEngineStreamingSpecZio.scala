// GosperBinaryEngineStreamingSpecZio.scala
package net.egp.cf.core

import zio.test.*

object GosperBinaryEngineStreamingSpecZio extends ZIOSpecDefault:

  def spec: Spec[Any, Nothing] =
    suite("GosperBinaryEngineStreamingSpec")(
      test("streaming arithmetic matches exact evaluation") {

        val x =
          ContinuedFraction.fromTerms(
            LazyList(3, 7, 15, 1, 292).map(BigInt(_))
          )

        val y =
          ContinuedFraction.fromTerms(
            LazyList(1, 2, 2, 2, 2, 2).map(BigInt(_))
          )

        val ops =
          List(
            BLFT.add,
            BLFT.subtract,
            BLFT.multiply,
            BLFT.divide
          )

        val checks =
          ops.map { op =>
            val exact =
              op.eval(
                x.toRational,
                y.toRational
              ).toContinuedFraction

            val streaming =
              GosperBinaryEngine.run(op, x, y)

            val exactTerms =
              exact.terms.take(10).toList

            val streamTerms =
              streaming.terms.take(10).toList

            assertTrue(exactTerms == streamTerms)
          }

        checks.reduce(_ && _)
      }
    )

// EOF: GosperBinaryEngineStreamingSpecZio.scala