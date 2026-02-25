// BEGIN FILE: UnaryDecision.scala
package net.egp.cf.core

/**
 * UnaryDecision
 *
 * Unary analogue of GosperDecision, for transforms over a single operand.
 *
 * Design intent (Gosper-smile):
 *   1) If extractSafe is defined, we MUST emit that digit (correctness invariant).
 *   2) Otherwise, ingest X if a term is available.
 *   3) If we cannot ingest, terminate.
 */
object UnaryDecision:

  ////////////////////////////////////////////////////////////
  // Action ADT
  ////////////////////////////////////////////////////////////

  sealed trait Action

  /** Emit a digit that is proven safe (i.e. extractSafe returned it). */
  final case class Emit(digit: BigInt) extends Action

  /** Ingest the next term from X. */
  case object IngestX extends Action

  /** No safe digit and no available ingestion term. */
  case object Terminate extends Action

  ////////////////////////////////////////////////////////////
  // Decision function
  ////////////////////////////////////////////////////////////

  def nextAction[Z <: UnaryTransform[Z]](
    z: Z,
    xTerm: Option[BigInt],
    xRange: Range
  ): Action =
    require(z != null, "z must not be null")
    require(xRange != null, "xRange must not be null")

    z.extractSafe(xRange) match
      case Some(d) =>
        Emit(d)

      case None =>
        xTerm match
          case Some(_) => IngestX
          case None    => Terminate

// END FILE: UnaryDecision.scala