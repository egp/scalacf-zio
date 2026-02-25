// BEGIN FILE: UnaryTransform.scala
package net.egp.cf.core

/**
 * UnaryTransform
 *
 * Nominal (non-structural) interface for unary Gosper-style transforms.
 * This avoids Scala 3 reflective selection requirements (Selectable/Dynamic).
 *
 * F-bounded so emit/ingestX return the same concrete transform type.
 */
trait UnaryTransform[Self <: UnaryTransform[Self]]:
  def extractSafe(xRange: Range): Option[BigInt]
  def emit(digit: BigInt): Self
  def ingestX(term: BigInt): Self
  def range(xRange: Range): Range

// END FILE: UnaryTransform.scala