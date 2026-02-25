// SmokeSpec.scala
package net.egp.cf

import zio.test.*

object SmokeSpec extends ZIOSpecDefault:
  def spec = suite("smoke")(test("it runs")(assertTrue(1 + 1 == 2)))

// EOF SmokeSpec.scala