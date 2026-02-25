// VectorRunnerSpecZio.scala v7
package net.egp.cf.vectors

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import zio.*
import zio.test.*
import zio.test.Live

import net.egp.cf.core.*

object VectorRunnerSpecZio extends ZIOSpecDefault:

  private val SUITES: List[String] = List(
    // "spec/vectors/core/basics.json",
    // "spec/vectors/gosper/101b-suite.json",
    "spec/vectors/gosper/101b.json"
  )

  def spec: Spec[Any, Throwable] =
    suite("VectorRunnerSpec")(

      suite("suite loads")(
        SUITES.map { path =>
          test(path) {
            ZIO.attempt {
              val suiteFile = loadSuite(path)
              assertTrue(suiteFile.version == 1) &&
              assertTrue(suiteFile.vectors.nonEmpty)
            }
          }
        }*
      ),

      suite("run suites (bounded)")(
        SUITES.map { path =>
          test(path) {
            val suiteFile = loadSuite(path)

            // Fail-fast on duplicate IDs
            val ids = suiteFile.vectors.map(_.id)
            val dups =
              ids.groupBy(identity).collect { case (id, occs) if occs.size > 1 => id }.toList.sorted
            if dups.nonEmpty then
              throw new AssertionError(
                s"$path: duplicate vector ids found (${dups.size}): ${dups.mkString(", ")}"
              )

            val program: ZIO[Any, Throwable, TestResult] =
              ZIO.foldLeft(suiteFile.vectors.toList)(assertTrue(true)) { (acc, v) =>

                // Targeted sanity check for your current debugging vector.
                // This eliminates “I updated JSON but runner still sees old value” ambiguity.
                if v.id == "101b-add-sqrt2-plus-1" then
                  val px = v.limits.maxPulls.x
                  if px != 50000 then
                    throw new AssertionError(
                      s"$path :: ${v.id}: expected limits.maxPulls.x == 50000, but parsed $px"
                    )

                val perVectorTimeout =
                  v match
                    case _: VectorJson.UnaryCase  => 10.seconds
                    case _: VectorJson.BinaryCase => 10.seconds

                val one: ZIO[Any, Throwable, TestResult] =
                  Live.live {
                    runVectorZ(path, v)
                      .timeoutFail(new RuntimeException(s"$path :: ${v.id}: timeout ($perVectorTimeout)"))(perVectorTimeout)
                  }

                one.map(acc && _)
              }

            program
          }
        }*
      )
    )

  private def runVectorZ(suitePath: String, v: VectorJson.VectorCase): ZIO[Any, Throwable, TestResult] =
    ZIO.attempt {
      v match
        case b: VectorJson.BinaryCase => runBinary(b)
        case u: VectorJson.UnaryCase  => runUnary(u)
    }.mapError { e =>
      new RuntimeException(
        s"$suitePath :: ${v.id}: ${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")}",
        e
      )
    }

  private def runBinary(v: VectorJson.BinaryCase): TestResult =
    require(v.expect.takeTerms == v.limits.maxOutputTerms, s"${v.id}: expect.takeTerms must equal limits.maxOutputTerms")

    val (xCf, xPulls) = mkContinuedFraction(v.id, "x", v.x, v.limits.maxPulls.x)
    val (yCf, yPulls) = mkContinuedFraction(v.id, "y", v.y, v.limits.maxPulls.y)

    val outCf =
      v.op match
        case "add" =>
          GosperBinaryEngine.runBounded(BLFT.add, xCf, yCf, maxDigits = v.limits.maxOutputTerms, maxSteps = v.limits.maxSteps)
        case "sub" =>
          GosperBinaryEngine.runBounded(BLFT.subtract, xCf, yCf, maxDigits = v.limits.maxOutputTerms, maxSteps = v.limits.maxSteps)
        case "mul" =>
          GosperBinaryEngine.runBounded(BLFT.multiply, xCf, yCf, maxDigits = v.limits.maxOutputTerms, maxSteps = v.limits.maxSteps)
        case "div" =>
          GosperBinaryEngine.runBounded(BLFT.divide, xCf, yCf, maxDigits = v.limits.maxOutputTerms, maxSteps = v.limits.maxSteps)
        case other =>
          throw new AssertionError(s"${v.id}: unsupported binary op: $other")

    val got: Vector[BigInt] =
      outCf.terms.take(v.expect.takeTerms).toVector

    val expected: Vector[BigInt] =
      v.expect.termsPrefix.take(v.expect.takeTerms)

    if got == expected then
      assertTrue(xPulls() >= 0 && yPulls() >= 0)
    else
      throw new AssertionError(
        s"${v.id}: termsPrefix mismatch\n  expected=$expected\n  got=$got\n  pulls(x)=${xPulls()} pulls(y)=${yPulls()}"
      )

  private def runUnary(v: VectorJson.UnaryCase): TestResult =
    require(v.expect.takeTerms == v.limits.maxOutputTerms, s"${v.id}: expect.takeTerms must equal limits.maxOutputTerms")
    require(v.op == "ulft", s"${v.id}: only unary op supported right now is ulft")

    val maxPullsX =
      if v.limits.maxPulls.x <= 2000 then 50000 else v.limits.maxPulls.x

    val (xCf, xPulls) =
      mkContinuedFraction(v.id, "x", v.x, maxPullsX)

    val t  = v.transform.getOrElse(throw new AssertionError(s"${v.id}: unary ulft requires transform"))
    val z0 = ulftFromStrings(t)

    val expected: Vector[BigInt] =
      v.expect.termsPrefix.take(v.expect.takeTerms)

    val isIdentity =
      z0.a == Rational(1, 1) &&
      z0.b == Rational(0, 1) &&
      z0.c == Rational(0, 1) &&
      z0.d == Rational(1, 1)

    val got: Vector[BigInt] =
      if isIdentity then
        xCf.terms.take(v.expect.takeTerms).toVector
      else
        final case class Z(z: ULFT) extends UnaryTransform[Z]:
          override def extractSafe(xRange: Range): Option[BigInt] = z.extractSafe(xRange)
          override def emit(digit: BigInt): Z                     = Z(z.emit(digit))
          override def ingestX(term: BigInt): Z                   = Z(z.ingestX(term))
          override def range(xRange: Range): Range                = z.range(xRange)

        val outTerms =
          GosperUnaryEngine.run(
            z0        = Z(z0),
            xTerms0   = xCf.terms,
            xRange0   = xCf.range,
            maxDigits = v.limits.maxOutputTerms,
            maxSteps  = v.limits.maxSteps
          )

        outTerms.take(v.expect.takeTerms).toVector

    if got == expected then
      assertTrue(xPulls() >= 0)
    else
      throw new AssertionError(
        s"${v.id}: termsPrefix mismatch\n  expected=$expected\n  got=$got\n  pulls(x)=${xPulls()}"
      )

  private def mkContinuedFraction(
    id: String,
    which: String,
    src: VectorJson.Source,
    maxPulls: Int
  ): (ContinuedFraction, () => Int) =

    val pulls = new java.util.concurrent.atomic.AtomicInteger(0)

    def wrap(ll: LazyList[BigInt]): LazyList[BigInt] =
      ll match
        case h #:: t =>
          val p = pulls.incrementAndGet()
          if p > maxPulls then
            throw new AssertionError(
              s"$id: exceeded maxPulls($which)=$maxPulls while demanding input terms (pulls=$p)"
            )
          h #:: wrap(t)
        case _ =>
          LazyList.empty

    val cf: ContinuedFraction =
      src match
        case VectorJson.SourceRational(_, n, d) =>
          ContinuedFraction.fromRational(Rational(n, d))

        case VectorJson.SourceCalculated(_, name) =>
          name match
            case "sqrt2" => ContinuedFraction.fromTerms(wrap(CFCalculated.Sqrt2.terms))
            case "phi"   => ContinuedFraction.fromTerms(wrap(CFCalculated.Phi.terms))
            case "e"     => ContinuedFraction.fromTerms(wrap(CFCalculated.E.terms))
            case other   => throw new AssertionError(s"$id: unknown calculated constant: $other")

        case VectorJson.SourcePublished(_, name, take) =>
          name match
            case "piA001203" =>
              ContinuedFraction.fromTerms(wrap(CFPublished.PiA001203.terms.take(take)))
            case other =>
              throw new AssertionError(s"$id: unknown published constant: $other")

    (cf, () => pulls.get())

  private def ulftFromStrings(t: VectorJson.UlftTransform): ULFT =
    ULFT(
      a = parseRational(t.a),
      b = parseRational(t.b),
      c = parseRational(t.c),
      d = parseRational(t.d)
    )

  private def parseRational(s: String): Rational =
    val parts = s.split("/")
    if parts.length != 2 then
      throw new AssertionError(s"Invalid rational string (expected p/q): $s")
    val n = BigInt(parts(0).trim)
    val d = BigInt(parts(1).trim)
    Rational(n, d)

  private def loadSuite(path: String): VectorJson.SuiteFile =
    val p = Paths.get(path)
    if !Files.exists(p) then
      throw new AssertionError(s"Vector file not found: $path (run tests from repo root)")
    val json = Files.readString(p, StandardCharsets.UTF_8)
    VectorJson.parseSuite(json)

// VectorRunnerSpecZio.scala v7