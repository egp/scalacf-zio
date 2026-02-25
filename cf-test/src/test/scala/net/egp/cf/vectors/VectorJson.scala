// BEGIN FILE: src/test/scala/net/egp/cf/vectors/VectorJson.scala
package net.egp.cf.vectors

import ujson.*

/**
 * VectorJson
 *
 * Minimal, future-proof JSON model + parser for golden vectors.
 *
 * Compatibility:
 * - Accepts discriminator keys: "kind" (preferred), or legacy "type", or legacy "$type".
 * - Avoids uPickle tagged ADTs; parses manually with ujson.
 */
object VectorJson:

  ////////////////////////////////////////////////////////////
  // Model
  ////////////////////////////////////////////////////////////

  final case class SuiteFile(
    version: Int,
    vectors: Vector[VectorCase]
  )

  sealed trait VectorCase:
    def id: String
    def limits: Limits
    def expect: Expect

  final case class BinaryCase(
    id: String,
    op: String,                 // "add"|"sub"|"mul"|"div"
    x: Source,
    y: Source,
    limits: Limits,
    expect: Expect
  ) extends VectorCase

  final case class UnaryCase(
    id: String,
    op: String,                 // currently: "ulft"
    x: Source,
    transform: Option[UlftTransform],
    limits: Limits,
    expect: Expect
  ) extends VectorCase

  // Sources
  sealed trait Source:
    def kind: String

  final case class SourceRational(kind: String, n: BigInt, d: BigInt) extends Source
  final case class SourceCalculated(kind: String, name: String) extends Source
  final case class SourcePublished(kind: String, name: String, take: Int) extends Source

  // Unary ULFT coefficients as "p/q" strings
  final case class UlftTransform(a: String, b: String, c: String, d: String)

  // Limits
  final case class MaxPulls(x: Int, y: Int)
  final case class Limits(
    maxOutputTerms: Int,
    maxSteps: Int,
    maxPulls: MaxPulls
  )

  // Expectation
  final case class Expect(
    takeTerms: Int,
    termsPrefix: Vector[BigInt]
  )

  ////////////////////////////////////////////////////////////
  // Public parse API
  ////////////////////////////////////////////////////////////

  def parseSuite(json: String): SuiteFile =
    val v = ujson.read(json)
    val obj = asObj(v, "root")
    val version = getInt(obj, "version", "root")

    val vectorsArr = getArr(obj, "vectors", ctx = "root")
    val vectors: Vector[VectorCase] =
      vectorsArr.value.toVector.zipWithIndex.map { (vv, idx) =>
        parseVectorCase(vv, ctx = s"root.vectors[$idx]")
      }

    SuiteFile(version = version, vectors = vectors)

  ////////////////////////////////////////////////////////////
  // Parsing internals
  ////////////////////////////////////////////////////////////

  private def parseVectorCase(v: Value, ctx: String): VectorCase =
    val obj = asObj(v, ctx)

    // Accept "kind" (preferred) or legacy keys.
    val kind = getDiscriminator(obj, ctx)

    kind match
      case "binary" =>
        val id = getStr(obj, "id", ctx)
        val op = getStr(obj, "op", ctx)
        val x = parseSource(getObj(obj, "x", ctx), ctx = s"$ctx.x")
        val y = parseSource(getObj(obj, "y", ctx), ctx = s"$ctx.y")
        val limits = parseLimits(getObj(obj, "limits", ctx), ctx = s"$ctx.limits")
        val expect = parseExpect(getObj(obj, "expect", ctx), ctx = s"$ctx.expect")
        BinaryCase(id, op, x, y, limits, expect)

      case "unary" =>
        val id = getStr(obj, "id", ctx)
        val op = getStr(obj, "op", ctx)
        val x = parseSource(getObj(obj, "x", ctx), ctx = s"$ctx.x")

        val transformOpt: Option[UlftTransform] =
          obj.value.get("transform") match
            case None => None
            case Some(t) =>
              Some(parseUlftTransform(asObj(t, s"$ctx.transform"), s"$ctx.transform"))

        val limits = parseLimits(getObj(obj, "limits", ctx), ctx = s"$ctx.limits")
        val expect = parseExpect(getObj(obj, "expect", ctx), ctx = s"$ctx.expect")
        UnaryCase(id, op, x, transformOpt, limits, expect)

      case other =>
        throw new IllegalArgumentException(s"$ctx: unknown kind/type='$other' (expected 'binary'|'unary')")

  private def parseSource(obj: Obj, ctx: String): Source =

    // Accept "kind" (preferred), or legacy "type"/"$type", or infer from keys.
    val kindOpt: Option[String] = 
      obj.value.get("kind") match
        case Some(Str(s)) => Some(s)
        case Some(other)  => throw new IllegalArgumentException(s"$ctx: key 'kind' expected string, got $other")
        case None =>
          obj.value.get("type") match
            case Some(Str(s)) => Some(s)
            case Some(other)  => throw new IllegalArgumentException(s"$ctx: key 'type' expected string, got $other")
            case None =>
              obj.value.get("$type") match
                case Some(Str(s)) => Some(s)
                case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$$type' expected string, got $other")
                case None         => None

    val kind: String =
      kindOpt.getOrElse {
        // Inference fallback for older/compact vector files:
        if obj.value.contains("n") && obj.value.contains("d") then "rational"
        else if obj.value.contains("name") && obj.value.contains("take") then "published"
        else if obj.value.contains("name") then "calculated"
        else
          throw new IllegalArgumentException(
            s"$ctx: missing discriminator key: expected 'kind' or legacy 'type'/'$$type' (and could not infer from keys)"
          )
      }

    kind match
      case "rational" =>
        val n = getBigInt(obj, "n", ctx)
        val d = getBigInt(obj, "d", ctx)
        SourceRational(kind, n, d)

      case "calculated" =>
        val name = getStr(obj, "name", ctx)
        SourceCalculated(kind, name)

      case "published" =>
        val name = getStr(obj, "name", ctx)
        val take = getInt(obj, "take", ctx)
        SourcePublished(kind, name, take)

      case other =>
        throw new IllegalArgumentException(
          s"$ctx: unknown source kind/type='$other' (expected 'rational'|'calculated'|'published')"
        )


  private def parseUlftTransform(obj: Obj, ctx: String): UlftTransform =
    UlftTransform(
      a = getStr(obj, "a", ctx),
      b = getStr(obj, "b", ctx),
      c = getStr(obj, "c", ctx),
      d = getStr(obj, "d", ctx)
    )

  private def parseLimits(obj: Obj, ctx: String): Limits =
    val maxOutputTerms = getInt(obj, "maxOutputTerms", ctx)
    val maxSteps       = getInt(obj, "maxSteps", ctx)

    val pullsObj = getObj(obj, "maxPulls", ctx)
    val px = getInt(pullsObj, "x", s"$ctx.maxPulls")
    val py = getIntOpt(pullsObj, "y", s"$ctx.maxPulls").getOrElse(0)

    Limits(
      maxOutputTerms = maxOutputTerms,
      maxSteps       = maxSteps,
      maxPulls       = MaxPulls(px, py)
    )

  private def parseExpect(obj: Obj, ctx: String): Expect =
    val takeTerms = getInt(obj, "takeTerms", ctx)
    val termsArr = getArr(obj, "termsPrefix", ctx)

    val termsPrefix: Vector[BigInt] =
      termsArr.value.toVector.zipWithIndex.map { (vv, idx) =>
        vv match
          case Num(n) => BigInt(n.toLong) // if you ever need >Long, store as string
          case Str(s) => BigInt(s.trim)
          case other  => throw new IllegalArgumentException(s"$ctx.termsPrefix[$idx]: expected number or string, got $other")
      }

    Expect(takeTerms = takeTerms, termsPrefix = termsPrefix)

  ////////////////////////////////////////////////////////////
  // discriminator helper
  ////////////////////////////////////////////////////////////

  /**
   * Returns the discriminator value from "kind" (preferred) or legacy "type"/"$type".
   * Throws if none present.
   */
  private def getDiscriminator(obj: Obj, ctx: String): String =
    obj.value.get("kind") match
      case Some(Str(s)) => s
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key 'kind' expected string, got $other")
      case None =>
        obj.value.get("type") match
          case Some(Str(s)) => s
          case Some(other)  => throw new IllegalArgumentException(s"$ctx: key 'type' expected string, got $other")
          case None =>
            obj.value.get("$type") match
              case Some(Str(s)) => s
              case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$$type' expected string, got $other")
              case None         => throw new IllegalArgumentException(s"$ctx: missing discriminator key: expected 'kind' or legacy 'type'/'$$type'")

  ////////////////////////////////////////////////////////////
  // ujson helpers
  ////////////////////////////////////////////////////////////

  private def getIntOpt(parent: Obj, key: String, ctx: String): Option[Int] =
    parent.value.get(key) match
      case None         => None
      case Some(Num(n)) => Some(n.toInt)
      case Some(Str(s)) => Some(s.trim.toInt)
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected int, got $other")

  private def asObj(v: Value, ctx: String): Obj =
    v match
      case o: Obj => o
      case other  => throw new IllegalArgumentException(s"$ctx: expected JSON object, got $other")

  private def getObj(parent: Obj, key: String, ctx: String): Obj =
    parent.value.get(key) match
      case Some(o: Obj) => o
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected object, got $other")
      case None         => throw new IllegalArgumentException(s"$ctx: missing required key '$key'")

  private def getArr(parent: Obj, key: String, ctx: String): Arr =
    parent.value.get(key) match
      case Some(a: Arr) => a
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected array, got $other")
      case None         => throw new IllegalArgumentException(s"$ctx: missing required key '$key'")

  private def getStr(parent: Obj, key: String, ctx: String): String =
    parent.value.get(key) match
      case Some(Str(s)) => s
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected string, got $other")
      case None         => throw new IllegalArgumentException(s"$ctx: missing required key '$key'")

  private def getInt(parent: Obj, key: String, ctx: String): Int =
    parent.value.get(key) match
      case Some(Num(n)) => n.toInt
      case Some(Str(s)) => s.trim.toInt
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected int, got $other")
      case None         => throw new IllegalArgumentException(s"$ctx: missing required key '$key'")

  private def getBigInt(parent: Obj, key: String, ctx: String): BigInt =
    parent.value.get(key) match
      case Some(Num(n)) => BigInt(n.toLong)
      case Some(Str(s)) => BigInt(s.trim)
      case Some(other)  => throw new IllegalArgumentException(s"$ctx: key '$key' expected big-int (num|string), got $other")
      case None         => throw new IllegalArgumentException(s"$ctx: missing required key '$key'")

// END FILE: rc/test/scala/net/egp/cf/vectors/VectorJson.scala