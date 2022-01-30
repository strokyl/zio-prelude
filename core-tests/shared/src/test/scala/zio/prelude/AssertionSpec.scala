package zio.prelude

import zio.test.Assertion._
import zio.test.{DefaultRunnableSpec, Gen, ZSpec, assert, check, checkAll}

object AssertionSpec extends DefaultRunnableSpec {

  private val matchesSpec = suite(".matches")(
    test("must fail when the regex only match a part of the string") {
      val regexpAssertion = Assertion.matches("biking")
      assert(regexpAssertion("toto like biking"))(isLeft(anything))
    },
    test("must fail when a regexp using | only match a part of the string") {
      val regexpAssertion = Assertion.matches("swimming|biking")
      assert(regexpAssertion("toto like biking"))(isLeft(anything))
    },
    test("must work when the regex only match the full string") {
      val regexpAssertion = Assertion.matches("t.*g")
      assert(regexpAssertion("toto like biking"))(isRight(isUnit))
    },
    testM("using Literal should only match a given character") {
      val inputGen = for {
        charForLiteral <- Gen.anyChar
        charGen         = Gen.weighted(Gen.elements(charForLiteral) -> 0.5, Gen.anyChar -> 0.5)
        str            <- Gen.stringBounded(0, 2)(charGen)
      } yield charForLiteral -> str

      check(inputGen) { case (charForLiteral, str) =>
        val literalAssertion = Assertion.matches(Assertion.Regex.Literal(charForLiteral))
        assert(literalAssertion(str).isRight)(equalTo(str === charForLiteral.toString))
      }
    },
    testM("using Literal should properly handle specifics regexp char") {
      val problematicChars = raw".[?(*{\?|^$$"

      val inputGen = for {
        charForLiteral <- Gen.fromIterable(problematicChars)
        otherChar       = Gen.fromIterable(Seq(charForLiteral, 'z'))
      } yield charForLiteral -> otherChar.toString

      checkAll(inputGen) { case (problematicChar, str) =>
        val literalAssertion = Assertion.matches(Assertion.Regex.Literal(problematicChar))
        assert(literalAssertion(str).isRight)(equalTo(str === problematicChar.toString))
      }
    },
    testM("using Anything should match anything") {
      val stringWithNewline = Gen.string(Gen.weighted(Gen.elements('\n') -> 0.3, Gen.anyChar -> 0.7))
      check(stringWithNewline) { str =>
        val anythingAssertion = Assertion.matches(Assertion.Regex.Anything)
        assert(anythingAssertion(str))(isRight(anything))
      }
    }
  )

  def spec: ZSpec[Environment, Failure] = suite("Assertion")(
    matchesSpec
  )

}
