package engine.parser

import engine.parser.utils.ParserRunner
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * Created by et on 16.03.17.
  */
class LarsLexerTest extends FlatSpec {



  private val filename = "/parser-programs/DummyProgram.lars"
  private val source = Source.fromURL(getClass.getResource(filename))
  private val lines = try source.mkString finally source.close()
  private val parser = new ParserRunner

//  "The parser" should "not throw any exceptions" in {
//    assert(parser.parseProgram(lines).successful)
//  }

  "The parser" should "recognize the import statement" in {
    assert(parser.parseImport("import engine.parser.factory.slidingWindowFunctionFactory.SlidingTimeWindowFactory as bar\n").successful)
  }

  "The parser" should "recognize the import statement with an argument" in {
//    assert(parser.parseImport("engine.parser.factory.slidingWindowFunctionFactory.SlidingTimeWindowFactory as bar\n").successful)
    assert(parser.parseImport("import engine.parser.factory.slidingWindowFunctionFactory.SlidingTimeWindowFactory( 20 ) as foo\n").successful)
  }

  "Atoms" should "be in lower case letters and may have parameters" in {
    assert(parser.parseAtom("a").successful &&
              !parser.parseAtom("A").successful &&
              parser.parseAtom("a(A)").successful)
  }

  "@-atoms" should "either have a variable (in capital letter) or a number after the at" in {
    assert(parser.parseHead("a at T").successful && parser.parseHead("a at 34").successful)
  }

  "The parser" should "recognize simple positive rules with regular atoms" in {
    assert(parser.parseRule("a :- b.").successful)
  }

  "Rules with negation" should "be recognized" in {
    assert(parser.parseRule("a:- not b.").successful)
  }

//  an [InvalidSyntaxException] should be thrownBy parser.parseRule("not a:- b.")

"Rules with negation in the head" should "not be recognized" in {
  intercept[InvalidSyntaxException] {
    parser.parseRule("not a:- b.").get.ruleHead
  }
//  assert(!parser.parseRule("not a:- b.").successful)
  }

  "Rules with @-atoms in the head" should " be recognized" in {
    assert(parser.parseRule("a at T:- b.").successful)
  }

  "Rules" should " should recognize optional spaces" in {
    assert(parser.parseRule("a:-b.").successful)
  }

  "Rules" should "end with a dot" in {
    assert(!parser.parseRule("a :- b").successful)
  }

  "The parser" should "at least be able to recognize strings" in {
    assert(parser.parseStr("thisisastring").successful)
  }

  "A string with a whitespace" should "should not be recognized" in {
    assert(!parser.parseStr("thisisnotastr ing").successful)
  }

  "Operations" should "contain uppercase letters (variables) and/or numbers" in {
    assert(parser.parseOperation("T=3+4").successful)
  }

  "Operations" should "also parse numbers only, in a left sided operation" in {
        assert(parser.parseOperation("3+4=7").successful)
  }

  "Operations" should "also parse numbers only, in a right sided operation" in {
    assert(parser.parseOperation("7=3+4").successful)
  }

  "Rules" should "also parse numbers only, in a left sided operation" in {
    assert(parser.parseRule("a :- 3+5=8.").successful)
  }

  "A rule body" should "recognize atoms, @-atoms and window atoms" in {
    assert(parser.parseBody("a, b at T, c in [t 5], T=3+4").successful)
  }

  "A rule body" should "recognize atoms, @-atoms, window atoms (all possibly with a preceding not) " +
    "and operations" in {
    assert(parser.parseBody("not a,b at T,c in [t 5],d always            in [t 3,4,5], T=3+4").successful)
  }
}
