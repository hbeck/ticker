package engine.parser

import org.scalatest.FlatSpec

/**
  * Created by et on 16.03.17.
  */
class SimpleParserTest extends FlatSpec {
  private val source = scala.io.Source.fromFile("/home/nechtan/Dropbox/Informatik/Projekt/steen/src/test/scala/engine/parser/InputFiles/SimpleProgram")
  private val lines = try source.mkString finally source.close()
  private val parser = new ParserRunner

//  "The parser" should "not throw any exceptions" in {
//    assert(parser.parseProgram(lines).successful)
//  }

  "The parser" should "recognize the import statement" in {
    assert(parser.parseImport("import foo as bar").successful)
//    println(parser.parseImport("import foo as bar"))
  }

  "The parser" should "recognize the import statement with an argument" in {
    assert(parser.parseImport("import foo(arg) as bar").successful)
  }

  "The parser" should "recognize simple positive rules with regular atoms" in {
    assert(parser.parseRule("a :- b.").successful)
//    println(parser.parseRule("a :- b."))
  }

  "Rules with negation" should "be recognized" in {
    assert(parser.parseRule("a:- not b.").successful)
  }

  "Rules with negation in the head" should "not be recognized" in {
    assert(!parser.parseRule("not a:- b.").successful)
  }

  "Rules with @-atoms in the head" should " be recognized" in {
    println(parser.parseRule("a at T:- b."))
    assert(parser.parseRule("a at T:- b.").successful)
  }

  "The parser" should "at least be able to recognize strings" in {
    assert(parser.parseStr("thisisastring").successful)
  }

  "A string with a whitespace" should "should no be recognized" in {
    assert(!parser.parseStr("thisisastr ing").successful)
  }

}
