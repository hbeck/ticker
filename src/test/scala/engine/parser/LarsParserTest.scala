package engine.parser

import core.lars.LarsProgram
import org.scalatest.FlatSpec

/**
  * Created by et on 11.04.17.
  */
class LarsParserTest extends FlatSpec {

  "The simple one-rule program" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- b.",false)
    assert(program.isDefined)
  }

  "The one-rule program with two body atoms" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- b,c.",false)
    assert(program.isDefined)
  }

  "Left-hand operations with a variable" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- 3+5=T.",false)
    assert(program.isDefined)
  }

  "Left-hand operations" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- 3+5=8.",false)
    assert(program.isDefined)
  }

  "Right-hand operations" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- 8=3+5.",false)
    assert(program.isDefined)
  }

  "Two sided operations" should "not be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- 10-2=3+5.",false)
    assert(program.isEmpty)
  }

  "Left-hand operations with variables" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- 3+5=A.",false)
    assert(program.isDefined)
  }

  "Right-hand operations with variables" should "be accepted" in {
    val program: Option[LarsProgram] = LarsParser("a :- A=3+5.",false)
    assert(program.isDefined)
  }

  "A program with imports, rules, and comments" should "be accepted and parsed" in {
    val program: Option[LarsProgram] = LarsParser("/parser-programs/TestProgram1.lars")
    assert(program.isDefined)
  }

  "Program jtms.evaluation.instances.CacheHopsEvalInst" should "not fail" in {
    val program: Option[LarsProgram] = LarsParser("/parser-programs/cacheHopsEvalInst.lars")
    assert(program.isDefined)
  }

  "Program jtms.evaluation.instances.MMedia" should "not fail" in {
    val program: Option[LarsProgram] = LarsParser("/parser-programs/MMedia.lars")
    assert(program.isDefined)
  }
}
