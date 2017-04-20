package engine.parser

import core.lars.LarsProgram
import jtms.evaluation.instances.{CacheHopsEvalInst1, MMediaDeterministicEvalInst}
import org.scalatest.FlatSpec

import scala.util.Random

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
    val foo = CacheHopsEvalInst1(10000,10,true,Random)
    val bar = foo.larsProgram(300)
    val diff1 = bar.rules diff program.get.rules
    val diff2 = program.get.rules diff bar.rules
    print("bar.rules == program.rules: ")
    println(bar.rules == program.get.rules)
    print("bar.rules diff program.rules: ")
    diff1.foreach(println(_))
    println("####################")
    print("program.rules diff bar.rules: ")
    diff2.foreach(println(_))
    println("--------------------------")
    println(program.get.toString())
    println(bar.toString())
    println(program.get == bar)
    assert(program.isDefined)
  }

  "Program jtms.evaluation.instances.MMedia" should "not fail" in {
    val program: Option[LarsProgram] = LarsParser("/parser-programs/MMedia.lars")
    assert(program.isDefined)
  }
}
