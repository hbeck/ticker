package engine.parser

import core.{Argument, Atom, Plus}
import core.lars.{HeadAtom, LarsProgram, LarsRule}
import iclp.evaluation.instances.{CacheHopsEvalInst1, MMediaDeterministicEvalInst}
import org.scalatest.FlatSpec

import scala.util.Random

/**
  * Created by et on 11.04.17.
  */
class LarsParserTest extends FlatSpec {

  private val a = Atom("a")
  private val b = Atom("b")
  private val c = Atom("c")

  "The simple one-rule program" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- b.",false)
    assert(result == LarsProgram(Seq(LarsRule(a,Set(b),Set()))))
    
  }

  "The one-rule program with two body atoms" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- b,c.",false)
    assert(result == LarsProgram(Seq(LarsRule(a,Set(b,c),Set()))))

  }

  "Left-hand operations with a variable" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- 3+5=T.",false)
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","T")),Set())))
    assert(result == check)
  }

  "Left-hand operations" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- 3+5=8.",false)
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","8")),Set())))
    assert(result == check)
  }

  "Right-hand operations" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- 8=3+5.",false)
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","8")),Set())))
    assert(result == check)
  }

  "Two sided operations" should "not be accepted" in {
    intercept[InvalidSyntaxException] {
      LarsParser("a :- 10-2=3+5.", false)
    }
  }

  "Left-hand operations with variables" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- 3+5=A.",false)
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","A")),Set())))
    assert(result == check)
  }

  "Right-hand operations with variables" should "be accepted" in {
    val result: LarsProgram = LarsParser("a :- A=3+5.",false)
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","A")),Set())))
    assert(result == check)
  }

/*  "A program with imports, rules, and comments" should "be accepted and parsed" in {
    val program: LarsProgram = LarsParser("/parser-programs/TestProgram1.lars")
  }*/

  "Program jtms.evaluation.instances.CacheHopsEvalInst" should "not fail" in {
    val program: LarsProgram = LarsParser("/parser-programs/cacheHopsEvalInst.lars")
    val cacheHops = CacheHopsEvalInst1(10000,10,0,Random)
    val cacheHopsProgram = cacheHops.larsProgram(300)
    assert(cacheHopsProgram.toString == program.toString)
  }

  "Program jtms.evaluation.instances.MMedia" should "not fail" in {
    val program: LarsProgram = LarsParser("/parser-programs/MMedia.lars")
    val mmedia = MMediaDeterministicEvalInst(300,10000,Random)
    val mmediaProgram = mmedia.larsProgram(300)
    assert(mmediaProgram.toString == program.toString)
  }
}
