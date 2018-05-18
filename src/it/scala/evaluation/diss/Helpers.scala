package evaluation.diss

import core._
import core.lars._

/**
  * Created by hb on 05.04.18.
  */
object Helpers {

  implicit def string2Atom(atomStr: String): Atom = {
    if (atomStr.indexOf("(") == -1) return Atom(Predicate(atomStr))
    val tokens: Seq[String] = {
      atomStr.replace("("," ").
        replace(")","").
        replace(","," ").split(" ")
    }

    val predicate = Predicate(tokens(0))

    val arguments: Seq[Argument] = tokens.tail.map { s =>
      if (s.charAt(0).isLower) {
        StringValue(s)
      }
      else if (s.charAt(0).isUpper) {
        Variable(s)
      }
      else if (s.charAt(0).isDigit) {
        IntValue(s)
      }
      else {
        throw new RuntimeException("cannot parse " + s + " within atom " + atomStr)
      }
    }

    Atom(predicate,arguments)

  }

  implicit def string2Predicate(caption: String): Predicate = Predicate(caption)

  def wt_At(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TimeWindow(windowSize), At(time), atom)
  def wt_D(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Diamond, atom)
  def wt_B(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Box, atom)
  def wc_At(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TupleWindow(windowSize), At(time), atom)
  def wc_D(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Diamond, atom)
  def wc_B(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Box, atom)

  def rule(head: HeadAtom, posBody: Set[ExtendedAtom], negBody: Set[ExtendedAtom]=Set()): LarsRule = {
    UserDefinedLarsRule(head, posBody, negBody)
  }

  def rule(head: HeadAtom, posBody: ExtendedAtom, negBody: ExtendedAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(posBody), Set(negBody))
  }

  def rule(head: HeadAtom, posBody: ExtendedAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(posBody), Set())
  }

  def fact(head: HeadAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(), Set())
  }

  def mustHave(model:Model, a: Atom, t: Int): Unit = {
    if (!model.contains(a)) {
      println(f"model at t=$t should contain atom $a: $model")
      assert(false)
    }
  }

  def mustNotHave(model:Model, a: Atom, t: Int): Unit = {
    if (model.contains(a)) {
      println(f"model at t=$t should not contain atom $a: $model")
      assert(false)
    }
  }

  def mustBeEmpty(model: Model, t: Int): Unit = {
    if (!model.isEmpty) {
      println(f"model at t=$t should be empty: $model")
      assert(false)
    }
  }

}
