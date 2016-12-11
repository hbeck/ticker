package clingo

import core.{GroundAtom, _}
import core.asp.{AspProgram, AspRule}
import core.lars.{LarsBasedProgram, LarsRule}

/**
  * Created by FM on 22.02.16.
  */
object ClingoConversion {

  def apply[TAtom <: Atom, TRule <: AspRule[TAtom]](program: AspProgram[TAtom, TRule]): ClingoProgram = {
    PlainClingoProgram(program.rules.map(apply[TAtom]).toSet)
  }

  def fromLars[TAtom <: Atom, TRule <: AspRule[TAtom]](program: AspProgram[TAtom, TRule] with LarsBasedProgram): ClingoProgramWithLars = {
    val rules = program.rules.map(apply[TAtom]).toSet
    ClingoProgramWithLars(rules, program.larsRules)
  }

  def apply[TAtom <: Atom](rule: AspRule[TAtom]): ClingoExpression = {
    if (rule.body.isEmpty) {
      apply(rule.head) + '.'
    } else {
      val iParts = rule.pos.map(apply[TAtom])
      val oParts = rule.neg.map(apply[TAtom]).map("not " + _)

      val parts = iParts ++ oParts

      val expression = parts.mkString(apply(rule.head) + " :- ", ", ", ".").trim

      expression
    }
  }

  def apply[TAtom](atom: TAtom): ClingoAtom = {
    val (atomName, argumentNames) = atom match {
      case x: ContradictionAtom => return ""
      case aa: AtomWithArgument => (aa.predicate.toString, aa.arguments.map(apply).mkString("(", ",", ")"))
      case x: GroundAtom => (x.predicate.caption, "")
      case _ => (atom.toString, "")
    }

    if (atomName.head.isUpper)
      throw new IllegalArgumentException("Currently only constants are allowed in an ASP expression. In ASP a constant starts with an lower-case character. You provided " + atom)

    if (atomName exists (_.isWhitespace))
      throw new IllegalArgumentException("Constants in ASP cannot contain a whitespace. You provided " + atom)

    if (!(atomName matches "^[a-zA-Z0-9_]*$"))
      throw new IllegalArgumentException("Constants in ASP cannot contain illegal characters!. You provided " + atom)

    atomName + argumentNames
  }

  def apply(argument: Argument): String = argument match {
    case StringValue(v) => v
    case IntValue(v) => v.toString
    case TimeValue(t) => t.toString

    case v: Variable => v.name
  }
}

trait ClingoProgram {
  val rules: Set[ClingoExpression]
}

case class PlainClingoProgram(rules: Set[ClingoExpression]) extends ClingoProgram

case class ClingoProgramWithLars(rules: Set[ClingoExpression], larsRules: Seq[LarsRule]) extends ClingoProgram with LarsBasedProgram {

}