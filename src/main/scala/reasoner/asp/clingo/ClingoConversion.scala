package reasoner.asp.clingo

import core.asp.{AspProgram, AspRule}
import core.lars.{LarsBasedProgram, LarsRule, TimePoint, TimeVariableWithOffset}
import core.{GroundAtom, _}
import reasoner.common.LarsProgramEncoding

/**
  * Created by FM on 22.02.16.
  */
object ClingoConversion {

  def apply[TAtom <: Atom, TRule <: AspRule[TAtom]](program: AspProgram[TAtom, TRule]): ClingoProgram = {
    PlainClingoProgram(program.rules.map(apply[TAtom]).toSet)
  }

  def fromLars(program: LarsProgramEncoding): ClingoProgramWithLars = {
    val rules = program.rules.map(apply[Atom]).toSet
    ClingoProgramWithLars(rules, program.larsRules, program.maximumTimeWindowSizeInTicks)
  }

  def apply[TAtom <: Atom](rule: AspRule[TAtom]): ClingoExpression = {
    //println(rule)
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

  def apply(relationAtom: RelationAtom): ClingoAtom = relationAtom match {
    case Eq(l, r) => f"$l = $r"
    case Neq(l, r) => f"$l != $r"
    case Leq(l, r) => f"$l <= $r"
    case Geq(l, r) => f"$l >= $r"
    case Lt(l, r) => f"$l < $r"
    case Gt(l, r) => f"$l > $r"
    case Plus(l, r, e) => f"$l + $r = $e"
    case Minus(l, r, e) => f"$l - $r = $e"
    case Times(l, r, e) => f"$l * $r = $e"
    case Divide(l, r, e) => f"$l / $r = $e"
    case Modulo(l, r, e) => f"$l \\ $r = $e"
    case Power(l, r, e) => f"$l ** $r = $e"
    case LeqLeq(l, x, u) => f"$x = $l .. $u" // l <= x <= u which has x safe automatically
    case LtLt(l, x, u) => f"$x = ${l+1} .. ${u-1}" //l < x < u
    case LtLeq(l, x, u) => f"$x = ${l+1} .. $u"
    case LeqLt(l, x, u) => f"$x = $l .. ${u-1}"
    case Incr(l, r) => f"$l + 1 = $r"
  }

  def apply[TAtom](atom: TAtom): ClingoAtom = {
    val (atomName, argumentNames) = atom match {
      case x: ContradictionAtom => return ""
      case r: RelationAtom => return this.apply(r)
      case aa: AtomWithArguments => (aa.predicate.toString, aa.arguments.map(apply).mkString("(", ",", ")"))
      case x: GroundAtom => (x.predicate.caption, "")
      case _ => (atom.toString, "")
    }

    if (atomName.head.isUpper)
      throw new IllegalArgumentException("Currently only constants are allowed in an ASP expression. In ASP a constant starts with an lower-case character. You provided " + atom)

    if (atomName exists (_.isWhitespace))
      throw new IllegalArgumentException("Constants in ASP cannot contain a whitespace. You provided " + atom)

    if (!(atomName matches "^[a-zA-Z0-9_]*$"))
      throw new IllegalArgumentException("Constants in ASP cannot contain illegal characters!. You provided " + atom)

    if (argumentNames.size > 2)
      atomName + argumentNames
    else
      atomName
  }

  def apply(argument: Argument): String = argument match {
    case StringValue(v) => v
    case IntValue(v) => v.toString
    case TimePoint(t) => t.toString
    case v: VariableWithOffset => v.toString
    case v: TimeVariableWithOffset => v.toString
    case v: Variable => v.name
  }
}

trait ClingoProgram { //extends Program... not so easy
  val rules: Set[ClingoExpression]
}

case class PlainClingoProgram(rules: Set[ClingoExpression]) extends ClingoProgram

case class ClingoProgramWithLars(rules: Set[ClingoExpression], larsRules: Seq[LarsRule], maximumTimeWindowSizeInTicks: Long) extends ClingoProgram with LarsBasedProgram {

}