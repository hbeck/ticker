package clingo

import core._
import core.asp.{AspProgram, AspRule}

/**
  * Created by FM on 22.02.16.
  */
object ClingoConversion {

  def apply[TAtom <: Atom, TRule <: AspRule[TAtom]](program: AspProgram[TAtom, TRule]): ClingoProgram = {
    program.rules.map(apply[TAtom]).toSet
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
      case Predicate(caption) => (caption, "")
      case aa: AtomWithArgument => (aa.atom.toString, aa.arguments.map(apply).mkString("(", ",", ")"))
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
    case Value(v) => v
    case Variable(v) => v
  }
}
