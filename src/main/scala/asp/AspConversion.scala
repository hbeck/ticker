package asp

import core.{Atom, Program, Rule}

/**
  * Created by FM on 22.02.16.
  */
object AspConversion {

  def apply(program: Program): Set[AspExpression] = {
    program.rules.map(apply)
  }

  def apply(rule: Rule): AspExpression = {
    if (rule.body.isEmpty) {
      return AspExpression(apply(rule.head) + '.')
    } else {
      val iParts = rule.I.map(apply)
      val oParts = rule.O.map(apply).map("not " + _)

      val parts = iParts ++ oParts

      val expression = parts.mkString(apply(rule.head) + " :- ", ", ", ".")

      AspExpression(expression)
    }
  }

  def apply(atom: Atom): String = {
    if (atom.caption.head.isUpper)
      throw new IllegalArgumentException("Currently only constants are allowed in an ASP expression. In ASP a constant starts with an lower-case character. You provided " + atom)

    if (atom.caption.exists(c => c.isWhitespace))
      throw new IllegalArgumentException("Constants in ASP cannot contain a whitespace. You provided " + atom)

    atom.caption
  }
}
