package asp

import core.{Program, Rule}

/**
  * Created by FM on 22.02.16.
  */
object AspConversion {

  def apply(program: Program): Seq[AspExpression] = {
    program.rules.map(apply)
  }

  def apply(rule: Rule): AspExpression = {
    if (rule.body.isEmpty) {
      return AspExpression(rule.head.caption + '.')
    } else {
      val iParts = rule.pos.map(_.caption)
      val oParts = rule.neg.map(_.caption).map("not " + _)

      val parts = iParts ++ oParts

      val expression = parts.mkString(rule.head.caption + " :- ", ", ", ".")

      AspExpression(expression)
    }
  }
}
