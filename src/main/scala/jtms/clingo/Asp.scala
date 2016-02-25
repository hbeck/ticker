package jtms.clingo

import core.Rule

/**
  * Created by FM on 22.02.16.
  */
object Asp {

  def apply(rule: Rule): AspExpression = {
    if (rule.body.isEmpty) {
      return AspExpression(rule.head.caption + '.')
    } else {
      val iParts = rule.I.map(_.caption)
      val oParts = rule.O.map(_.caption).map("not " + _)

      val parts = iParts ++ oParts

      val expression = parts.mkString(rule.head.caption + " :- ", ", ", ".")

      AspExpression(expression)
    }
  }
}
