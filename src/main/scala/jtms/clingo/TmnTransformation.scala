package jtms.clingo

import jtms.Rule

/**
  * Created by FM on 22.02.16.
  */
object TmnTransformation {

  def apply(j: Rule): ClingoExpression = {
    if ((j.I union j.O).isEmpty) {
      return j.head.caption + '.'
    } else {
      val iParts = j.I.map(_.caption)
      val oParts = j.O.map(_.caption).map("not " + _)

      val parts = iParts ++ oParts

      parts.mkString(j.head.caption + " :- ", ", ", ".")
    }
  }
}
