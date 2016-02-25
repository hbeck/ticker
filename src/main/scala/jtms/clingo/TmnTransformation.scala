package jtms.clingo

import jtms.Justification

/**
  * Created by FM on 22.02.16.
  */
object TmnTransformation {

  def apply(j: Justification): ClingoExpression = {
    if ((j.I union j.O).isEmpty) {
      return j.n.caption + '.'
    } else {
      val iParts = j.I.map(_.caption)
      val oParts = j.O.map(_.caption).map("not " + _)

      val parts = iParts ++ oParts

      parts.mkString(j.n.caption + " :- ", ", ", ".")
    }
  }
}
