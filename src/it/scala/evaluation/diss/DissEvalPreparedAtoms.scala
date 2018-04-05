package evaluation.diss

import core.{Atom, Predicate}

/**
  * Created by hb on 05.04.18.
  */
object DissEvalPreparedAtoms {

  val a = atom("a")
  val b = atom("b")

  def atom(s: String) = Atom(Predicate(s))

}
