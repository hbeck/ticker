package jtms.clingo

import core.{Atom, Program}

/**
  * Created by FM on 25.02.16.
  */

object Asp {
  def apply(program: Program): Set[Set[Atom]] = {
    val asp = new Asp(ClingoWrapper())

    asp(program)
  }
}

class Asp(val clingo: ClingoWrapper) {

  def apply(program: Program): Set[Set[Atom]] = {
    val expressions = AspConversion(program)

    val result = clingo.run(expressions)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(a => Atom(a)))
      return convertedModels
    }

    Set()
  }

}
