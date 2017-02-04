package clingo


import core._
import core.asp.NormalProgram

/**
  * Created by FM on 25.02.16.
  */

object ClingoEvaluation {
  def apply(program: NormalProgram): Set[Model] = {
    val asp = new ClingoEvaluation(ClingoWrapper())

    asp(program)
  }

  def apply() = new ClingoEvaluation(ClingoWrapper())

  //string to Atom
  def convert(result: ClingoAtom): Atom = {
    if (!result.contains('('))
      return Atom(result)

    val nameParts = result.split('(')
    val predicate = Predicate(nameParts.head)
    val arguments = nameParts.tail.head.replace("(", "").replace(")", "").split(',')

    GroundAtom(predicate, (arguments map (Value(_))) :_*)
  }
}

class ClingoEvaluation(val clingo: ClingoWrapper) extends Evaluation {

  def apply(program: NormalProgram): Set[Model] = {
    apply(ClingoConversion(program))
  }

  def apply(clingoProgram: ClingoProgram): Set[Model] = {
    val result = clingo.run(clingoProgram)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(ClingoEvaluation.convert))
      return convertedModels
    }

    Set()
  }
}
