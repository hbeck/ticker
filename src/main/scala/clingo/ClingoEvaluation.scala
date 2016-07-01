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

  def convert(result: ClingoAtom): Atom = {
    if (!result.contains('('))
      return Atom(result)

    val nameParts = result.split('(')
    val atom = Atom(nameParts.head)
    val arguments = nameParts.tail.head.replace("(", "").replace(")", "").split(',')

    // TODO convert arguments into correct type

    //    val regex = """^(\w+)(\((\w+)(,)?+\))?""".r("atom", "arguments")

    //    var matches = regex.findAllMatchIn(result)
    //    var matche = regex.findFirstMatchIn(result)
    //
    //    var name = matche.get.group("atom")
    //    var arguments = matche.get.group("arguments")

    // TODO do we have always ground results?
    NonGroundAtom(atom, arguments map Argument.convertToArgument)
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
