package asp

import core.EvaluationResult.Model
import core._

/**
  * Created by FM on 25.02.16.
  */

object Asp {
  def apply(program: Program): Set[Model] = {
    val asp = new Asp(ClingoWrapper())

    asp(program)
  }

  def apply() = new Asp(ClingoWrapper())

  def convert(result: String): Atom = {
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

    AtomWithArguments(atom, arguments)
  }
}

class Asp(val clingo: ClingoWrapper) extends Evaluation {

  def apply(program: Program): Set[Model] = {
    val expressions = AspConversion(program)

    val result = clingo.run(expressions)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(Asp.convert))
      return convertedModels
    }

    Set()
  }
}
