package clingo


import core.{Value, _}
import core.asp.NormalProgram
import core.lars.TimePoint

/**
  * Created by FM on 25.02.16.
  */

object ClingoEvaluation {
  def apply(program: NormalProgram): Set[Model] = {
    val asp = new ClingoEvaluation(ClingoWrapper())

    asp(program)
  }

  def apply() = new ClingoEvaluation(ClingoWrapper())


  private val TimeAtomPattern = "(.+)_at".r
  private val CntAtomPattern = "(.+)_cnt".r
  private val TimeCntAtomPattern = "(.+)_at_cnt".r

  //string to Atom
  def convert(result: ClingoAtom): Atom = {
    if (!result.contains('('))
      return Atom(result)


    val nameParts = result.split('(')

    val predicateName = nameParts.head
    val arguments = nameParts.tail.head.
      replace("(", "").
      replace(")", "").
      split(',').
      map(Value(_)).
      toSeq

    val atom = (predicateName, arguments) match {
      case (TimeCntAtomPattern(predicate), Seq(IntValue(t), tick: IntValue)) => PinnedAtom(Atom(predicate), TimePoint(t), tick)
      case (CntAtomPattern(predicate), Seq(i: IntValue)) => PinnedAtom.asCount(Atom(predicate), i)
      case (TimeAtomPattern(predicate), Seq(IntValue(t))) => PinnedAtom(Atom(predicate), TimePoint(t))
      case _ => GroundAtom(Predicate(predicateName), arguments: _*)
    }

    atom
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
