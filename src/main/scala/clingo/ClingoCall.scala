package clingo


import core.{Value, _}
import core.asp.NormalProgram
import core.lars.TimePoint

/**
  * Created by FM on 25.02.16.
  */

object ClingoCall {
  def apply(program: NormalProgram): Set[Model] = {
    val asp = new ClingoCall(ClingoWrapper())

    asp(program)
  }

  def apply() = new ClingoCall(ClingoWrapper())

  private val TimeAtomPattern = "(.+)_at".r
  //private val CntAtomPattern = "(.+)_cnt".r
  private val TimeCntAtomPattern = "(.+)_at_cnt".r

  //string to Atom
  def convert(result: ClingoAtom): Atom = {
    if (!result.contains('(')) {
      return Atom(result)
    }

    val nameParts = result.split('(')

    val predicateName = nameParts.head
    val arguments = nameParts.tail.head.
      replace("(", "").
      replace(")", "").
      split(',').
      map(Value(_)).
      toSeq

    val atom = (predicateName, arguments) match {
      case (TimeCntAtomPattern(predicate), Seq(IntValue(t), cnt: IntValue)) => PinnedAtom.asPinnedAtCntAtom(Atom(predicate), TimePoint(t), cnt)
      case (TimeAtomPattern(predicate), Seq(IntValue(t))) => PinnedAtom.asPinnedAtAtom(Atom(predicate), TimePoint(t))
      case _ => GroundAtom(Predicate(predicateName), arguments)
    }

    atom
  }
}

class ClingoCall(val clingo: ClingoWrapper) extends Evaluation {

  def apply(program: NormalProgram): Set[Model] = {
    apply(ClingoConversion(program))
  }

  def apply(clingoProgram: ClingoProgram): Set[Model] = {
    val result = clingo.run(clingoProgram)
    val models = clingo.parseResult(result)

    if (models.isDefined) {
      val convertedModels = models.get.map(m => m.map(ClingoCall.convert))
      return convertedModels
    }

    Set()
  }
}
