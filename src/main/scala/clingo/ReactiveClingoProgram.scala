package clingo

import core._

/**
  * Created by fm on 25/01/2017.
  */

object ClingoSignalAtom {

  def fromAtom(atom: Atom): ClingoSignalAtom = atom match {
    case GroundAtomWithArguments(p, args) => ClingoSignalAtom(p, args)
    case NonGroundAtomWithArguments(p, args) => ClingoSignalAtom(p, convert(p, args))
    case _ => ClingoSignalAtom(atom.predicate, Seq())
  }

  def convert(predicate: Predicate, arguments: Seq[Argument]): Seq[Argument] = arguments collect {
    case v: Value => v
    case v: Variable => TickParameter(deriveCaption(predicate, v))
  }

  private def deriveCaption(predicate: Predicate, variable: Variable) = lowerCasedPredicate(predicate) + "_" + variable.name

  private def lowerCasedPredicate(predicate: Predicate) = predicate.caption.head.toLower + predicate.caption.tail
}

case class ClingoSignalAtom(atom: Predicate, arguments: Seq[Argument] = Seq()) {
  val atName: ClingoAtom = f"at_$atom"
  val cntName: ClingoAtom = f"cnt_$atom"
  val functionName = f"signals_${atom}_${arguments.size}"
  val parameters: Seq[TickParameter] = arguments collect {
    case t: TickParameter => t
  }
}

object TickParameter {
  def fromName(parameter: String): Either[String, TickParameter] = {
    if (parameter.exists(_.isWhitespace))
      Left("Parameter cannot contain whitespaces")
    else
      Right(TickParameter(parameter))
  }
}

case class TickParameter private[clingo](name: String) extends Variable {
  override def toString: ClingoExpression = name
}

case class TickConstraint(predicate: Predicate, parameter: TickParameter) {
  override def toString: ClingoExpression = f"$predicate($parameter)"
}

case class ReactiveClingoProgram(volatileRules: Set[ClingoExpression], signals: Set[ClingoSignalAtom]) {

  val timeConstraint: TickConstraint = TickConstraint(Predicate("now"), TickParameter("t"))
  val countConstraint: TickConstraint = TickConstraint(Predicate("cnt"), TickParameter("c"))

  val constraints = Seq(timeConstraint, countConstraint)
  val tickParameters: Seq[TickParameter] = constraints map (_.parameter)

  def external(constraint: TickConstraint): String = external(constraint.predicate.toString, Seq(constraint.parameter))

  def external(atom: ClingoAtom, arguments: Seq[Argument]): String = f"#external $atom(${argumentList(arguments)})."

  val externalConstraints: Seq[ClingoExpression] = constraints map external

  val signalPrograms: Set[ClingoExpression] = signals map (signal =>
    f"""#program ${signal.functionName}(${argumentList(tickParameters ++ signal.parameters)}).
       |
       |${external(signal.atName, signal.arguments :+ timeConstraint.parameter)}
       |${external(signal.cntName, signal.arguments :+ countConstraint.parameter)}
       |
     """.stripMargin)


  private val newLine = System.lineSeparator()
  val program: ClingoExpression =
    f"""${signalPrograms.mkString(newLine)}
       |
       |#program volatile(${tickParameters.mkString(", ")}).

       |${externalConstraints.mkString(newLine)}
       |
       |${volatileRules.mkString(newLine)}
       |
  """.stripMargin

  private def argumentList(arguments: Seq[Argument]) = arguments.mkString(", ")
}
