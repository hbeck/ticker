package clingo

import core.{Atom, Predicate}

/**
  * Created by fm on 25/01/2017.
  */

case class ClingoSignalAtom(atom: Predicate, arguments: Seq[TickParameter] = Seq())

object TickParameter {
  def fromName(parameter: String): Either[String, TickParameter] = {
    if (parameter.exists(_.isWhitespace))
      Left("Parameter cannot contain whitespaces")
    else
      Right(TickParameter(parameter))

  }
}

case class TickParameter private[clingo](value: String) {
  override def toString: ClingoExpression = value
}

case class TickAtom(atom: ClingoAtom) {
  override def toString: ClingoExpression = atom
}

case class TickConstraint(predicate: Predicate, parameter: TickParameter) {
  override def toString: ClingoExpression = f"$predicate($parameter)"
}

case class ReactiveClingoProgram(volatileRules: Set[ClingoExpression], signals: Set[ClingoSignalAtom],
                                 time: TickConstraint = TickConstraint(Predicate("now"), TickParameter("t")),
                                 count: TickConstraint = TickConstraint(Predicate("cnt"), TickParameter("c"))
                                ) {

  val newLine = System.lineSeparator()

  val constraints = Seq(time, count)
  val tickParameters = constraints map (_.parameter)

  def external(constraint: TickConstraint): String = external(constraint.predicate.toString, Seq(constraint.parameter))

  def external(atom: ClingoAtom, arguments: Seq[TickParameter]): String = f"#external $atom(${argumentList(arguments)})."

  val externalConstraints = constraints map external

  val signalPrograms = signals map (s =>
    f"""#program signals_${s.atom}_${s.arguments.size}(${argumentList(tickParameters ++ s.arguments)}).
       |
       |${external(f"at_${s.atom}", s.arguments :+ time.parameter)}
       |${external(f"cnt_${s.atom}", s.arguments :+ count.parameter)}
       |
     """.stripMargin)


  val program =
    f"""${signalPrograms.mkString(newLine)}
       |
       |#program volatile(${tickParameters.mkString(", ")}).

       |${externalConstraints.mkString(newLine)}
       |
       |${volatileRules.mkString(newLine)}
       |
  """.stripMargin

  def argumentList(arguments: Seq[TickParameter]) = arguments.mkString(", ")
}
