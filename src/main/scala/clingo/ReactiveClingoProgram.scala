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

case class TickConstraint(atom: TickAtom, parameter: TickParameter) {
  override def toString: ClingoExpression = f"$atom($parameter)"
}

case class ReactiveClingoProgram(volatileRules: Set[ClingoExpression], signals: Set[ClingoSignalAtom],
                                 //                                 constraints: Seq[TickConstraint] = Seq(TickConstraint(TickAtom("now"), TickParameter("t")), TickConstraint(TickAtom("cnt"), TickParameter("c")))
                                 timeParameter: TickParameter = TickParameter("t"),
                                 countParameter: TickParameter = TickParameter("c")
                                ) {

  val tickParameters = Seq(timeParameter, countParameter)

  val newLine = System.lineSeparator()

  val signalPrograms = signals map (s =>
    f"""#program signals_${s.atom}_${s.arguments.size}(${argumentList(tickParameters ++ s.arguments)}).
       |
       |#external at_${s.atom}(${argumentList(s.arguments.+:(timeParameter))}).
       |#external cnt_${s.atom}(${argumentList(s.arguments.+:(countParameter))}).
       |
     """.stripMargin)


  val program =
    f"""${signalPrograms.mkString(newLine)}
       |
       |#program volatile(${tickParameters.mkString(", ")}).

       |${volatileRules.mkString(newLine)}
       |
  """.stripMargin

  def argumentList(arguments: Seq[TickParameter]) = arguments.mkString(", ")
}
