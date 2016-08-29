package evaluation

/**
  * Created by FM on 29.08.16.
  */

sealed trait OptionIdentifier {
  val option: String
  val description: String
}

sealed trait OptionValue extends OptionIdentifier {
  val value: String
}

sealed trait EvaluationType extends OptionValue {
  val value: String
  val description = "Evaluation-Type"
  val option = "et"
}

object Tms extends EvaluationType {
  val value = "tms"
}

object Clingo extends EvaluationType {
  val value = "clingo"
}

sealed trait EvaluationModifier extends OptionValue {
  val value: String
  val description = "Evaluation-Modifier"
  val option = "em"
}

object Greedy extends EvaluationModifier {
  val value = "greedy"
}

object Doyle extends EvaluationModifier {
  val value = "Doyle"
}

object Learn extends EvaluationModifier {
  val value = "Learn"
}

object Input extends OptionIdentifier {
  val option = "in"
  val description = "Input-File"
}

case class Input(value: String) extends OptionValue {
  val option = "in"
  val description = "Input-File"
}


object ArgumentParsing {

  val options: Set[OptionIdentifier] = Set(Tms, Clingo, Greedy, Doyle, Learn, Input)


  def argsParser(args: Array[String]): Set[OptionValue] = {
    val foundOptions = args.zip(args.tail).
      map(arg => {
        val matchedOption = options.
          filter(o => o.option == arg._1).
          collectFirst {
            case o: OptionValue => o
            case o: Input => Input(arg._2)
          }
        matchedOption
      }).
      filter(o => o.isDefined).
      map(o => o.get)

    foundOptions.toSet
  }
}