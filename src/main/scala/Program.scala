
import engine.config.{BuildEngine, EvaluationModifier, EvaluationTypes}
import jtms.evaluation.Util
import java.io.File

import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.EvaluationTypes.EvaluationTypes
import runner.EngineRunner

import scala.concurrent.duration._
import scala.io.Source

/**
  * Created by FM on 05.11.16.
  */
object Program {

  def main(args: Array[String]): Unit = {

    val myargs = Seq(
      "--program", "/Users/FM/Documents/OneDrive/Uni/Diplom_Beck/steen/src/test/resources/test.rules",
      "--evaluationType", "Tms"
    ).toArray

    parseParameters(myargs) match {
      case Some(config) => {
        val program = Util.readProgram(Source.fromFile(config.programFile))

        val engine = BuildEngine.
          withProgram(program).
          withTickSize(config.inputSpeed).
          withConfiguration(config.evaluationType, config.evaluationModifier)

        engine match {
          case Some(e) => {
            val runner = EngineRunner(e, config.inputSpeed, config.outputSpeed)
            runner.receiveInputFromStdIn(config.outputSpeed.unit)
            runner.receiveInputOverHttp
            runner.start()
          }
          case None => throw new RuntimeException("Could not build engine!")
        }
      }
      case None => throw new RuntimeException("Could not parse arguments")
    }
  }


  def parseParameters(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("scopt", "3.x")

      opt[File]('p', "program").required().valueName("<file>").
        action((x, c) => c.copy(programFile = x)).
        text("program is a required file property")

      opt[EvaluationTypes]('e', "evaluationType").required().valueName("<evaluation type>").
        action((x, c) => c.copy(evaluationType = x)).
        text("An evaluation type is required")

      opt[EvaluationModifier]('m', "evaluationModifier").optional().valueName("<evaluation modifier>").
        action((x, c) => c.copy(evaluationModifier = x)).
        text("An evaluation type is required")

      opt[Duration]("inputSpeed").optional().valueName("<input speed>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("inputSpeed must be > 0")
          else
            Right()
        ).
        action((x, c) => c.copy(inputSpeed = x))

      opt[Duration]("outputSpeed").optional().valueName("<output speed>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("outputSpeed must be > 0")
          else
            Right()
        ).
        action((x, c) => c.copy(outputSpeed = x))

      this.checkConfig(c =>
        if (c.inputSpeed >= c.outputSpeed)
          Left("inputSpeed must be lower than output")
        else
          Right()
      )

      help("help").text("Specify init parameters for running th engine")

    }

    parser.parse(args, Config(programFile = new File("")))
  }


  implicit val evaluationTypesRead: scopt.Read[EvaluationTypes.Value] =
    scopt.Read.reads(EvaluationTypes withName)


  implicit val evaluationModifierRead: scopt.Read[EvaluationModifier.Value] =
    scopt.Read.reads(EvaluationModifier withName)


  case class Config(evaluationType: EvaluationTypes = EvaluationTypes.Tms,
                    evaluationModifier: EvaluationModifier = EvaluationModifier.Greedy,
                    programFile: File,
                    inputSpeed: Duration = 100 milliseconds,
                    outputSpeed: Duration = 1 second
                   )

}



