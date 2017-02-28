
import java.io.File

import Program.InputTypes.InputTypes
import core.lars.{Format, LarsProgram}
import engine.config.EvaluationModifier.EvaluationModifier
import engine.config.EvaluationTypes._
import engine.config.{BuildEngine, EvaluationModifier, EvaluationTypes}
import runner._

import scala.concurrent.duration._
import scala.io.Source

/**
  * Created by FM on 05.11.16.
  */
object Program {

  def main(args: Array[String]): Unit = {

    val myargs = Seq(
      "--program", "src/test/resources/test.rules",
      "--evaluationType", "Clingo",
      "--evaluationModifier", "Push",
      "--inputType", "Http,StdIn",
      "--inputSpeed", "1s",
      "--outputSpeed", "1min"
    ).toArray

    parseParameters(args) match {
      case Some(config) => {
        val program = Load(config.inputSpeed.unit).readProgram(Source.fromFile(config.programFile))

        printProgram(program)

        println()

        val engine = BuildEngine.
          withProgram(program).
          withTickSize(config.inputSpeed).
          withConfiguration(config.evaluationType, config.evaluationModifier)

        engine match {
          case Some(e) => {
            val runner = EngineRunner(e, config.inputSpeed, config.outputSpeed)
            config.inputs foreach {
              case InputTypes.Http => runner.connect(ReadFromHttp(config.outputSpeed.unit))
              case InputTypes.StdIn => runner.connect(ReadFromStdIn(config.outputSpeed.unit))
            }
            runner.connect(OutputToStdOut())
            runner.start()
          }
          case None => throw new RuntimeException("Could not build engine!")
        }
      }
      case None => throw new RuntimeException("Could not parse arguments")
    }
  }

  def printProgram(program: LarsProgram): Unit = {
    println(f"Lars Program of ${program.rules.size} rules with ${program.extendedAtoms.size} different atoms.")

    if (program.rules.length <= 10) {
      Format(program) foreach println
    } else {
      println("First 10 rules:")
      Format(program.rules.take(10)) foreach println
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
        text("An evaluation type is required, possible values: " + EvaluationTypes.values)

      opt[EvaluationModifier]('m', "evaluationModifier").optional().valueName("<evaluation modifier>").
        action((x, c) => c.copy(evaluationModifier = x)).
        text("An evaluation type is required, possible values: " + EvaluationModifier.values)

      opt[Duration]("inputSpeed").optional().valueName("<value><unit>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("inputSpeed must be > 0")
          else
            Right()
        ).
        action((x, c) => c.copy(inputSpeed = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[Duration]("outputSpeed").optional().valueName("<value><unit>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("outputSpeed must be > 0")
          else
            Right()
        ).
        action((x, c) => c.copy(outputSpeed = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[Seq[InputTypes]]('i', "inputType").optional().valueName("<input type>,<input type>,...").
        action((x, c) => c.copy(inputs = x))
        .text("Possible Input Types: " + InputTypes.values)

      this.checkConfig(c =>
        if (c.inputSpeed >= c.outputSpeed)
          Left("inputSpeed must be lower than output")
        else
          Right()
      )

      this.checkConfig(c =>
        if (c.evaluationType == EvaluationTypes.Clingo && c.evaluationModifier != EvaluationModifier.Pull && c.evaluationModifier != EvaluationModifier.Push)
          Left("Invalid EvaluationModifier for evaluation Type")
        else if (c.evaluationType != EvaluationTypes.Clingo && (c.evaluationModifier == EvaluationModifier.Pull || c.evaluationModifier == EvaluationModifier.Push))
          Left("Invalid EvaluationModifier for evaluation Type")
        else
          Right()
      )

      help("help").text("Specify init parameters for running th engine")

    }

    parser.parse(args, Config(programFile = new File("")))
  }


  implicit val evaluationTypesRead: scopt.Read[EvaluationTypes.Value] =
    scopt.Read.reads(EvaluationTypes withName)


  implicit val evaluationModifierRead: scopt.Read[EvaluationModifier.Value] = scopt.Read.reads(EvaluationModifier withName)

  implicit val inputTypesRead: scopt.Read[InputTypes.Value] =
    scopt.Read.reads(InputTypes withName)


  object InputTypes extends Enumeration {
    type InputTypes = Value
    val StdIn, Http = Value
  }

  case class Config(evaluationType: EvaluationTypes = EvaluationTypes.Tms,
                    evaluationModifier: EvaluationModifier = EvaluationModifier.GreedyLazyRemove,
                    programFile: File,
                    inputSpeed: Duration = 100 milliseconds,
                    outputSpeed: Duration = 1 second,
                    inputs: Seq[InputTypes] = Seq(InputTypes.StdIn)
                   )

}



