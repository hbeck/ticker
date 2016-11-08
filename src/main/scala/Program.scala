import java.util.TimerTask
import java.util.concurrent.Executors

import Program.{EvaluationModifier, EvaluationTypes}
import engine.config.BuildEngine
import jtms.evaluation.Util

import scala.collection.parallel.FutureThreadPoolTasks
import java.io.File

import Program.EvaluationModifier.EvaluationModifier
import Program.EvaluationTypes.EvaluationTypes
import core.{Atom, Model}
import engine.{EvaluationEngine, NoResult, Result}

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
          withConfiguration(config.evaluationType.toString.toLowerCase(), config.evaluationModifier.toString.toLowerCase())

        engine match {
          case Some(e) => run(e, config.inputSpeed, config.outputSpeed)
          case None => throw new RuntimeException("Could not build engine!")
        }
      }
      case None => throw new RuntimeException("Could not parse arguments")
    }
  }

  def run(engine: EvaluationEngine, engineSpeed: Duration, outputSpeed: Duration): Unit = {
    val executor = Executors.newSingleThreadExecutor()

    var ticks = 0

    def convertTicksToOutput(tick: Long) = tick * engineSpeed.toMillis / outputSpeed.toMillis
    def convertTicksToInput(tick:Long) = tick * engineSpeed.toMillis

    val timer = new java.util.Timer()
    timer.scheduleAtFixedRate(new TimerTask {

      override def run(): Unit = ticks = ticks + 1

    }, engineSpeed.toMillis, engineSpeed.toMillis)

    var lastModel: Result = NoResult

    timer.scheduleAtFixedRate(new TimerTask {

      override def run(): Unit = executor.execute(new Runnable {
        override def run(): Unit = {

          val model = engine.evaluate(ticks)
          if (model.get != lastModel.get) {
            val timeInOutput = Duration(convertTicksToOutput(ticks), outputSpeed.unit)
            model.get match {
              case Some(m) => println(f"Model at T $timeInOutput: $m")
              case None => println(f"No model at T $timeInOutput")
            }
            lastModel = model
          }

        }
      })
    }, outputSpeed.toMillis, outputSpeed.toMillis)

    Iterator.continually(scala.io.StdIn.readLine).
      map(line => line.
        split(',').
        map(_.trim).
        map(Atom(_)).array
      ).
      takeWhile(_.nonEmpty).
      foreach(atoms => {

        val inputTicks = Duration(convertTicksToInput(ticks), engineSpeed.unit)
        println(f"Received input ${atoms.mkString(", ")} at T $inputTicks")

        executor.execute(new Runnable {
          override def run(): Unit = {
            engine.append(ticks)(atoms: _*)
          }
        })
      })
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
        action((x, c) => c.copy(inputSpeed = x))
      opt[Duration]("outputSpeed").optional().valueName("<output speed>").
        action((x, c) => c.copy(outputSpeed = x))

      help("help").text("prints this usage text")

    }

    parser.parse(args, Config(programFile = new File("")))
  }

  object EvaluationTypes extends Enumeration {
    type EvaluationTypes = Value
    val Tms, Clingo = Value
  }

  implicit val evaluationTypesRead: scopt.Read[EvaluationTypes.Value] =
    scopt.Read.reads(EvaluationTypes withName _)

  object EvaluationModifier extends Enumeration {
    type EvaluationModifier = Value
    val Greedy, Doyle, Learn = Value
  }

  implicit val evaluationModifierRead: scopt.Read[EvaluationModifier.Value] =
    scopt.Read.reads(EvaluationModifier withName _)
}

case class Config(evaluationType: EvaluationTypes = EvaluationTypes.Tms,
                  evaluationModifier: EvaluationModifier = EvaluationModifier.Greedy,
                  programFile: File,
                  inputSpeed: Duration = 100 milliseconds,
                  outputSpeed: Duration = 1 second
                 )
