import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import Program.{EvaluationModifier, EvaluationTypes}
import engine.config.BuildEngine
import jtms.evaluation.Util

import scala.collection.parallel.FutureThreadPoolTasks
import java.io.File

import Program.EvaluationModifier.EvaluationModifier
import Program.EvaluationTypes.EvaluationTypes
import core.lars.TimePoint
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
          case Some(e) => {
            val runner = EngineRunner(e, config.inputSpeed, config.outputSpeed)
            runner.start()
            runner.receiveInputFromStdIn()
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

case class EngineRunner(engine: EvaluationEngine, engineSpeed: Duration, outputSpeed: Duration) {
  val executor = Executors.newSingleThreadExecutor()

  var ticks: TimePoint = TimePoint(0)

  val timer = new java.util.Timer()

  var lastModel: Result = NoResult

  def convertTicksToOutput(tick: TimePoint) = Duration(tick.value * engineSpeed.toMillis / outputSpeed.toMillis, outputSpeed.unit)

  def convertTicksToInput(tick: TimePoint) = Duration(Duration(tick.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS).toUnit(engineSpeed.unit), engineSpeed.unit)

  def updateTicks(): Unit = ticks = ticks + 1

  def evaluateModel(): Unit = {

    val model = engine.evaluate(ticks)
    if (model.get != lastModel.get) {
      val timeInOutput = convertTicksToOutput(ticks)
      model.get match {
        case Some(m) => println(f"Model at T $timeInOutput: $m")
        case None => println(f"No model at T $timeInOutput")
      }
      lastModel = model
    }
  }

  def append(atoms: Seq[Atom]): Unit = {
    // TODO: discuss which time to use (capture ticks here or in .run)
    executor.execute(new Runnable {
      override def run(): Unit = {
        engine.append(ticks)(atoms: _*)
      }
    })
  }

  def start(): Unit = {
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = updateTicks
    }, engineSpeed.toMillis, engineSpeed.toMillis)

    val modelUpdate = new Runnable {
      override def run(): Unit = evaluateModel
    }
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = executor.execute(modelUpdate)
    }, outputSpeed.toMillis, outputSpeed.toMillis)
  }

  def receiveInputFromStdIn(): Unit = {
    Iterator.continually(scala.io.StdIn.readLine).
      map(line => line.
        split(',').
        map(_.trim).
        map(Atom(_)).array
      ).
      takeWhile(_.nonEmpty).
      foreach(atoms => {

        val inputTicks = convertTicksToInput(ticks)
        println(f"Received input ${atoms.mkString(", ")} at T $inputTicks")

        append(atoms)
      })
  }
}
