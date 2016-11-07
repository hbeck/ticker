import java.util.TimerTask
import java.util.concurrent.Executors

import Program.{EvaluationModifier, EvaluationTypes}
import engine.config.BuildEngine
import jtms.evaluation.Util

import scala.collection.parallel.FutureThreadPoolTasks
import java.io.File

import Program.EvaluationModifier.EvaluationModifier
import Program.EvaluationTypes.EvaluationTypes
import core.Atom
import engine.EvaluationEngine

import scala.io.Source

/**
  * Created by FM on 05.11.16.
  */
object Program {
  def main(args: Array[String]): Unit = {
//    val myargs = Seq(
//      "--program", "/Users/FM/Documents/OneDrive/Uni/Diplom_Beck/steen/src/test/resources/test.rules",
//      "--evaluationType", "Tms"
//    ).toArray

    parseParameters(args) match {
      case Some(config) => {
        val program = Util.readProgram(Source.fromFile(config.programFile))

        val engine = BuildEngine.
          withProgram(program).
          withConfiguration(config.evaluationType.toString.toLowerCase(), config.evaluationModifier.toString.toLowerCase())

        engine match {
          case Some(e) => run(e)
          case None => throw new RuntimeException("Could not build engine!")
        }
      }
      case None => throw new RuntimeException("Could not parse arguments")
    }
  }

  def run(engine: EvaluationEngine): Unit = {
    val executor = Executors.newSingleThreadExecutor()

    var time = 0

    val timer = new java.util.Timer()
    timer.scheduleAtFixedRate(new TimerTask {

      override def run(): Unit = executor.execute(new Runnable {
        override def run(): Unit = {
          time = time + 1
          val model = engine.evaluate(time)

          model.get match {
            case Some(m) => println(f"Model at T $time: $m")
            case None => println(f"No model at T $time")
          }

        }
      })
    }, 1000, 1000)

    Iterator.continually(scala.io.StdIn.readLine).
      map(line => line.
        split(',').
        map(_.trim).
        map(Atom(_)).array
      ).
      takeWhile(_.nonEmpty).
      foreach(atoms => {

        println(f"Received input ${atoms.mkString(", ")} at T $time")

        executor.execute(new Runnable {
          override def run(): Unit = {
            engine.append(time)(atoms: _*)
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
                  programFile: File
                 )
