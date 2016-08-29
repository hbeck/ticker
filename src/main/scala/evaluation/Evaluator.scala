package evaluation

import core.Atom
import core.lars.{LarsProgram, TimePoint}
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildEngine
import engine.{EvaluationEngine, StreamEntry}
import jtms.{JtmsDoyle, JtmsGreedy, JtmsLearn}

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

//import scala.reflect.runtime.universe
//import scala.reflect.runtime._
import scala.util.Random

object Evaluator {


  def buildEngineFromArguments(args: Array[String], program: LarsProgram): EvaluationEngine = {

    val arguments = ArgumentParsing.argsParser(args)

    if (args.length != 2) {
      printUsageAndExit(args, "Supply the correct arguments")
    }

    val evaluationType = args(0)
    val evaluationModifier = args(1)

    val engine = BuildEngine.withProgram(program).fromArguments(evaluationType, evaluationModifier)
    if (engine.isDefined) {
      Console.println(f"Engine: $evaluationType $evaluationModifier")
      return engine.get
    } else {
      // TODO: not nice
      printUsageAndExit(args, "wrong combination of evaluation-type/modifier specified")
      return null
    }
  }


  def printUsageAndExit(args: Array[String], exitMessage: String) = {
    Console.err.println(exitMessage)
    Console.err.println()

    Console.out.println("You specified: " + args.mkString(" "))

    val optionsUsage = ArgumentParsing.options.map(o => f"-${o.option} <value>")

    Console.out.println("Usage: Evaluator " + optionsUsage.mkString(" "))
    Console.err.println()

    val optionsDescription = ArgumentParsing.options.
      groupBy(o => o.description).
      map(o => f"${o._1}: " + o._2.collect { case a: OptionValue => a.value })

    Console.out.println(optionsDescription.mkString("\n"))
    System.exit(-1)
  }

  def fromArguments(args: Array[String], instance: String, program: LarsProgram) = {
    Console.out.println(f"Evaluating ${instance}")

    val provider = () => Evaluator.buildEngineFromArguments(args, program)

    Evaluator(instance, provider)
  }

  def generateSignals(probabilities: Map[Atom, Double], random: Random, t0: TimePoint, t1: TimePoint) = {
    val signals = (t0.value to t1.value) map (t => {
      val atoms = selectAtoms(random)(probabilities)

      StreamEntry(TimePoint(t), atoms)
    })

    signals
  }


  def selectAtoms(random: Random)(probabilities: Map[Atom, Double]): Set[Atom] = {
    val atoms = probabilities filter {
      case (_, probability) => random.nextDouble() <= probability
    }
    atoms keySet
  }

}

case class Evaluator(instance: String, engineProvider: () => EvaluationEngine) {

  def streamInputsAsFastAsPossible(warmUps: Int = 2, repetitions: Int = 5)(inputs: Seq[StreamEntry]): TimingsConfigurationResult = {
    val appendExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()
    val evaluateExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()

    def test = {
      val engine = TimedEvaluationEngine(engineProvider(), appendExecutionTimes, evaluateExecutionTimes)

      inputs.foreach(i => {
        engine.append(i.time)(i.atoms.toSeq: _*)

        engine.evaluate(i.time)
      })
    }

    // warmup - we need to clear execution times afterwards
    (1 to warmUps) foreach (i => {
      test
      Console.println("Warm-up " + i)
    })
    appendExecutionTimes.clear()
    evaluateExecutionTimes.clear()

    profile.profileR(repetitions)(test)


    TimingsConfigurationResult(
      instance,
      StatisticResult.fromExecutionTimes(appendExecutionTimes),
      StatisticResult.fromExecutionTimes(evaluateExecutionTimes)
    )
  }

  def successfulModelComputations(inputs: Seq[StreamEntry]): SuccessConfigurationResult = {
    val engine = engineProvider()

    val modelDefined = inputs.zipWithIndex.map {
      case (entry, i) => {

        engine.append(entry.time)(entry.atoms.toSeq: _*)

        val model = engine.evaluate(entry.time)

        (i, model.get.isDefined)
      }
    }

    SuccessConfigurationResult(instance, modelDefined)
  }
}