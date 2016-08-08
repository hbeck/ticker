package evaluation

import core.Atom
import core.lars.{Duration, LarsProgram, TimePoint}
import engine.EvaluationEngine
import engine.asp.oneshot.EvaluationMode
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildEngine
import jtms.{JtmsDoyle, JtmsGreedy}

import scala.collection.mutable.ArrayBuffer

//import scala.reflect.runtime.universe
//import scala.reflect.runtime._
import scala.util.Random

object Evaluator {

  def buildEngineFromArguments(args: Array[String], programLoader: String => LarsProgram): EvaluationEngine = {
    if (args.length != 3) {
      printUsageAndExit(args, "Supply the correct arguments")
    }
    val program = programLoader(args(2))

    val evaluationType = args(0)
    val evaluationModifier = args(1)

    val engine = buildEngine(program, evaluationType, evaluationModifier)
    if (engine.isDefined) {
      return engine.get
    } else {
      // TODO: not nice
      printUsageAndExit(args, "wrong combination of evaluation-type/modifier specified")
      return null
    }
  }


  def buildEngine(program: LarsProgram, evaluationType: String, evaluationModifier: String): Option[EvaluationEngine] = {
    // TODO: not nice

    if (evaluationType == "tms") {
      if (evaluationModifier == "greedy") {
        return Some(greedyTms(program))
      } else if (evaluationModifier == "doyle") {
        return Some(doyleTms(program))
      }
    } else if (evaluationType == "clingo") {
      if (evaluationModifier == "push") {
        return Some(clingoPush(program))
      } else if (evaluationModifier == "pull") {
        return Some(clingoPull(program))
      }
    }

    None
  }

  def greedyTms(program: LarsProgram) = {
    val tms = JtmsGreedy(new Random(1))
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def doyleTms(program: LarsProgram) = {
    val tms = JtmsDoyle(new Random(1))

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def clingoPush(program: LarsProgram) = {
    BuildEngine.withProgram(program).useAsp().withClingo().use().usePush().start()
  }

  def clingoPull(program: LarsProgram) = {
    BuildEngine.withProgram(program).useAsp().withClingo().use().usePull().start()
  }


  def printUsageAndExit(args: Array[String], exitMessage: String) = {
    Console.err.println(exitMessage)
    Console.err.println()

    Console.out.println("You specified: " + args.mkString(" "))

    Console.out.println("Usage: Evaluator <evaluation-type> <evaluation-modifier> <input-file>")
    Console.err.println()
    Console.out.println("evaluation-type: asp or tms")
    Console.out.println("evaluation-modifier: greedy or doyle")
    Console.out.println("   asp: pull or push")
    Console.out.println("   tms: greedy or doyle")
    System.exit(-1)
  }
}

case class Evaluator(engineProvider: () => EvaluationEngine, warmups: Int = 5, repetitions: Int = 5) {

  def streamInputsAsFastAsPossible(inputs: Seq[(TimePoint, Seq[Atom])]): (StatisticResult, StatisticResult) = {
    val appendExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()
    val evaluateExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()

    profile.withWarmup(warmups, repetitions)({
      val engine = new TimedEvaluationEngine(engineProvider(), appendExecutionTimes, evaluateExecutionTimes)

      inputs.foreach(i => {
        engine.append(i._1)(i._2: _*)

        engine.evaluate(i._1)
      })
    })

    (
      StatisticResult.fromExecutionTimes(appendExecutionTimes),
      StatisticResult.fromExecutionTimes(evaluateExecutionTimes)
      )
  }
}