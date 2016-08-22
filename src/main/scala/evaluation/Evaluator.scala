package evaluation

import core.Atom
import core.lars.{Duration, LarsProgram, TimePoint}
import engine.{EvaluationEngine, StreamEntry}
import engine.asp.oneshot.EvaluationMode
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildEngine
import jtms.{JtmsDoyle, JtmsGreedy, JtmsLearn}

import scala.collection.mutable.ArrayBuffer

//import scala.reflect.runtime.universe
//import scala.reflect.runtime._
import scala.util.Random

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

object Evaluator {

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

  def buildEngineFromArguments(args: Array[String], programLoader: String => LarsProgram): EvaluationEngine = {

    val arguments = argsParser(args)
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


  def buildEngine(program: LarsProgram,
                  evaluationType: String,
                  evaluationModifier: String,
                  random: Random = new Random(1)): Option[EvaluationEngine] = {
    // TODO: not nice

    if (evaluationType == "tms") {
      if (evaluationModifier == "greedy") {
        return Some(greedyTms(program, random))
      } else if (evaluationModifier == "doyle") {
        return Some(doyleTms(program, random))
      } else if (evaluationModifier == "learn") {
        return Some(learnTms(program, random))
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

  def greedyTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = JtmsGreedy(random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def doyleTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = JtmsDoyle(random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def learnTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = new JtmsLearn(random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

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

    val optionsUsage = options.map(o => f"-${o.option} <value>")

    Console.out.println("Usage: Evaluator " + optionsUsage.mkString(" "))
    Console.err.println()

    val optionsDescription = options.
      groupBy(o => o.description).
      map(o => f"${o._1}: " + o._2.collect { case a: OptionValue => a.value })

    Console.out.println(optionsDescription.mkString("\n"))
    System.exit(-1)
  }
}

case class Evaluator(engineProvider: () => EvaluationEngine, warmups: Int = 5, repetitions: Int = 5) {

  def streamInputsAsFastAsPossible(inputs: Seq[StreamEntry]): (StatisticResult, StatisticResult) = {
    val appendExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()
    val evaluateExecutionTimes = ArrayBuffer[scala.concurrent.duration.Duration]()

    def test = {
      val engine = new TimedEvaluationEngine(engineProvider(), appendExecutionTimes, evaluateExecutionTimes)

      inputs.foreach(i => {
        engine.append(i.time)(i.atoms.toSeq: _*)

        engine.evaluate(i.time)
      })
    }

    // warmup - we need to clear execution times afterwards
    test
    appendExecutionTimes.clear()
    evaluateExecutionTimes.clear()

    profile.profileR(repetitions)(test)

    (
      StatisticResult.fromExecutionTimes(appendExecutionTimes),
      StatisticResult.fromExecutionTimes(evaluateExecutionTimes)
      )
  }
}