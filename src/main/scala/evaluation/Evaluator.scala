package evaluation

import core.Atom
import core.lars.{Duration, LarsProgram, TimePoint}
import engine.EvaluationEngine
import engine.asp.tms.policies.LazyRemovePolicy
import engine.config.BuildEngine
import jtms.JtmsGreedy

import scala.collection.mutable.ArrayBuffer
//import scala.reflect.runtime.universe
//import scala.reflect.runtime._
import scala.util.Random

object Evaluator {


  def main(args: Array[String]): Unit = {
    if (args.length != 3) {
      printUsageAndExit("Supply the correct arguments");
    }
    val program = loadProgram(args(2))

    val evaluationType = args(0)
    val evaluationModifier = args(1)

    val engine = buildEngine(program, evaluationType, evaluationModifier)


    // feed data
    engine.append(1)(Atom("a"))
  }

  def loadProgram(programIdentifier: String): LarsProgram = {

    //    var classLoader = new java.net.URLClassLoader(
    //      Array(new File("module.jar").toURI.toURL),
    //      this.getClass.getClassLoader)
//    val mirror = universe.runtimeMirror(getClass.getClassLoader)
//    val cls = mirror.classSymbol(Class.forName(programIdentifier))
//    val module = cls.companion.asModule
//    val i = mirror.reflectModule(module).instance
    //    val c = cm.classLoader.loadClass(programIdentifier).asInstanceOf[ProgramProvider]

    //    c.program
//    i.asInstanceOf[ {val program: LarsProgram}].program
    return null
  }


  def buildEngine(program: LarsProgram, evaluationType: String, evaluationModifier: String): EvaluationEngine = {
    if (evaluationType == "tms") {
      if (evaluationModifier == "greedy") {
        return greedyTms(program)
      }
    }

    printUsageAndExit("wrong combination of evaluation-type/modifier specified")
    // TODO: not nice
    return null

  }

  def greedyTms(program: LarsProgram) = {
    val tms = JtmsGreedy(new Random(1))
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }


  def printUsageAndExit(exitMessage: String) = {
    Console.err.println(exitMessage)
    Console.err.println()

    Console.out.println("Usage: Evaluator <evaluation-type> <evaluation-modifier> <input-file>")
    Console.err.println()
    Console.out.println("evaluation-type: asp or tms")
    Console.out.println("evaluation-modifier: greedy or doyle")
    Console.out.println("   asp: pull or push")
    Console.out.println("   tms: greedy or doyle")
    System.exit(-1)
  }
}

case class Evaluator2(engineProvider: () => EvaluationEngine, warmups: Int = 5, repetitions: Int = 5) {

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