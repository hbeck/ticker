package fixtures

import core.lars.{LarsProgram, TimePoint}
import core.Atom
import reasoner.Reasoner
import reasoner.config.BuildReasoner
import reasoner.incremental.jtms.algorithms.Jtms
import reasoner.incremental.jtms.networks.OptimizedNetwork

import scala.util.Random

/**
  * Created by FM on 01.06.16.
  */

sealed trait EvaluationMode
object AspBasedEvaluation extends EvaluationMode
object IncrementalEvaluation extends EvaluationMode

trait ReasonerBuilder {

  //case class ReasonerBuilderConfig(evaluationType: EvaluationType, builder: ReasonerBuilder)

  type ReasonerBuilder = ((LarsProgram) => Reasoner)

  val reasonerBuilder: ReasonerBuilder

  //

  def intv(start: Int, end: Int): Set[Int] = (start to end).toSet

  def complement(upTo: Int)(s: Set[Int]) = intv(0,upTo) -- s

  def checkEntailments(program: LarsProgram, expectedEntailmentTimePoints: Map[Atom,Set[Int]], stream:Map[Int,Set[Atom]]): Unit = {

    val engine = reasonerBuilder(program)

    val maxInt:Int = expectedEntailmentTimePoints.values reduce { (l,r) => l ++ r} reduce Math.max
    println("timeline: [0,"+maxInt+"]")

    for (t <- 0 to maxInt) {

      stream.get(t) match {
        case Some(atoms) => atoms foreach (atom => engine.append(TimePoint(t))(atom))
        case None =>
      }

      val model = engine.evaluate(TimePoint(t)).model

      for (atom <- expectedEntailmentTimePoints.keys) {
        if (expectedEntailmentTimePoints(atom) contains t) {
          if (!(model contains atom)) {
            println(f"t=$t: atom ${atom} not in model ${model}")
          }
          assert(model contains atom)
        } else {
          if (model contains atom) {
            println(f"t=$t: atom ${atom} in model ${model}")
          }
          assert(!(model contains atom))
        }
      }

    }
  }

}

trait ClingoPullReasoner extends ReasonerBuilder {
  val reasonerBuilder = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePull().seal()
}

trait ClingoPushReasoner extends ReasonerBuilder {
  val reasonerBuilder = (p: LarsProgram) => BuildReasoner.withProgram(p).configure().withClingo().withDefaultEvaluationMode().usePush().seal()
}

trait JtmsIncrementalReasoner extends ReasonerBuilder {

  val reasonerBuilder = (p: LarsProgram) => {
    val jtms = Jtms(new OptimizedNetwork(), new Random(1))
    //jtms.doConsistencyCheck = false

    BuildReasoner.withProgram(p).configure().withIncremental().withJtms(jtms).use().seal()
  }
}
