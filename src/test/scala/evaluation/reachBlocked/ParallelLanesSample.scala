package evaluation.reachBlocked

import core.lars.{LarsFact, LarsProgram, LarsRule}
import core.{Atom, Ground, Value}
import evaluation.RunWithAllImplementations
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsPushEngine}
import org.scalatest.Matchers._

/**
  * Created by FM on 20.06.16.
  */
class ParallelLanesSample extends ConfigurableEvaluationSpec with ReachBlockedProgram with TimeTestFixtures with TmsPushEngine {

  type EdgeAtomMap = (Option[Value], Atom)

  /**
    * ** n_1_1      n_i_1  n_1_n
    *
    * / -- . -- . -- . -- . -- \      path 1
    * /                          \
    * a --- . ---. -- . -- . --- b    path i
    * \  n_i_1     n_i_i  n_i_n /
    * \ -- . -- . -- . -- . -- /      path n
    *
    * ** n_n_1     n_i_i  n_n_n
    *
    * * Node 1   Node i Node n
    */

  val startNode = Value("a")
  val endNode = Value("b")

  val reach_a_b = reach(startNode, endNode)

  val paths = 2
  val nodes = 1

  // TODO: as dictionary {path -> Seq[EdgeNodeMap]}
  val edgeAndAtoms: Seq[EdgeAtomMap] = (1 to paths) flatMap (p => {
    (0 to nodes) map (n => {
      val nodeName = Value(f"n_${p}_$n")
      val nextNode = Value(f"n_${p}_${n + 1}")
      n match {
        case 0 => (None, edge(startNode, nextNode))
        case `nodes` => (Some(nodeName), edge(nodeName, endNode))
        case _ => (Some(nodeName), edge(nodeName, nextNode))
      }
    })
  })

  val generatedNodes = edgeAndAtoms.filter(_._1.isDefined).map(_._1.get)
  val availableNodes = generatedNodes union Seq(startNode, endNode)
  val edges: Seq[LarsRule] = edgeAndAtoms map (_._2) map (a => LarsFact(a))

  val program = {
    var p = Set[LarsRule]()

    val generateRedundantRules = false

    for (x <- availableNodes) {
      for (y <- availableNodes) {
        for (z <- availableNodes) {
          val g = Ground(Map(X -> x, Y -> y, Z -> z))
          p = p ++ (baseProgram.rules filter (r => generateRedundantRules || (r == reach_X_Z && x != z) || r != reach_X_Z && x != y) map (g.apply))
        }
      }
    }

    LarsProgram(p.toSeq union edges)
  }

  "Unblocked paths" should "lead to reach(a,b) at t0" in {
    evaluationEngine.evaluate(t0).get.get should contain(reach_a_b)
  }

  "A single obstacle at one path at t0" should "still lead to reach(a,b) at t0...t5" in {
    evaluationEngine.append(t0)(obstacle(generatedNodes.head))

    evaluationEngine.evaluate(t0).get.get should contain(reach_a_b)
  }

  "Obstacles at all paths at t0" should "not lead to reach(a,b) at t0" in {
    val atT0 = evaluationEngine.append(t0) _

    (0 to paths - 1) foreach (p => atT0(Seq(obstacle(generatedNodes.drop(p * nodes).head))))

    evaluationEngine.evaluate(t0).get.get should not contain (reach_a_b)
  }

  "Obstacles at all paths at t0" should "lead to reach(a,b) at t6" in {
    val atT0 = evaluationEngine.append(t0) _

    (0 to paths - 1) foreach (p => atT0(Seq(obstacle(generatedNodes.drop(p * nodes).head))))

    evaluationEngine.evaluate(6).get.get should contain(reach_a_b)
  }
}


class AllParallelLanes extends RunWithAllImplementations(new ParallelLanesSample)