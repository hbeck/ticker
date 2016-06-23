package evaluation.reachBlocked

import core.lars.{LarsFact, LarsProgram, LarsRule}
import core.{Atom, Ground, Value}
import fixtures.{ConfigurableEvaluationSpec, TimeTestFixtures, TmsPushEngine}
import org.scalatest.Matchers._

/**
  * Created by FM on 20.06.16.
  */
class ParallelLanesSample extends ConfigurableEvaluationSpec with ReachBlockedProgram with TimeTestFixtures with TmsPushEngine {
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

  val paths = 1
  val nodes = 1

  val program = {
    val C: Seq[(Option[Value], Atom)] = (1 to paths) flatMap (p => {
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

    var p = Set[LarsRule]()

    val nodeNames = C.filter(_._1.isDefined).map(_._1.get) union Seq(startNode, endNode)

    val generateUseLessRules = false
    for (x <- nodeNames) {
      for (y <- nodeNames) {
        for (z <- nodeNames) {
          val g = Ground(Map(X -> x, Y -> y, Z -> z))
          p = p ++ (baseProgram.rules filter (r => generateUseLessRules || (r == reach_X_Z && x != z) || r != reach_X_Z && x != y) map (g.apply))
        }
      }
    }

    val edges: Seq[LarsRule] = C map (_._2) map (a => LarsFact(a))


    LarsProgram(p.toSeq union edges)
  }

  "Unblocked paths" should "lead to reach(a,b) at t0" in {
    evaluationEngine.evaluate(t0).get.get should contain(reach_a_b)
  }

}
