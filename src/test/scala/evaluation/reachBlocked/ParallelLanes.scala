package evaluation.reachBlocked

import core.grounding.LarsGrounding
import core.lars.{LarsFact, _}
import core.{Atom, Ground, Value}

/**
  * Created by FM on 11.07.16.
  */
trait ParallelLanes extends ReachBlockedProgram {

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

  var edgeAndAtoms: Seq[EdgeAtomMap] = Seq()
  var edges: Seq[LarsRule] = Seq()
  var generatedNodes: Seq[Value] = Seq()
  var availableNodes: Seq[Value] = Seq()


  var lanes = 0
  var nodes = 0

  def generateProgram(nodes: Int, lanes: Int, generateRedundantRules: Boolean = false) = {

    generateEdges(nodes, lanes)

    var p = Set[LarsRule]()

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

  private def generateEdges(nodes: Int, lanes: Int): Unit = {
    this.lanes = lanes
    this.nodes = nodes

    // TODO: as dictionary {path -> Seq[EdgeNodeMap]}
    edgeAndAtoms = (1 to lanes) flatMap (p => {
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
    generatedNodes = edgeAndAtoms.filter(_._1.isDefined).map(_._1.get)
    availableNodes = generatedNodes union Seq(startNode, endNode)
    edges = edgeAndAtoms map (_._2) map (a => LarsFact(a))
  }

  def generateProgramWithGrounding(nodes: Int, lanes: Int) = {
    generateEdges(nodes, lanes)

    val program = LarsProgram(baseProgram.rules ++ edges)

    val grounder = LarsGrounding(program)

    grounder.groundProgram

  }

  def nodesAt(lane: Int) = generatedNodes.drop(lane * nodes).take(nodes)
}
