package jtms.graph

import core.Atom
import jtms.{in, TMN}
import jtms.TMN.Label

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphTraversal.DepthFirst

//import scalax.collection.GraphTraversal.Cycle
import scalax.collection.immutable.Graph

/**
  * Created by FM on 11.03.16.
  */
object TmnGraph {
  def apply(tmn: TMN) = new TmnGraph(tmn)

}

class TmnGraph(tmn: TMN) {

  def forSupport = {
    scalax.collection.immutable.Graph.from(tmn.Supp.keys, tmn.Supp.flatMap(s => s._2.map(DiEdge(s._1, _))))
  }

  def forLabel = {
    val graph = Graph.from(tmn.SuppWithStatus.keys, tmn.SuppWithStatus.flatMap(s => s._2.map(DiEdge(s._1, _))))

    new LabelGraph(graph)
  }
}

//.filter(_._2 == in)
class LabelGraph(val graph: Graph[Label, DiEdge]) {
  def nodes = graph.nodes

  def edges = graph.edges

  def isCyclic = graph.isCyclic

  def findCycle = graph.findCycle

  def findPositiveCycle(atom: Atom) = {
    (graph get(atom, in)).withSubgraph(nodes = _._2 == in).findCycle(_.withKind(DepthFirst))
  }
}