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
object DependencyGraph {
  def apply(tmn: TMN) = new DependencyGraph(tmn)

}

class DependencyGraph(tmn: TMN) {

  def forSupport = {
    scalax.collection.immutable.Graph.from(tmn.Supp.keys, tmn.Supp.flatMap(s => s._2.map(DiEdge(s._1, _))))
  }

  def forLabel = {
    val graph = Graph.from(tmn.SuppWithStatus.keys, tmn.SuppWithStatus.flatMap(s => s._2.map(DiEdge(s._1, _))))

    new LabelGraph(graph)
  }
}

class LabelGraph(val graph: Graph[Label, DiEdge]) {
  def nodes = graph.nodes

  def edges = graph.edges

  def isCyclic = graph.isCyclic

  def findCycle = graph.findCycle

  def findPositiveCycle(atom: Atom) = graph find(atom, in) match {
    case Some(node) => node.withSubgraph(nodes = _._2 == in).findCycle(_.withKind(DepthFirst))
    case None => None
  }

}