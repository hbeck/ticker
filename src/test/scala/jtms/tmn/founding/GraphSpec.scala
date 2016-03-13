package jtms.tmn.founding

import core.{Rule, Program, Fact, Atom}
import jtms.TMN.Label
import jtms.{Status, out, in, TMN}
import jtms.graph.TmnGraph

import scala.collection
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.Graph

import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import language.implicitConversions

import org.scalatest.FlatSpec

/**
  * Created by FM on 11.03.16.
  */
class GraphSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")

  def factGraph = {
    TmnGraph(TMN(Program(Fact(a))))
  }

  "A program with only one fact" should "generate a Graph containing the atom" in {
    assert(factGraph.forSupport.nodes == Set(a))
  }
  it should "contain no edges" in {
    assert(factGraph.forSupport.edges.isEmpty)
  }

  def oneRulePosGraph = {
    val program = Program(Fact(a), Rule.pos(a).head(b))

    TmnGraph(TMN(program))
  }

  "The graph for a program with one fact and one rule" should "contain two nodes" in {
    assert(oneRulePosGraph.forSupport.nodes == Set(a, b))
  }

  it should "contain one edge" in {
    val edges = oneRulePosGraph.forSupport.edges
    val edge = b ~> a

    assert(edges == Set(edge))
  }

  it should "contain the Labels (a, in), (b, in)" in {
    assert(oneRulePosGraph.forLabel.nodes == Set((a, in), (b, in)))
  }
  it should "contain the edges (b, in) ~> (a, in)" in {
    assert(oneRulePosGraph.forLabel.edges == Set((b, in) ~>(a, in)))
  }

  def oneRuleNegGraph = {
    val program = Program(
      Rule.neg(a).head(b)
    )

    TmnGraph(TMN(program))
  }

  "A program with the rule b :- a" should "contain the labels (a, out), (b, in)" in {
    assert(oneRuleNegGraph.forLabel.nodes == Set((a, out), (b, in)))
  }
  it should "contain the edge (a, out) ~> (b,in)" in {
    assert(oneRuleNegGraph.forLabel.edges == Set((b, in) ~>(a, out)))
  }

  def posCycleGraph = {
    val program = Program(
      Rule.pos(a).head(b),
      Rule.pos(b).head(a)
    )

    TmnGraph(TMN(program))
  }

  "A program with a positive cycle" should "contain only atoms with status out" in {
    val nodes = posCycleGraph.forLabel.nodes

    assert(nodes == Set((a, out), (b, out)))
    //    assert(nodes forall(_._2 == out)) TODO: check why this doesn't compile
  }
  it should "contain two edges containing only out-Atoms" in {
    val edges = posCycleGraph.forLabel.edges

    assert(edges.size == 2)
    assert(edges == Set((b, out) ~>(a, out), (a, out) ~>(b, out)))
  }
  it should "detect a cycle" in {
    val graph = posCycleGraph.forLabel
    val cycle = graph.findCycle

    assert(cycle.isDefined)
    assert(cycle.get.nodes.toSet == Set((b, out), (a, out)))
  }

  def negCycleGraph = {
    val program = Program(
      Rule.neg(a).head(b),
      Rule.neg(b).head(a)
    )

    TmnGraph(TMN(program))
  }

  "A program with a negative cycle" should "contain one in and one out atom" in {
    val nodes = negCycleGraph.forLabel.nodes

    assert(nodes == Set((a, out), (b, in)))
  }
  it should "contain no cycle" in {
    val graph = negCycleGraph.forLabel

    val cycle = graph findPositiveCycle b

    assert(cycle.isEmpty)
  }
}
