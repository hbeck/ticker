package jtms.tmn.founding

import core._
import jtms.{out, in, TMN}
import jtms.graph.{DependencyGraph}
import org.scalatest.FlatSpec

/**
  * Created by FM on 13.03.16.
  */
class XYZSample extends FlatSpec {

  //  x :- y. y :- x, not z.

  val x = Atom("x")
  val y = Atom("y")
  val z = Atom("z")

  val program = Program(
    Rule.pos(y).head(x),
    Rule.pos(x).neg(z).head(y)
  )

  val tmn = TMN(program)

  "The program" should "have no model" in {
    assert(tmn.getModel == None)
  }

  "The dependency-graph" should "have a cycle" in {
    val graph = DependencyGraph(tmn)

    val cycle = graph.forLabel.findCycle

    assert(cycle.isDefined)
  }

  it should "contain only out nodes" in {
    val graph = DependencyGraph(tmn).forLabel

    val cycle = graph.findCycle

    assert(cycle.get.nodes.forall(_._2 == out))
  }

  it should "find no positive cycle for x" in {
    val graph = DependencyGraph(tmn).forLabel

    val cycle = graph.findPositiveCycle(x)

    assert(cycle == None)
  }
}
