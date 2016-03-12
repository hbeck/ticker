package jtms.tmn.examples

import core._
import jtms._
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Car extends FlatSpec {

  val S_not = Atom("car is not starting")
  val G_not = Atom("not enough gas")
  val G = Atom("enough gas")
  val D = Atom("defect")
  val I = Atom("ignition broken")
  val C = Atom("carb broken")

  val N_cont = ContradictionAtom("contradiction")

  val j0 = Rule.pos(S_not).neg(D).head(G_not);
  val j1 = Rule.pos(S_not, G).head(D)
  val j2 = Rule.pos(G, G_not).head(N_cont)
  val j3 = Rule.pos(I).head(D)
  val j4 = Rule.pos(C).head(D)

  val notStarting = Fact(S_not)
  val enoughGas = Fact(G)
  val notEnoughGas = Fact(G_not)
  val brokenIgnition = Fact(C)

  val program = Program(j0, j1, j2, j3, j4)

  def Tmn = {
    val t = TMN(program)

    t
  }

  "When the car is not starting and there is not enough gas" should "not result in a defect" in {
    val tmn = Tmn

    tmn.add(notStarting)
    tmn.add(notEnoughGas)

    val model = tmn.model()

    assert(model.contains(D) == false)
  }

  "When the car is not starting and there is enough gas" should "result in a defect" in {
    val tmn = Tmn

    tmn.add(notStarting)
    tmn.add(enoughGas)

    val model = tmn.model().get

    assert(model.contains(D))
  }

  "When the car is not starting and there is no gas information" should "result in not enough gas" in {
    val tmn = Tmn

    tmn.add(notStarting)

    val model = tmn.model().get

    assert(model.contains(G_not))
  }

  "When the car is not starting and there is a broken ignition" should "result in a defect" in {
    val tmn = Tmn

    tmn.add(notStarting)
    tmn.add(brokenIgnition)

    val model = tmn.model().get

    assert(model.contains(D))
  }

  "When the car is not starting and there is a broken ignition and enogh gas" should "result in a defect" in {
    val tmn = Tmn

    tmn.add(notStarting)
    tmn.add(enoughGas)
    tmn.add(brokenIgnition)

    val model = tmn.model().get

    assert(model.contains(D))
  }
}
