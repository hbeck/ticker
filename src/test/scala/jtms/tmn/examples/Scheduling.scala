package jtms.tmn.examples

import core._
import jtms.TMN
import org.scalatest.FlatSpec

/**
  * Created by FM on 17.03.16.
  */
class Scheduling extends FlatSpec {

  // Example according to Doyle paper, page 252

  val Time1000 = Atom("time1000")
  val NotTime1000 = Atom("notTime1000")

  val Room813 = Atom("room813")
  val Room801 = Atom("room801")

  val contradiction = ContradictionAtom("c")

  val program = Program(
    Rule(Time1000, Set(), Set(NotTime1000)),
    //    Fact(NotTime1000),
    Rule(Room813, Set(), Set(Room801))
  )

  "The model" should "be Time1000, Room813" in {
    val tmn = TMN(program)

    assert(tmn.getModel.get == Set(Time1000, Room813))
  }

  "Adding a contradiction :- Time1000,Room813" should "lead to model notTime1000, Room813" in {
    val tmn = TMN(program)
    tmn.add(Rule(contradiction, Set(Time1000, Room813), Set()))

    //assert(tmn.getModel.get == Set(NotTime1000, Room813)) //TODO that's not a stable model!
    assert(tmn.getModel == None)
  }

  "Adding a contradiction :- notTime1000" should "lead to model time1000, Room801" in {
    val tmn = TMN(
      program +
        Rule(contradiction, Set(Time1000, Room813), Set()) +
        Rule(contradiction, Set(NotTime1000), Set())
    )

    //assert(tmn.getModel.get == Set(Time1000, Room801)) //TODO that's not a stable model!
    assert(tmn.getModel == None)
  }
}
