package engine

import core.lars._
import core._
import fixtures.JtmsIncrementalEngine
import org.scalatest.FunSuite

/**
  * Created by hb on 02.03.17.
  */
class IncrementalTmsTests extends FunSuite with JtmsIncrementalEngine {

  test("test 1") {

    val b = Atom(Predicate("b"),Seq(StringVariable("X")))
    val h = Atom(Predicate("h"),Seq(StringVariable("X")))

    val program = LarsProgram.from(
      h <= WindowAtom(SlidingTimeWindow(2), Diamond, b)
    )

    val engine = defaultEngine(program)

    var model = engine.evaluate(TimePoint(0)).model
    assert(model.size == 1) //

    val signal = Atom(Predicate("b"),Seq(StringValue("y")))

    engine.append(TimePoint(0))(signal)

    model = engine.evaluate(TimePoint(0)).model
    assert(model contains signal)

    val inference = Atom(Predicate("h"),Seq(StringValue("y")))

    println("evaluate 0")
    model = engine.evaluate(TimePoint(0)).model
    assert(model contains inference)

    println("evaluate 1")
    model = engine.evaluate(TimePoint(1)).model
    assert(model contains inference)

    println("evaluate 2")
    model = engine.evaluate(TimePoint(2)).model
    assert(model contains inference)

    model = engine.evaluate(TimePoint(3)).model
    assert(!(model contains inference))

    //additional
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("1")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("2")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("0")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("1")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("2")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("3")))))

    engine.append(TimePoint(3))(signal)
    model = engine.evaluate(TimePoint(3)).model

    assert(model contains inference)
    assert(model contains Atom(Predicate("b"),Seq(StringValue("y"))))
    val bat3 = Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))
    var found = false
    for (a <- model) {
      if (a equals bat3) {
        found = true
        //println("hash in set:    "+a.hashCode)
        //println("hash of manual: "+bat3.hashCode)
      }

    }
    assert(found)
    assert(model contains bat3)
    assert(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

    model = engine.evaluate(TimePoint(5)).model

    assert(model contains inference)
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

    model = engine.evaluate(TimePoint(6)).model

    assert(!(model contains inference))
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

  }

  test("basic propositional") {
    val p = Atom(Predicate("p"))
    val q1 = Atom(Predicate("q1"))
    val q2 = Atom(Predicate("q2"))
    val q3 = Atom(Predicate("q3"))
    val q4 = Atom(Predicate("q4"))
    val q5 = Atom(Predicate("q5"))
    val q6 = Atom(Predicate("q6"))
    val q7 = Atom(Predicate("q7"))
    val q8 = Atom(Predicate("q8"))
    val q10 = Atom(Predicate("q10"))
    val q11 = Atom(Predicate("q11"))
    val qn1 = Atom(Predicate("qn1"))
    val qn2 = Atom(Predicate("qn2"))
    val qn3 = Atom(Predicate("qn3"))
    //val qn4 = Atom(Predicate("qn4"))
    val qn5 = Atom(Predicate("qn5"))
    val qn6 = Atom(Predicate("qn6"))
    val qn7 = Atom(Predicate("qn7"))
    //val qn8 = Atom(Predicate("qn8"))
    val qn10 = Atom(Predicate("qn10"))
    //val qn11 = Atom(Predicate("qn11"))
    val T = TimeVariableWithOffset("T")

    val heads = Seq(q1,q2,q3,q4,q5,q6,q7,q8,q10,q11,qn1,qn2,qn3,qn5,qn6,qn7,qn10)
    val atoms = Seq(p)++heads

    val program = LarsProgram.from(
      q1 <= WindowAtom(SlidingTimeWindow(10), Diamond, p),
      q2 <= WindowAtom(SlidingTimeWindow(10), Box, p),
      q3 <= WindowAtom(SlidingTimeWindow(10), At(3), p),
      q4 <= WindowAtom(SlidingTimeWindow(10), At(T), p),
      q5 <= WindowAtom(SlidingTupleWindow(10), Diamond, p),
      q6 <= WindowAtom(SlidingTupleWindow(10), Box, p),
      q7 <= WindowAtom(SlidingTupleWindow(10), At(3), p),
      q8 <= WindowAtom(SlidingTupleWindow(10), At(T), p),
      q10 <= p,
      q11 <= AtAtom(T,p),
      qn1 <= not(WindowAtom(SlidingTimeWindow(10), Box, p)),
      qn2 <= not(WindowAtom(SlidingTimeWindow(10), Diamond, p)),
      qn3 <= not(WindowAtom(SlidingTimeWindow(10), At(3), p)),
      //qn4 <= not(WindowAtom(SlidingTimeWindow(10), At(T), p)), //grounding limitation
      qn5 <= not(WindowAtom(SlidingTupleWindow(10), Box, p)),
      qn6 <= not(WindowAtom(SlidingTupleWindow(10), Diamond, p)),
      qn7 <= not(WindowAtom(SlidingTupleWindow(10), At(3), p)),
      //qn8 <= not(WindowAtom(SlidingTupleWindow(10), At(T), p)), //grounding limitation
      UserDefinedLarsRule(qn10,Set(),Set(p))
      //UserDefinedLarsRule(qn11,Set(),Set(AtAtom(T,p))) //grounding limitation
    )

    val expectModelAtTimes:Map[Atom,Set[Int]] = Map(
      p -> Set(3),
      q1 -> (3 to 13).toSet,
      q2 -> Set(),
      q3 -> (3 to 13).toSet,
      q4 -> (3 to 13).toSet,
      q5 -> (3 to 20).toSet,
      q6 -> Set(),
      q7 -> (3 to 20).toSet,
      q8 -> (3 to 20).toSet,
      q10 -> Set(3),
      q11 -> (3 to 20).toSet,
      qn1 -> (0 to 20).toSet,
      qn2 -> ((0 to 2).toSet ++ (14 to 20).toSet),
      qn3 -> ((0 to 2).toSet ++ (14 to 20).toSet),
      qn5 -> (0 to 20).toSet,
      qn6 -> (0 to 2).toSet,
      qn7 -> (0 to 2).toSet,
      qn10 -> ((0 to 2).toSet ++ (4 to 20).toSet)
    )

    val engine = defaultEngine(program)

    for (t <- 0 to 20) {
      if (t==3) {
        engine.append(TimePoint(t))(p)
      }

      val model = engine.evaluate(TimePoint(t)).model

      for (a <- atoms) {
        if (expectModelAtTimes(a) contains t) {
          assert(model contains a)
        } else {
          assert(!(model contains a))
        }
      }

    }

    /* t=3
    assert(model contains p)
    assert(model contains q1)
    assert(!(model contains q2))
    assert(model contains q3)
    assert(model contains q4)
    assert(model contains q5)
    assert(model contains q7)
    assert(model contains q8)
    assert(model contains q10)
    assert(model contains q11)
    assert(!(model contains q6))
    */


  }


}
