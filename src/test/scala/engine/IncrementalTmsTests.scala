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
    assert(model.isEmpty)

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
    assert(model contains Atom(Predicate("b_cnt"),Seq(StringValue("y"),StringValue("2"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

    model = engine.evaluate(TimePoint(5)).model

    assert(model contains inference)
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3"))))
    assert(model contains Atom(Predicate("b_cnt"),Seq(StringValue("y"),StringValue("2"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

    model = engine.evaluate(TimePoint(6)).model

    assert(!(model contains inference))
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3"))))
    assert(model contains Atom(Predicate("b_cnt"),Seq(StringValue("y"),StringValue("2"))))
    assert(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2"))))

  }


}
