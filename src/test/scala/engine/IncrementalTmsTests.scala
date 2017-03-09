package engine

import core.lars._
import core.{Atom, Predicate, StringValue, StringVariable}
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

    //TODO assert next addition has count correct

  }


}
