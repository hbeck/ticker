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
      h <= WindowAtom(SlidingTimeWindow(3), Diamond, b)
    )

    val engine = defaultEngine(program)

    var model = engine.evaluate(TimePoint(0)).get
    assert(model.isEmpty)

    val signal = Atom(Predicate("b"),Seq(StringValue("y")))

    engine.append(TimePoint(0))(signal)

    model = engine.evaluate(TimePoint(0)).get
    assert(model contains signal)

    val inference = Atom(Predicate("h"),Seq(StringValue("y")))

    for (t <- 0 to 3) {
      print("evaluate "+t)
      model = engine.evaluate(TimePoint(t)).get
      assert(model contains inference)
    }

    model = engine.evaluate(TimePoint(4)).get
    assert(model.isEmpty)

  }


}
