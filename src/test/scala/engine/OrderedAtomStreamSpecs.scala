package engine

import core.Atom
import org.scalatest.FlatSpec

/**
  * Created by FM on 08.04.16.
  */
class OrderedAtomStreamSpecs extends FlatSpec {

  val atom = Atom("a")

  val t0 = Time(0)
  val t1 = Time(1)
  val t2 = Time(2)

  def stream = {
    new OrderedAtomStream
  }

  "An empty engine" should "not evaluate to a result at t0" in {
    val engine = stream
    assert(engine.evaluate(t0) == Set())
  }

  "Appending atom a at t1" should "allow it to be queried at t1" in {
    val engine = stream

    engine.append(t1)(Set(atom))

    assert(engine.evaluate(t1) == Set(atom))
  }

  it should "not be queried at t0" in {
    val engine = stream


    engine.append(t1)(Set(atom))

    assert(engine.evaluate(t0) == Set())
  }

  it should "not be available at t2" in {
    val engine = stream

    engine.append(t1)(Set(atom))

    assert(engine.evaluate(t2) == Set())
  }

  "Adding to atoms after each other" should "result in only atom at t1" in {
    val engine = stream

    engine.append(t0)(Set(atom))

    val atom2 = Atom("b")

    engine.append(t1)(Set(atom2))

    assert(engine.evaluate(t1) == Set(atom2))
  }

  "Adding two atoms at the same time point" should "allow both to be queried" in {
    val engine = stream
    val atT1 = engine.append(t1) _

    atT1(Set(atom))

    val atom2 = Atom("b")

    atT1(Set(atom2))

    assert(engine.evaluate(t1) == Set(atom, atom2))
  }

  "On an empty stream, evaluateUntil at t1" should "return no results" in {
    val s = stream

    assert(s.evaluateUntil(t1) == Set())
  }

  "A stream with one entry at t1" should "be returned as single result" in {
    val s = stream

    s.append(t0)(Set(atom))

    assert(s.evaluateUntil(t1) == Set(Evaluation(t0, Set(atom))))
  }

  "A stream with one entry at t0 and one t1" should "return both with their timestamps" in {
    val s = stream

    s.append(t0)(Set(atom))

    val b = Atom("b")
    s.append(t1)(Set(b))

    assert(s.evaluateUntil(t1) == Set(Evaluation(t0, Set(atom)), Evaluation(t1, Set(b))))
  }
}
