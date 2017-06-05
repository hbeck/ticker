package engine

import core.{Atom, GroundAtom}
import fixtures.TimeTestFixtures
import org.scalatest.FlatSpec

/**
  * Created by FM on 08.04.16.
  */
class OrderedAtomStreamSpecs extends FlatSpec with TimeTestFixtures {

  val atom = Atom("a").asInstanceOf[GroundAtom]

  def stream = {
    new SignalTracker(100, 100, DefaultTrackedSignal.apply)
  }

  "An empty engine" should "not evaluate to a result at t0" in {
    val engine = stream
    assert(engine.allTimePoints(t0) == Seq())
  }

  "Appending atom a at t1" should "allow it to be queried at t1" in {
    val engine = stream

    engine.track(t1, Seq(atom))

    assert(engine.allTimePoints(t1).map(_.signal) == Seq(atom))
  }

  it should "not be queried at t0" in {
    val engine = stream


    engine.track(t1, Seq(atom))

    assert(engine.allTimePoints(t0) == Seq())
  }

  it should "be available at t2" in {
    val engine = stream

    engine.track(t1, Seq(atom))

    assert(engine.allTimePoints(t2).map(_.signal) == Seq(atom))
  }

  "Adding to atoms after each other" should "result in both atoms at t1" in {
    val engine = stream

    engine.track(t0, Seq(atom))

    val atom2 = Atom("b")

    engine.track(t1, Seq(atom2))

    assert(engine.allTimePoints(t1).map(_.signal) == Seq(atom, atom2))
  }

  "Adding two atoms at the same time point" should "allow both to be queried" in {
    val engine = stream


    engine.track(t1, Seq(atom))

    val atom2 = Atom("b")

    engine.track(t1, Seq(atom2))

    assert(engine.allTimePoints(t1).map(_.signal) == Seq(atom, atom2))
  }

  "On an empty stream, evaluateUntil at t1" should "return no results" in {
    val s = stream

    assert(s.allTimePoints(t1) == Seq())
  }

  "A stream with one entry at t1" should "be returned as single result" in {
    val s = stream

    s.track(t0, Seq(atom))

    assert(s.allTimePoints(t1) == Seq(DefaultTrackedSignal(atom, t0, 1)))
  }

  "A stream with one entry at t0 and one t1" should "return both with their timestamps" in {
    val s = stream

    s.track(t0, Seq(atom))

    val b = Atom("b")
    s.track(t1, Seq(b))

    assert(s.allTimePoints(t1) == Seq(
      DefaultTrackedSignal(atom, t0, 1),
      DefaultTrackedSignal(b.asInstanceOf[GroundAtom], t1, 2))
    )
  }
}
