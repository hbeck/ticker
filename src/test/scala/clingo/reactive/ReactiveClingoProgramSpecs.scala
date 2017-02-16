package clingo.reactive


import core.lars.{Diamond, LarsProgram, SlidingTimeWindow, WindowAtom}
import core.{Atom, Predicate, Variable}
import engine.asp.PlainLarsToAspMapper
import org.scalatest.FlatSpec
import org.scalatest.Inspectors._
import org.scalatest.Matchers._

/**
  * Created by fm on 26/01/2017.
  */
class ReactiveClingoProgramSpecs extends FlatSpec {

  val emptyProgram = ReactiveClingoProgram(Set(), Set())

  "A program without signals or volatile rules" should "have no #external signals" in {
    assert(emptyProgram.signalPrograms.isEmpty)
  }

  it should "have only external constraints" in {
    pending
    val externalLines = emptyProgram.program.lines.
      filter(_.startsWith("#external")).
      toSeq

    forExactly(1, externalLines) { p => assert(p.contains("now(t)")) }
    forExactly(1, externalLines) { p => assert(p.contains("cnt(c)")) }

    assert(externalLines.size == 2)
  }

  it should "contain no lines without #" in {
    val lines = emptyProgram.program.lines.filterNot(_.trim.isEmpty).toSeq
    forAll(lines) { line => line should startWith("#") }
  }

  "A program with a signal" should "have one external entry" in {
    val s = ReactiveClingoProgram(Set(), Set(ClingoSignal(Predicate("b"))))

    assert(s.program.contains("#external b_at(t)."))
    assert(s.program.contains("#external b_cnt(c)."))
  }
  it should "have a program named signals_b" in {
    val s = ReactiveClingoProgram(Set(), Set(ClingoSignal(Predicate("b"))))

    assert(s.program.contains("#program signals_b_0"))
  }

  "A program with a signal with arity 1" should "have a program with additional parameters" in {
    val atom = Atom(Predicate("b"), Seq(Variable("arg")))
    val s = ReactiveClingoProgram(Set(), Set(ClingoSignal.fromAtom(atom)))

    assert(s.program.contains("#program signals_b_1"))
    assert(s.program.contains(", b_arg)"))
  }

  "A program with a volatile rule" should "have one rule entry" in {
    val s = ReactiveClingoProgram(Set("a :- b."), Set())

    assert(s.program.contains("a :- b."))
  }

  "A program with a rule and a signal" should "have both entries" in {
    val s = ReactiveClingoProgram(Set("a :- b."), Set(ClingoSignal.fromAtom(Atom("b"))))

    assert(s.program.contains("at_b(t)"))
    assert(s.program.contains("a :- b"))
  }


  "The program a :- wˆ2 d b" should "be mapped correctly" in {
    val p = LarsProgram.from(
      Atom("a") <= WindowAtom(SlidingTimeWindow(2), Diamond, Atom("b"))
    )
    val mapper = PlainLarsToAspMapper()

    val mappedProgram = mapper.apply(p)

    val clingoProgram = ReactiveClingoProgram.fromMapped(mappedProgram)

    assert(clingoProgram.signals.size == 1)
  }

  "Clingo Signals from Atoms with one different variable argument" should "be constructed by index position and be the same" in {
    val a = Atom("a")
    val v = Variable("v")
    val w = Variable("w")
    val signal_v = ClingoSignal.fromAtom(a(v))
    val signal_w = ClingoSignal.fromAtom(a(w))

    assert(signal_v == signal_w)
  }
}
