package jtms.asp.examples

import asp.Asp
import core._
import jtms._
import jtms.asp.AtomValidationASP
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */

trait LibraryBehaviorASP {
  this: FlatSpec =>

  val v = Atom("verfuegbar")
  val g = Atom("gestohlen")
  val p = Atom("am_angegebenen_Platz_vorhanden")
  val f = Atom("falsch_einsortiert")
  val not_p = Atom("nicht_am_angegebenen_Platz_vorhanden")
  val a = Atom("ausleihbar")
  val n = Atom("nachschlagewerk")
  val not_a = Atom("nicht_ausleihbar")
  val h = Atom("im_Handapperart_einer_Veranstaltung")

  val bot = new ContradictionAtom("f")

  val r1 = Fact(v)
  val r2 = Rule(p,Set(v),Set(f, g))
  val r3 = Rule(not_p,f)
  val r4 = Rule(not_p,g)
  val r5 = Rule(a,Set(p),Set(h, n))
  val r6 = Rule(bot,Set(p, not_p))
  val r7 = Rule(not_a,n)
  val r8 = Rule(not_a,h)
  val r9 = Rule(bot,Set(a, not_a))

  val r10: Rule = Rule(bot,a)
  val r11: Rule = Rule(bot,p)

  val program = Program(r1, r2, r3, r4, r5, r6, r7, r8, r9)

  def library(evaluation: Evaluation) = {
    it should "be V, P, A" in {
      info("The valid model")
      assert(evaluation(program) contains Set(v, p, a))
    }
    it should "be V,H,P,A_not" in {
      info("With the fact H the model")

      val model = evaluation(program + Fact(h))

      assert(model contains Set(v, h, p, not_a))
    }

    /* the following models diverge from ASP */

    it should "be A_not,H,P, V" in {
      info("With a constraint for A the model")

      val model = evaluation(program + r10)

      if (evaluation.isInstanceOf[Asp]) pending

      info("H is currently chosen 'by random'")
      assert(model contains Set(not_a, h, p, v))
    }

    it should "be P_not,F,V" in {
      info("With a constraint for P the model")

      val model = evaluation(program + r11)

      if (evaluation.isInstanceOf[Asp]) pending

      info("F is currently chosen 'by random'")
      assert(model contains Set(not_p, f, v))
    }

  }
}

class LibraryASP extends FlatSpec with LibraryBehaviorASP with EvaluateJTMNImplementations {
  "The Library Sample" should behave like theSame(library)
}

class LibraryAtomValidationASP extends FlatSpec with AtomValidationASP with LibraryBehaviorASP {

  def Network = AnswerUpdateNetwork(program)

  "Atom V" must behave like atomValidation(Network, v) { validator =>
    validator.status(in)
    validator.rules(r1)
    validator.suppRule(Some(r1))
    validator.supp()
    validator.ancestors()
    validator.antecedents()
    validator.foundations()
    validator.cons(p)
    validator.aff(p)
    validator.repercussions(p, a)
  }

  "Atom P" must behave like atomValidation(Network, p) { validator =>
    validator.status(in)
    validator.rules(r2)
    validator.suppRule(Some(r2))
    validator.supp(v, f, g)
    validator.ancestors(v, f, g)
    validator.antecedents(v, f, g)
    validator.foundations(v, f, g)
    validator.cons(a, bot)
    validator.aff(a)
    validator.repercussions(a)
  }

  "Atom A" must behave like atomValidation(Network, a) { validator =>
    validator.status(in)
    validator.rules(r5)
    validator.suppRule(Some(r5))
    validator.supp(p, h, n)
    validator.ancestors(p, h, n, v, f, g)
    validator.antecedents(p, h, n)
    validator.foundations(p, h, n, v, f, g)
    validator.cons(bot)
    validator.aff()
    validator.repercussions()
  }

  "Atom F" must behave like atomValidation(Network, f) { validator =>
    validator.status(out)
    validator.rules()
    validator.suppRule(None)
    validator.supp()
    validator.ancestors()
    validator.antecedents()
    validator.foundations()
    validator.cons(p, not_p)
    validator.aff(p, not_p)
    validator.repercussions(p, a, not_p, bot)
  }
  "Atom G" must behave like atomValidation(Network, g) { validator =>
    validator.status(out)
    validator.rules()
    validator.suppRule(None)
    validator.supp()
    validator.ancestors()
    validator.antecedents()
    validator.foundations()
    validator.cons(p, not_p)
    validator.aff(p, not_p)
    validator.repercussions(p, a, not_p, bot)
  }


  "Atom H" must behave like atomValidation(Network, h) { validator =>
    validator.status(out)
    validator.rules()
    validator.suppRule(None)
    validator.supp()
    validator.ancestors()
    validator.antecedents()
    validator.foundations()
    validator.cons(a, not_a)
    validator.aff(a, not_a)
    validator.repercussions(a, not_a, bot)
  }
  "Atom N" must behave like atomValidation(Network, n) { validator =>
    validator.status(out)
    validator.rules()
    validator.suppRule(None)
    validator.supp()
    validator.ancestors()
    validator.antecedents()
    validator.foundations()
    validator.cons(a, not_a)
    validator.aff(a, not_a)
    validator.repercussions(a, not_a, bot)
  }

  "Atom P_not" must behave like atomValidation(Network, not_p) { validator =>
    validator.status(out)
    validator.suppRule(None)
    validator.rules(r3, r4)
    validator.supp(f, g)
    validator.ancestors(f, g)
    validator.antecedents()
    validator.foundations()
    validator.cons(bot)
    validator.aff(bot)
    validator.repercussions(bot)
  }

  "Atom A_not" must behave like atomValidation(Network, not_a) { validator =>
    validator.status(out)
    validator.suppRule(None)
    validator.rules(r8, r7)
    validator.supp(h, n)
    validator.ancestors(h, n)
    validator.antecedents()
    validator.foundations()
    validator.cons(bot)
    validator.aff(bot)
    validator.repercussions(bot)
  }

  "Atom N_cont" must behave like atomValidation(Network, bot) { validator =>
    validator.status(out)
    validator.suppRule(None)
    validator.rules(r6, r9)
    validator.supp(not_p, not_a)
    validator.ancestors(not_p, not_a, f, g, h, n)
    validator.antecedents()
    validator.foundations()
    validator.cons()
    validator.aff()
    validator.repercussions()
  }
}
