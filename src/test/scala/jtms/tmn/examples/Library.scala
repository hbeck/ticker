package jtms.tmn.examples

import asp.Asp
import aspsamples.EvaluateBothImplementations
import core._
import jtms._
import jtms.tmn.AtomValidation
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */

trait LibraryBehavior {
  this: FlatSpec =>

  val V = Atom("verfuegbar")
  val G = Atom("gestohlen")
  val P = Atom("am_angegebenen_Platz_vorhanden")
  val F = Atom("falsch_einsortiert")
  val P_not = Atom("nicht_am_angegebenen_Platz_vorhanden")
  val A = Atom("ausleihbar")
  val N = Atom("nachschlagewerk")
  val A_not = Atom("nicht_ausleihbar")
  val H = Atom("im_Handapperart_einer_Veranstaltung")

  val Falsum = new ContradictionAtom("f")

  val j1 = Fact(V)
  val j2 = Rule.pos(V).neg(F, G).head(P)
  val j3 = Rule.pos(F).head(P_not)
  val j4 = Rule.pos(G).head(P_not)
  val j5 = Rule.pos(P).neg(H, N).head(A)
  val j6 = Rule.pos(P, P_not).head(Falsum)
  val j7 = Rule.pos(N).head(A_not)
  val j8 = Rule.pos(H).head(A_not)
  val j9 = Rule.pos(A, A_not).head(Falsum)

  val jExclusionA: Rule = Rule.pos(A).head(Falsum)

  val program = Program(j1, j2, j3, j4, j5, j6, j7, j8, j9)

  def library(evaluation: Evaluation) = {
    it should "be V, P, A" in {
      info("The valid model")
      assert(evaluation(program) contains Set(V, P, A))
    }
    it should "be V,H,P,A_not" in {
      info("With the fact H the model")
      val p = program + Fact(H)

      val model = evaluation(p)

      assert(model contains Set(V, H, P, A_not))
    }

    it should "be A_not,H,P, V" in {
      info("With a constraint for A the model")
      val p = program + jExclusionA

      val model = evaluation(p)

      if (evaluation.isInstanceOf[Asp]) pending

      info("H is currently chosen 'by random'")
      assert(model contains Set(A_not, H, P, V))
    }

    it should "also return the same model when using just a single constraint" in {
      val p = program + Rule.pos(A).head(Falsum)

      val model = evaluation(p)

      if (evaluation.isInstanceOf[Asp]) pending

      info("H is currently chosen 'by random'")
      assert(model contains Set(A_not, H, P, V))
    }

    it should "be P_not,F,V" in {
      info("With a constraint for P the model")
      val p = program + Rule.pos(P).head(Falsum)

      val model = evaluation(p)

      if (evaluation.isInstanceOf[Asp]) pending

      info("F is currently chosen 'by random'")
      assert(model contains Set(P_not, F, V))
    }
  }
}

class Library extends FlatSpec with LibraryBehavior with EvaluateBothImplementations {
  "The Library Sample" should behave like theSame(library)
}

class LibraryAtomValidation extends FlatSpec with AtomValidation with LibraryBehavior {

  def Tmn = TMN(program)

  "Atom V" must behave like atomValidation(Tmn, V) { validator =>
    validator.state(in)
    validator.Rules(j1)
    validator.SJ(Some(j1))
    validator.Supp()
    validator.Ancestors()
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(P)
    validator.ACons(P)
    validator.Repercussions(P, A)
  }

  "Atom P" must behave like atomValidation(Tmn, P) { validator =>
    validator.state(in)
    validator.Rules(j2)
    validator.SJ(Some(j2))
    validator.Supp(V, F, G)
    validator.Ancestors(V, F, G)
    validator.Antecedents(V, F, G)
    validator.Foundations(V, F, G)
    validator.Cons(A, Falsum)
    validator.ACons(A)
    validator.Repercussions(A)
  }

  "Atom A" must behave like atomValidation(Tmn, A) { validator =>
    validator.state(in)
    validator.Rules(j5)
    validator.SJ(Some(j5))
    validator.Supp(P, H, N)
    validator.Ancestors(P, H, N, V, F, G)
    validator.Antecedents(P, H, N)
    validator.Foundations(P, H, N, V, F, G)
    validator.Cons(Falsum)
    validator.ACons()
    validator.Repercussions()
  }

  "Atom F" must behave like atomValidation(Tmn, F) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.Ancestors()
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.Repercussions(P, A, P_not, Falsum)
  }
  "Atom G" must behave like atomValidation(Tmn, G) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.Ancestors()
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.Repercussions(P, A, P_not, Falsum)
  }


  "Atom H" must behave like atomValidation(Tmn, H) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.Ancestors()
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.Repercussions(A, A_not, Falsum)
  }
  "Atom N" must behave like atomValidation(Tmn, N) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.Ancestors()
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.Repercussions(A, A_not, Falsum)
  }

  "Atom P_not" must behave like atomValidation(Tmn, P_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j3, j4)
    validator.Supp(F, G)
    validator.Ancestors(F, G)
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(Falsum)
    validator.ACons(Falsum)
    validator.Repercussions(Falsum)
  }

  "Atom A_not" must behave like atomValidation(Tmn, A_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j8, j7)
    validator.Supp(H, N)
    validator.Ancestors(H, N)
    validator.Antecedents()
    validator.Foundations()
    validator.Cons(Falsum)
    validator.ACons(Falsum)
    validator.Repercussions(Falsum)
  }

  "Atom N_cont" must behave like atomValidation(Tmn, Falsum) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j6, j9)
    validator.Supp(P_not, A_not)
    validator.Ancestors(P_not, A_not, F, G, H, N)
    validator.Antecedents()
    validator.Foundations()
    validator.Cons()
    validator.ACons()
    validator.Repercussions()
  }
}
