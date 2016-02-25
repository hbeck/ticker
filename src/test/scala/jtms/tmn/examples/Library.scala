package jtms.tmn.examples

import core.{Rule, Premise, ContradictionAtom, Atom}
import jtms._
import jtms.tmn.AtomValidation
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */
class Library extends FlatSpec with AtomValidation {

  val V = Atom("verfügbar")
  val G = Atom("gestohlen")
  val P = Atom("am angegebenen Platz vorhanden")
  val F = Atom("falsch einsortiert")
  val P_not = Atom("nicht am angegebenen Platz vorhanden")
  val A = Atom("ausleihbar")
  val N = Atom("Nachschlagewerk")
  val A_not = Atom("nicht ausleihbar")
  val H = Atom("im Handapperart einer Veranstaltung")
  val N_cont = ContradictionAtom("Widerspruch")

  val j1 = Premise(V)
  val j2 = Rule.in(V).out(F, G).head(P)
  val j3 = Rule.in(F).head(P_not)
  val j4 = Rule.in(G).head(P_not)
  val j5 = Rule.in(P).out(H, N).head(A)
  val j6 = Rule.in(P, P_not).head(N_cont)
  val j7 = Rule.in(N).head(A_not)
  val j8 = Rule.in(H).head(A_not)
  val j9 = Rule.in(A, A_not).head(N_cont)

  val jExclusionA =  Rule.in(A).head(N_cont)

  def TMN = {
    val tmn = new TMN(Set(V, G, P, F, P_not, A, N, A_not, H, N_cont))

    tmn.add(j1)
    tmn.add(j2)
    tmn.add(j3)
    tmn.add(j4)
    tmn.add(j5)
    tmn.add(j6)
    tmn.add(j7)
    tmn.add(j8)
    tmn.add(j9)

    tmn
  }

  "The valid model" should "be V, P, A" in {
    assert(TMN.getModel() == Set(V, P, A))
  }

  "Atom V" must behave like atomValidation(TMN, V) { validator =>
    validator.state(in)
    validator.Rules(j1)
    validator.SJ(Some(j1))
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P)
    validator.ACons(P)
    validator.AConsTrans(P, A)
  }

  "Atom P" must behave like atomValidation(TMN, P) { validator =>
    validator.state(in)
    validator.Rules(j2)
    validator.SJ(Some(j2))
    validator.Supp(V, F, G)
    validator.SuppTrans(V, F, G)
    validator.Ant(V, F, G)
    validator.AntTrans(V, F, G)
    validator.Cons(A, N_cont)
    validator.ACons(A)
    validator.AConsTrans(A)
  }

  "Atom A" must behave like atomValidation(TMN, A) { validator =>
    validator.state(in)
    validator.Rules(j5)
    validator.SJ(Some(j5))
    validator.Supp(P, H, N)
    validator.SuppTrans(P, H, N, V, F, G)
    validator.Ant(P, H, N)
    validator.AntTrans(P, H, N, V, F, G)
    validator.Cons(N_cont)
    validator.ACons()
    validator.AConsTrans()
  }

  "Atom F" must behave like atomValidation(TMN, F) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.AConsTrans(P, A, P_not, N_cont)
  }
  "Atom G" must behave like atomValidation(TMN, G) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(P, P_not)
    validator.ACons(P, P_not)
    validator.AConsTrans(P, A, P_not, N_cont)
  }


  "Atom H" must behave like atomValidation(TMN, H) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.AConsTrans(A, A_not, N_cont)
  }
  "Atom N" must behave like atomValidation(TMN, N) { validator =>
    validator.state(out)
    validator.Rules()
    validator.SJ(None)
    validator.Supp()
    validator.SuppTrans()
    validator.Ant()
    validator.AntTrans()
    validator.Cons(A, A_not)
    validator.ACons(A, A_not)
    validator.AConsTrans(A, A_not, N_cont)
  }

  "Atom P_not" must behave like atomValidation(TMN, P_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j3, j4)
    validator.Supp(F, G)
    validator.SuppTrans(F, G)
    validator.Ant()
    validator.AntTrans()
    validator.Cons(N_cont)
    validator.ACons(N_cont)
    validator.AConsTrans(N_cont)
  }

  "Atom A_not" must behave like atomValidation(TMN, A_not) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j8, j7)
    validator.Supp(H, N)
    validator.SuppTrans(H, N)
    validator.Ant()
    validator.AntTrans()
    validator.Cons(N_cont)
    validator.ACons(N_cont)
    validator.AConsTrans(N_cont)
  }

  "Atom N_cont" must behave like atomValidation(TMN, N_cont) { validator =>
    validator.state(out)
    validator.SJ(None)
    validator.Rules(j6, j9)
    validator.Supp(P_not, A_not)
    validator.SuppTrans(P_not, A_not, F, G, H, N)
    validator.Ant()
    validator.AntTrans()
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
  }

  "With the premise H the model" should "be V,H,P,A_not" in {
    val tmn = TMN
    tmn.add(Premise(H))

    val model = tmn.getModel()

    assert(model == Set(V, H, P, A_not))
  }

  "With a contradiction node for A the model" should "be A_not,H,P, V" in {
    val tmn = TMN

    tmn.add(jExclusionA)

    val model = tmn.getModel()
    info("H is currently chosen 'by random'")
    assert(model == Set(A_not, H, P, V))
  }

  it should "also return the same model when using just a single contradiction node" in {
    val tmn = TMN

    tmn.add(Rule.in(A).head(N_cont))

    val model = tmn.getModel()
    info("H is currently chosen 'by random'")
    assert(model == Set(A_not, H, P, V))
  }

  "With a contradiction node for P the model" should "be P_not,F,V" in {
    val tmn = TMN

    tmn.add(Rule.in(P).head(N_cont))

    val model = tmn.getModel()
    info("F is currently chosen 'by random'")
    assert(model == Set(P_not, F, V))
  }
}
