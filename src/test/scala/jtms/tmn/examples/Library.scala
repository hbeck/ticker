package jtms.tmn.examples

import core._
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
  val j2 = Rule.pos(V).neg(F, G).head(P)
  val j3 = Rule.pos(F).head(P_not)
  val j4 = Rule.pos(G).head(P_not)
  val j5 = Rule.pos(P).neg(H, N).head(A)
  val j6 = Rule.pos(P, P_not).head(N_cont)
  val j7 = Rule.pos(N).head(A_not)
  val j8 = Rule.pos(H).head(A_not)
  val j9 = Rule.pos(A, A_not).head(N_cont)

  val jExclusionA = Rule.pos(A).head(N_cont)

  val program = Program(j1, j2, j3, j4, j5, j6, j7, j8, j9)

  def Tmn = {
    val tmn = TMN(program)

    tmn
  }

  "The valid model" should "be V, P, A" in {
    assert(Tmn.getModel() == Set(V, P, A))
  }

  "Atom V" must behave like atomValidation(Tmn, V) { validator =>
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

  "Atom P" must behave like atomValidation(Tmn, P) { validator =>
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

  "Atom A" must behave like atomValidation(Tmn, A) { validator =>
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

  "Atom F" must behave like atomValidation(Tmn, F) { validator =>
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
  "Atom G" must behave like atomValidation(Tmn, G) { validator =>
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


  "Atom H" must behave like atomValidation(Tmn, H) { validator =>
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
  "Atom N" must behave like atomValidation(Tmn, N) { validator =>
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

  "Atom P_not" must behave like atomValidation(Tmn, P_not) { validator =>
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

  "Atom A_not" must behave like atomValidation(Tmn, A_not) { validator =>
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

  "Atom N_cont" must behave like atomValidation(Tmn, N_cont) { validator =>
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
    val tmn = Tmn
    tmn.add(Premise(H))

    val model = tmn.getModel()

    assert(model == Set(V, H, P, A_not))
  }

  "With a contradiction node for A the model" should "be A_not,H,P, V" in {
    val tmn = Tmn

    tmn.add(jExclusionA)

    val model = tmn.getModel()
    info("H is currently chosen 'by random'")
    assert(model == Set(A_not, H, P, V))
  }

  it should "also return the same model when using just a single contradiction node" in {
    val tmn = Tmn

    tmn.add(Rule.pos(A).head(N_cont))

    val model = tmn.getModel()
    info("H is currently chosen 'by random'")
    assert(model == Set(A_not, H, P, V))
  }

  "With a contradiction node for P the model" should "be P_not,F,V" in {
    val tmn = Tmn

    tmn.add(Rule.pos(P).head(N_cont))

    val model = tmn.getModel()
    info("F is currently chosen 'by random'")
    assert(model == Set(P_not, F, V))
  }
}
