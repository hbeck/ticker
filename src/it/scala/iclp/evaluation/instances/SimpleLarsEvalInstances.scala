package iclp.evaluation.instances

import core._
import core.asp.NormalRule
import core.lars.{LarsProgram, LarsRule}
import iclp.evaluation.StreamingTmsEvalInst

import scala.util.Random

/**
  * Created by hb on 26.04.17.
  */
abstract class SimpleLarsEvalInstances extends StreamingTmsEvalInst {

  override def staticRules(): Seq[NormalRule] = ???

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = ???

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = ???

}

case class BoxEvalInst(windowSize: Int, timePoints: Int, random: Random) extends SimpleLarsEvalInstances {

  val inf = Atom("inf")
  val sig = Atom("sig")

  override def larsProgram(windowSize: Int): LarsProgram = {

    val rules: Seq[LarsRule] = Seq[LarsRule](
      inf <= wB(windowSize,sig)
    )

    LarsProgram(rules)
  }

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t<=2*windowSize) Seq(sig) else Seq()
  }

  override def verifyModel(model: Option[Model], t: Int): Unit = {
    val m = model.get
    if (t <= 2*windowSize) {
      contains(m,t,inf)
    } else {
      notContains(m,t,inf)
    }
  }

}

case class TupleBoxEvalInst() extends SimpleLarsEvalInstances {

  val windowSize = 3
  val timePoints = 20
  val random: Random = new Random(0)

  val _a = Predicate("a")
  val _b = Predicate("b")
  val _v = Predicate("v")

  val a_X = Atom(_a,Seq("X"))
  val b_X = Atom(_b,Seq("X"))
  val v_X = Atom(_v,Seq("X"))

  val a_x1 = Atom(_a,Seq("x1"))
  val a_x2 = Atom(_a,Seq("x2"))
  val a_x3 = Atom(_a,Seq("x3"))
  val a_x4 = Atom(_a,Seq("x4"))
  val b_x1 = Atom(_b,Seq("x1"))
  val b_x2 = Atom(_b,Seq("x2"))
  val b_x3 = Atom(_b,Seq("x3"))
  val b_x4 = Atom(_b,Seq("x4"))

  val facts = Seq(Atom(_v,Seq("x1")),Atom(_v,Seq("x2")),Atom(_v,Seq("x3")),Atom(_v,Seq("x4"))) map (larsFact(_))

  override def larsProgram(windowSize: Int): LarsProgram = {

    val rules: Seq[LarsRule] = Seq[LarsRule](
      b_X <= v_X and tup_wB(windowSize,a_X)
    ) ++ facts

    LarsProgram(rules)
  }

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t==0) {
      Seq()
    } else if (t==1) {
      Seq(a_x1,a_x2,a_x3,a_x4)
    } else if (t==2) {
      Seq(a_x2,a_x4)
    } else if (t==3) {
      Seq(a_x2,a_x4)
    } else if (t==4) {
      Seq(a_x2,a_x4)
    } else if (t==5) {
      Seq(a_x4)
    } else if (t==6) {
      Seq(a_x4)
    } else if (t==7) {
      Seq(a_x4,a_x1,a_x2,a_x3)
    } else if (t==8) {
      Seq(a_x2,a_x3)
    } else if (t==9) {
      Seq()
    } else if (t==10) {
      Seq()
    } else if (t==11) {
      Seq(a_x1,a_x2,a_x3)
    } else if (t==12) {
      Seq(a_x3)
    } else if (t==13) {
      Seq(a_x3)
    } else if (t==14) {
      Seq(a_x3)
    } else if (t==15) {
      Seq(a_x3,a_x1)
    } else if (t==16) {
      Seq(a_x3)
    } else if (t==17) {
      Seq(a_x1,a_x3)
    } else {
      Seq()
    }
  }

  override def verifyModel(model: Option[Model], t: Int): Unit = {
    val m = model.get
    if (t == 0) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t == 1) {
      notContains(m,t,b_x1)
      contains(m,t,b_x2)
      contains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 2) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 3) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 4) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 5) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 6) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      contains(m,t,b_x4)
    } else if (t == 7) {
      contains(m,t,b_x1)
      contains(m,t,b_x2)
      contains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t == 8) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      contains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t == 9) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t == 10) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t == 11) {
      contains(m,t,b_x1)
      contains(m,t,b_x2)
      contains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else if (t >= 12 && t <=17) {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      contains(m,t,b_x3)
      notContains(m,t,b_x4)
    } else {
      notContains(m,t,b_x1)
      notContains(m,t,b_x2)
      notContains(m,t,b_x3)
      notContains(m,t,b_x4)
    }
  }

}
