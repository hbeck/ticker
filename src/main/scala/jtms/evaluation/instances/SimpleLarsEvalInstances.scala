package jtms.evaluation.instances

import core._
import core.asp.NormalRule
import core.lars.{LarsProgram, LarsRule}
import jtms.evaluation.StreamingTmsEvalInst

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

  val q = Atom("q")
  val p = Atom("q")

  override def larsProgram(windowSize: Int): LarsProgram = {

    val rules: Seq[LarsRule] = Seq[LarsRule](
      q <= wB(windowSize,p)
    )

    LarsProgram(rules)
  }

  override def verifyModel(model: Option[Model], t: Int): Unit = {
    val m = model.get
    if (t <= 2*windowSize) {
      contains(m,t,q)
    } else {
      notContains(m,t,q)
    }
  }

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t<=2*windowSize) Seq(p) else Seq()
  }

}
