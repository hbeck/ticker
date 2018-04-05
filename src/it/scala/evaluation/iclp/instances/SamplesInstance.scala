package evaluation.iclp.instances

import core._
import core.asp.NormalRule
import core.lars.{AtAtom, LarsProgram, LarsRule}
import evaluation.iclp.StreamingTmsEvalInst

import scala.util.Random

trait Randomness

object High extends Randomness

object Low extends Randomness

/**
  * Created by hb on 26.04.17.
  */
abstract class SamplesInstance(windowSize: Int, instanceCount: Int, randomness: Randomness, random: Random) extends StreamingTmsEvalInst {

  val value = Predicate("value")
  val b = Predicate("b")
  val h = Predicate("h")

  val T: Variable = StringVariable("T")

  val X: Variable = StringVariable("X")


  //
  //  h(X) :- value(X), \timeWin D b(X)
  //  h(X) :- value(X), \timeWin B b(X)
  //  @_T h(X) :- value(X), \tupleWin @_T b(X)
  //  h(X) :- value(X), \tupleWin D b(X)
  //  h(X) :- value(X), \tupleWin B b(X)

  val instances = (0 to instanceCount) map (IntValue(_))

  override def staticRules(): Seq[NormalRule] = ???

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = ???

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = ???

  def factAtoms(): Seq[Atom] = {
    instances map (value(_))
  }

  def generateSignal(v: Value): Seq[Atom] = randomness match {
    case Low if random.nextDouble() < (1.0 / 50.0) => Seq(b(v))
    case High if random.nextDouble() < 0.95 => Seq(b(v))
    case _ => Seq()
  }


  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    instances flatMap generateSignal
  }

}

case class SampleTimeWindowAtInstance(windowSize: Int, timePoints: Int, val instanceCount: Int, randomness: Randomness, random: Random) extends SamplesInstance(windowSize, instanceCount, randomness, random) {

  override def larsProgram(windowSize: Int): LarsProgram = {

    val rules: Seq[LarsRule] = Seq[LarsRule](
      AtAtom(T, h(X)) <= value(X) and wAt(windowSize, T, b(X))
    ) ++ (factAtoms map larsFact)

    LarsProgram(rules)
  }


  override def verifyModel(model: Option[Model], t: Int): Unit = {
    // do we need this?
  }

}