package evaluation.diss.instances

import core.{Atom, IntValue, Model}
import evaluation.diss.Instance
import evaluation.diss.PreparedAtoms.string2Atom
import evaluation.diss.programs.AnalyticProgramProvider._
import evaluation.diss.programs.{RandomProvider, LinReachLtProgramProvider}

import scala.util.Random

/**
  * Created by hb on 01.05.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb}
  * scale: nr of nodes
  * percent: lower tier of node indexes that do *not* get a fail signal. measure of model maintainability.
  */
case class LinReachPercInstance(random: Random, wm: String, windowSize: Int, signalEvery: Int, scale: Int, percent: Int) extends Instance with LinReachLtProgramProvider with RandomProvider {

  assert(signalEvery > 0)
  assert(scale > 0)
  assert(percent >= 0)
  assert(percent <= 100)

  val offset:Int = (0.01*percent*scale).toInt //e.g. 30% of 200 = 60
  val shifterUpperBound = scale-offset //140 --> 0..139. actual range adds offset

  val winMod = winModFromString(wm)

  var currFailAtom: Option[Atom] = None

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % signalEvery == 0) {
      val i = random.nextInt(shifterUpperBound+1)+offset //--> (0+60..140+60) = 60..200 [fail(0)..fail(59) are not produced. last edge (200,201)]
      val atom: Atom = f"fail($i)"
      currFailAtom = Some(atom)
      Seq[Atom](atom)
    } else {
      currFailAtom = None
      Seq()
    }
  }

  override def verify_time_at(model: Model, t: Int): Unit = {
    verify_time_diamond(model,t)
  }

  override def verify_time_diamond(model: Model, t: Int): Unit = {
    //partial
    if (!currFailAtom.isEmpty) {
      val failAtom = currFailAtom.get
      val i = failAtom.arguments.head.asInstanceOf[IntValue].int
      for (atom <- model) {
        if (atom.predicate.caption == "reach") {
          val x = atom.arguments.head.asInstanceOf[IntValue].int
          val y = atom.arguments.tail.head.asInstanceOf[IntValue].int
          if (x <= i) {
            if (x == i || y > i) {
              println(f"reach($x,$y) derived at t=$t despite signal fail($i). model: $model")
              assert(false)
            }
          }
        }
      }
    }
  }

  override def verify_time_box(model: Model, t: Int): Unit = {
    //
  }

  override def verify_count_at(model: Model, t: Int): Unit = {
    verify_count_diamond(model,t)
  }

  override def verify_count_diamond(model: Model, t: Int): Unit = {
    //
  }

  override def verify_count_box(model: Model, t: Int): Unit = {
    //
  }

}
