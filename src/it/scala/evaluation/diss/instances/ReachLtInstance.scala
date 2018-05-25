package evaluation.diss.instances

import evaluation.diss.Helpers.string2Atom
import core.{Atom, IntValue, Model}
import evaluation.diss.instances.traits.{AnalyticInstance, Randomized}
import evaluation.diss.programs.LinReachLtProgramProvider
import evaluation.diss.programs.properties.AnalyticProgramProvider.winModFromString

import scala.util.Random

/**
  * Created by hb on 30.04.18.
  *
  * wm: window and modality indicator: {ta,td,tb,ca,cd,cb}
  * scale: nr of nodes
  */
case class ReachLtInstance(random: Random, wm: String, windowSize: Int, signalEvery: Int, scale: Int) extends AnalyticInstance with LinReachLtProgramProvider with Randomized {

  assert(signalEvery > 0)
  assert(scale > 0)

  val winMod = winModFromString(wm)

  var currFailAtom: Option[Atom] = None

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t % signalEvery == 0) {
      val i = random.nextInt(scale)
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
