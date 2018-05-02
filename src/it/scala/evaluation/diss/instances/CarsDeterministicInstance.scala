package evaluation.diss.instances

import core.Atom
import evaluation.diss.instances.traits.Instance
import evaluation.diss.programs.CarsProgramProvider
import reasoner.Result

/**
  * Created by hb on 01.05.18.
  *
  * scale: total number of cars
  * k: threshold - write 'moreThanK' if more than k recorded in timeWindowSize
  *
  */
case class CarsDeterministicInstance(scale: Int, timeWindowSize: Int, k: Int) extends Instance with CarsProgramProvider {

  assert(scale > 0)
  assert(timeWindowSize >= 0)
  assert(k >= 1)

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    Seq() //TODO
  }

  override def verifyOutput(result: Result, t: Int): Unit = {
    //TODO
  }

}
