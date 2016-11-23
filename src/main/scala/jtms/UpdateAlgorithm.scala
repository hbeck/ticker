package jtms

import core.Atom
import core.asp._
import jtms.algorithms.JtmsGreedy
import jtms.networks.SimpleNetwork

/**
  * Created by FM on 13.10.16.
  */
object JtmsUpdateAlgorithm {
  def apply() = JtmsGreedy(TruthMaintenanceNetwork())
}


trait JtmsUpdateAlgorithm extends ChoiceControl {

  def add(rule: NormalRule)

  def remove(rule: NormalRule)

  //def recompute()
  def getModel(): Option[Set[Atom]]

  def set(model: Set[Atom]): Boolean

  def update(atoms: Set[Atom])


  //book keeping:
  var recordStatusSeq = false
  var statusSeq = Seq[(Atom, Status, String)]()

  var recordChoiceSeq = false
  var choiceSeq = Seq[Atom]()

  def rules: Set[NormalRule]


}
