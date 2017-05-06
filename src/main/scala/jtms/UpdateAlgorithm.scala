package jtms

import core.{Atom, Model}
import core.asp._
import jtms.algorithms.JtmsDoyleHeuristics

/**
  * Created by FM on 13.10.16.
  */
object JtmsUpdateAlgorithm {
  def apply() = new JtmsDoyleHeuristics(TruthMaintenanceNetwork())
}


trait JtmsUpdateAlgorithm extends ChoiceControl {

  def add(rule: NormalRule)

  def remove(rule: NormalRule)

  def getModel(): Option[Model]

  def set(model: Set[Atom]): Boolean

  def update(atoms: Set[Atom])


  //book keeping:
  var recordStatusSeq = false
  var statusSeq = Seq[(Atom, Status, String)]()

  var recordChoiceSeq = false
  var choiceSeq = Seq[Atom]()

  def rules: Set[NormalRule]


}
