package jtms

import core.asp._
import core.{Atom, Model}
import jtms.algorithms._
import jtms.networks.OptimizedNetworkForLearn

/**
  * Created by FM on 13.10.16.
  */
object Jtms {

  sealed trait JtmsVariant
  object JtmsVariantDoyleHeuristics extends JtmsVariant
  object JtmsVariantDoyle extends JtmsVariant
  //old and experimental:
  object JtmsVariantBeierle extends JtmsVariant
  object JtmsVariantBeierleFixed extends JtmsVariant
  object JtmsVariantGreedy extends JtmsVariant
  object JtmsVariantLearn extends JtmsVariant

  //default implementation
  var jtmsVariant: JtmsVariant = JtmsVariantDoyleHeuristics

  def apply(P: NormalProgram): Jtms = {
    val jtms = Jtms()
    P.rules foreach jtms.add
    jtms
  }

  def apply(): Jtms = {
    jtmsVariant match {
      case `JtmsVariantDoyleHeuristics` => new JtmsDoyleHeuristics(TruthMaintenanceNetwork())
      case `JtmsVariantDoyle` => new JtmsDoyle(TruthMaintenanceNetwork())
      case `JtmsVariantBeierle` => new JtmsBeierle(TruthMaintenanceNetwork())
      case `JtmsVariantBeierleFixed` => new JtmsBeierleFixed(TruthMaintenanceNetwork())
      case `JtmsVariantGreedy` => new JtmsGreedy(TruthMaintenanceNetwork())
      case `JtmsVariantLearn` => new JtmsLearn(new OptimizedNetworkForLearn())
    }

  }
}


trait Jtms extends ChoiceControl {

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
