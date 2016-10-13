package jtms

import core.Atom
import core.asp._

/**
  * Created by FM on 13.10.16.
  */
trait JtmsUpdateAlgorithm extends ChoiceControl {
  val jtms: JtmsAbstraction
  def add(rule: NormalRule)
  def remove(rule: NormalRule)
  //def recompute()
  def getModel(): Option[Set[Atom]]
  def set(model: Set[Atom]): Boolean

  def update(atoms: Set[Atom])


  //book keeping:
  var recordStatusSeq = false
  var statusSeq = Seq[(Atom,Status,String)]()

  var recordChoiceSeq = false
  var choiceSeq = Seq[Atom]()


}
