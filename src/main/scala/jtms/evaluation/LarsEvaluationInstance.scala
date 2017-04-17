package jtms.evaluation

import core.Model
import core.Atom
import core.lars.LarsProgram

import scala.util.Random

/**
  * Created by hb on 17.04.17.
  */
trait LarsEvaluationInstance {
  val timePoints: Int
  val program: LarsProgram
  def verifyModel(model: Option[Model], t: Int)
  def generateFactAtomsToAddAt(t: Int): Seq[Atom]
  def random: Random
}
