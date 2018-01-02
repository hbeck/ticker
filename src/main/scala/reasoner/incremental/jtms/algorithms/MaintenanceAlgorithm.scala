package reasoner.incremental.jtms.algorithms

import core.{Atom, Model}
import core.asp.NormalRule

/**
  * Abstraction of update mechanism
  *
  * Created by hb on 07.12.17.
  */
trait MaintenanceAlgorithm {

  def add(rule: NormalRule)

  def remove(rule: NormalRule)

  def getModel(): Option[Model]

  def set(model: Set[Atom]): Boolean

  def update(atoms: Set[Atom])

  def rules: Set[NormalRule]

}
