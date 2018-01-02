package reasoner.incremental.jtms.tmn

import core.asp.{AspFact, NormalRule}
import core.{Atom, Predicate}
import reasoner.incremental.jtms.algorithms.Jtms
import org.scalatest.FlatSpec

/**
  * Created by FM on 05.02.16.
  */
class JtmsSpec extends FlatSpec {
  def Assumption(node: String): NormalRule = Assumption(Atom(Predicate(node)))

  def Assumption(node: Atom): NormalRule = AspFact(node)

  def EmptyJtms = Jtms()
}
