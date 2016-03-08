package core

import jtms.{out, in, Status}

case class Label(val atom: Atom, val status: Status)

/**
  * Created by FM on 07.03.16.
  */
case class Support(val atom: Atom) {
  var forIn: Set[Label] = Set()
  var forOut: Set[Label] = Set()
  var rule: Option[Rule] = None

  def statusIn(rule: Rule) = {
    val inAtoms = rule.pos.map(Label(_, in))
    val outAtoms = rule.neg.map(Label(_, out))

    forIn = inAtoms union outAtoms
    this.rule = Some(rule)
  }

  def statusOut(spoilers: Set[Label]) = {
    forOut = forOut ++ spoilers
    this.rule = None
  }

  def forStatus(status: Status) = {
    status match {
      case `in` => forIn
      case `out` => forOut
      case _ => Set()
    }
  }
}
