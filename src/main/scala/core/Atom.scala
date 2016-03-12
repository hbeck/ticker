package core

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom { // TODO (hb) should extend rule
  def caption: String
  override def toString = caption
}

object Atom {
  def apply(caption: String): Atom = UserDefinedAtom(caption)
}

case class UserDefinedAtom(caption: String) extends Atom

case class ContradictionAtom(caption: String) extends Atom //TODO (HB)
