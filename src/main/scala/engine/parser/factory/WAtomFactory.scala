package engine.parser.factory

import core.{Atom, Variable}
import core.lars._

/**
  * Created by et on 22.03.17.
  */
case class WAtomFactory(predicate: AtomTrait, tempMod: Option[TemporalModality], window: WindowFactory)
  extends AtomTrait {

  val atom: WindowAtom = create(predicate,tempMod,window)
  override val neg: Boolean = false

  def create(predicate: AtomTrait, tempMod: Option[TemporalModality], window: WindowFactory): WindowAtom =
    tempMod match {
      case None => matchAtom(predicate,None,window)
      case _ => matchAtom(predicate,tempMod,window)
    }

  private def matchAtom(predicate: AtomTrait, mod: Option[TemporalModality], window: WindowFactory): WindowAtom =
    predicate match {
      case at: AtAtomFactory => WindowAtom(window.wfn,At(Variable(at.time)),at.atom.atom)
      case a: AtomFactory => WindowAtom(window.wfn,mod.get,a.atom)
      case c => println(c); ???
    }
}
