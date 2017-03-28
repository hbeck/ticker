package engine.parser.factory

import core.Atom
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
      case at: AtAtom => WindowAtom(window.wfn,At(at.time),at.atom)
      case a: Atom => WindowAtom(window.wfn,mod.get,a)
    }
}
