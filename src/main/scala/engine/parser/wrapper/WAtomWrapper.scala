package engine.parser.wrapper

import core.Atom
import core.lars._

/**
  * Created by et on 22.03.17.
  */
case class WAtomWrapper(predicate: AtomTrait, tempMod: Option[TemporalModality], window: WindowWrapper)
  extends AtomTrait {

  val atom: WindowAtom = create(predicate,tempMod,window)
  override val neg: Boolean = false

  def create(predicate: AtomTrait, tempMod: Option[TemporalModality], window: WindowWrapper): WindowAtom =
    tempMod match {
      case None => matchAtom(predicate,None,window)
      case _ => matchAtom(predicate,tempMod,window)
    }

  private def matchAtom(predicate: AtomTrait, mod: Option[TemporalModality], window: WindowWrapper): WindowAtom =
    predicate match {
      case at: AtAtom => WindowAtom(window.wfn,At(at.time),at.atom)
      case a: Atom => WindowAtom(window.wfn,mod.get,a)
    }
}
