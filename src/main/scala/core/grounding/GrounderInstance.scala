package core.grounding

import core.Atom
import core.asp.NormalRule
import core.lars.{ExtendedAtom, HeadAtom, LarsRule}

/**
  * Created by hb on 08.03.17.
  */
object GrounderInstance {
  def forLars() = new RuleGrounder[LarsRule, HeadAtom, ExtendedAtom]()
  def forAsp() = new RuleGrounder[NormalRule, Atom, Atom]()
}
