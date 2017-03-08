package core.grounding

import core.Atom
import core.asp.NormalRule
import core.lars.{ExtendedAtom, HeadAtom, LarsRule}

/**
  * Created by hb on 08.03.17.
  */
object GrounderInstance {
  def oneShotLars(inspect: StaticProgramInspection[LarsRule,HeadAtom,ExtendedAtom]) = new OneShotRuleGrounder[LarsRule, HeadAtom, ExtendedAtom](inspect)
  def oneShotAsp(inspect: StaticProgramInspection[NormalRule,Atom,Atom]) = new OneShotRuleGrounder[NormalRule, Atom, Atom](inspect)
}
