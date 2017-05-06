package core.grounding

import core.Atom
import core.asp.NormalRule
import core.lars.{ExtendedAtom, HeadAtom, LarsRule}

/**
  * Created by hb on 08.03.17.
  */
object GrounderInstance {
  def forLars(inspect: StaticProgramInspection[LarsRule,HeadAtom,ExtendedAtom]) = new RuleGrounder[LarsRule, HeadAtom, ExtendedAtom](inspect)
  def forAsp(inspect: StaticProgramInspection[NormalRule,Atom,Atom]) = new RuleGrounder[NormalRule, Atom, Atom](inspect)
}
