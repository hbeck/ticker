package core.grounding

import core.lars.{ExtendedAtom, HeadAtom}
import core.{Program, Rule}

/**
  * Created by hb on 08.03.17.
  */
trait OneShotGrounding[TProgram <: Program[TRule,THead,TBody], TRule <: Rule[THead,TBody], THead <: HeadAtom, TBody <: ExtendedAtom] {
  val program: TProgram
}
