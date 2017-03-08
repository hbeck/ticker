package core

import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by hb on 08.03.17.
  */
trait Program[TRule <: Rule[THead,TBody], THead <: HeadAtom, TBody <: ExtendedAtom] {
  //val rules: Seq[TRule] //skip that; ClingoExpression is a string
}
