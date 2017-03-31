package core.grounding

import core.{Rule, Value, Variable}
import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by hb on 08.03.17.
  */
trait ProgramInspection[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom] {

  def possibleVariableValues(rule: TRule): Map[Variable, Set[Value]]

}
