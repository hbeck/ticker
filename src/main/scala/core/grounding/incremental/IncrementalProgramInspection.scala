package core.grounding.incremental

import core._
import core.asp.{NormalProgram, NormalRule}
import core.grounding.{ProgramInspection, StaticProgramInspection}
import core.lars.{ExtendedAtom, HeadAtom, LarsProgram, LarsRule}

/**
  * Created by hb on 08.03.17.
  */
/*
 * Works for ASP and LARS without @
 *
  */
case class IncrementalProgramInspection[TRule <: Rule[THead, TBody], THead <: HeadAtom, TBody <: ExtendedAtom](rules: Seq[TRule]) extends ProgramInspection[TRule,THead,TBody] {

  //TODO fix static one first
  override def possibleVariableValues(rule: TRule, ensureGroundResult: Boolean): Map[Variable, Set[Value]] = {
    Map()
    //rule.variables map (v => (v, possibleValuesForVariable(rule, v))) toMap
  }

}

object IncrementalProgramInspection {
  def forLars(program: LarsProgram): IncrementalProgramInspection[LarsRule, HeadAtom, ExtendedAtom] = IncrementalProgramInspection[LarsRule, HeadAtom, ExtendedAtom](program.rules)

  //TODO use incremental after static one has been fixed
  //def forAsp(program: NormalProgram): IncrementalProgramInspection[NormalRule, Atom, Atom] = IncrementalProgramInspection[NormalRule, Atom, Atom](program.rules)
  def forAsp(program: NormalProgram): StaticProgramInspection[NormalRule, Atom, Atom] = StaticProgramInspection[NormalRule, Atom, Atom](program.rules)
}