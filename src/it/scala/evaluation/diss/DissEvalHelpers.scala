package evaluation.diss

import core.Atom
import core.lars._

/**
  * Created by hb on 05.04.18.
  */
object DissEvalHelpers {

  def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TimeWindow(windowSize), At(time), atom)
  def wD(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Diamond, atom)
  def wB(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Box, atom)
  def tup_wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TupleWindow(windowSize), At(time), atom)
  def tup_wD(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Diamond, atom)
  def tup_wB(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Box, atom)

  def rule(head: HeadAtom, posBody: Set[ExtendedAtom], negBody: Set[ExtendedAtom]=Set()): LarsRule = {
    UserDefinedLarsRule(head, posBody, negBody)
  }

  def rule(head: HeadAtom, posBody: ExtendedAtom, negBody: ExtendedAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(posBody), Set(negBody))
  }

  def rule(head: HeadAtom, posBody: ExtendedAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(posBody), Set())
  }

  def fact(head: HeadAtom): LarsRule = {
    UserDefinedLarsRule(head, Set(), Set())
  }

}
