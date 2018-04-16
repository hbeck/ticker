package evaluation.diss

import core.Atom
import core.lars._

/**
  * Created by hb on 05.04.18.
  */
object DissEvalHelpers {

  def wt_At(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TimeWindow(windowSize), At(time), atom)
  def wt_D(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Diamond, atom)
  def wt_B(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Box, atom)
  def wc_At(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TupleWindow(windowSize), At(time), atom)
  def wc_D(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Diamond, atom)
  def wc_B(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Box, atom)

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
