package core.lars

import core.Atom

/**
  * Created by FM on 01.05.16.
  */
trait ExtendedAtom

trait HeadAtom extends ExtendedAtom


object HeadAtom {
  implicit def headAtomToBuilder(atom: HeadAtom): LarsBuilderHead = new LarsBuilderHead(atom)

  implicit def headAtomToFact(atom: HeadAtom): LarsRule = LarsFact(atom)
}

case class WindowAtom(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) extends ExtendedAtom

case class AtAtom(time: Time, atom: Atom) extends HeadAtom