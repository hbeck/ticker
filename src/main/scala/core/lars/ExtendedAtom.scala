package core.lars

import core.Atom
import engine.Time

/**
  * Created by FM on 01.05.16.
  */
trait ExtendedAtom

trait HeadAtom extends ExtendedAtom


object HeadAtom {
  implicit def headAtomToBuilder(atom: HeadAtom): LarsBuilderHead = new LarsBuilderHead(atom)

  implicit def headAtomToFact(atom: HeadAtom): Rule = Fact(atom)
}

case class WindowAtom(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) extends ExtendedAtom

case class AtAtom(val time: Time, val atom: Atom) extends HeadAtom