package core.lars

import core.Atom

/**
  * Created by FM on 01.05.16.
  */
trait ExtendedAtom {
  val atom: Atom //TODO
  def isGround(): Boolean
  def assign(assignment: Assignment): ExtendedAtom //not called 'ground' since assignment might be partial
}

trait HeadAtom extends ExtendedAtom {
  //def atom(): Atom //TODO
}

trait GroundExtendedAtom extends ExtendedAtom {
  override def isGround(): Boolean = true
  override def assign(assignment: Assignment) = this
}

object HeadAtom {
  implicit def headAtomToBuilder(atom: HeadAtom): LarsBuilderHead = new LarsBuilderHead(atom)

  implicit def headAtomToFact(atom: HeadAtom): LarsFact = LarsFact(atom)
}

case class WindowAtom(windowFunction: WindowFunction, temporalModality: TemporalModality, atom: Atom) extends ExtendedAtom {
  override def isGround(): Boolean = atom.isGround()
  override def assign(assignment: Assignment) = WindowAtom(windowFunction, temporalModality, (atom assign assignment).asInstanceOf[Atom])
}

case class AtAtom(time: Time, atom: Atom) extends HeadAtom {
  override def isGround(): Boolean = time.isInstanceOf[TimePoint] && atom.isGround()
  override def assign(assignment: Assignment): ExtendedAtom = atom assign assignment
}