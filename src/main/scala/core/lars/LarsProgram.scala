package core.lars

import core._

/**
  * Created by FM on 01.05.16.
  */
trait LarsBasedProgram /* extends Program[LarsRule,HeadAtom,ExtendedAtom] */ {
  val larsRules: Seq[LarsRule]

  lazy val windowAtoms: Set[WindowAtom] = larsRules flatMap {
    _.body collect { case w: WindowAtom => w }
  } toSet

  lazy val atAtoms: Set[HeadAtom] = larsRules flatMap {
    _.atoms collect {
      case a: AtAtom => a
      case WindowAtom(_, At(t), a) => AtAtom(t, a)
    }
  } toSet

  lazy val slidingTimeWindowsAtoms: Set[TimeWindow] = windowAtoms collect {
    case w: WindowAtom if w.windowFunction.isInstanceOf[TimeWindow] => w.windowFunction.asInstanceOf[TimeWindow]
  }
  lazy val slidingTupleWindowsAtoms: Set[TupleWindow] = windowAtoms collect {
    case w: WindowAtom if w.windowFunction.isInstanceOf[TupleWindow] => w.windowFunction.asInstanceOf[TupleWindow]
  }

  lazy val maximumWindowSize: TimeWindowSize = slidingTimeWindowsAtoms.isEmpty match {
    case true => TimeWindowSize(0)
    case false => slidingTimeWindowsAtoms.maxBy(_.windowSize).windowSize
  }
  lazy val maximumTupleWindowSize: TupleCount = slidingTupleWindowsAtoms.isEmpty match {
    case true => 0
    case false => slidingTupleWindowsAtoms.maxBy(_.windowSize).windowSize
  }

  lazy val intensionalAtoms: Set[Atom] = larsRules map (_.head.atom) toSet

  lazy val signals: Set[Atom] = windowAtoms.map(_.atom) -- intensionalAtoms

}

object LarsRule {
  def apply(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]) = UserDefinedLarsRule(head, pos, neg)
}

case class UserDefinedLarsRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom] = Set()) extends LarsRule {

  override lazy val atoms: Set[ExtendedAtom] = pos union neg + head

  override def from(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): UserDefinedLarsRule = {
    UserDefinedLarsRule(head, pos, neg)
  }
}

case class LarsFact(head: HeadAtom) extends Fact[HeadAtom, ExtendedAtom] {

  override lazy val atoms: Set[ExtendedAtom] = Set[ExtendedAtom](head)

  override def from(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): LarsFact = {
    LarsFact(head)
  }
}

case class BasicLarsFact(head: Atom) extends Fact[Atom, ExtendedAtom] {

  override lazy val atoms: Set[ExtendedAtom] = Set[ExtendedAtom](head)

  override def from(head: Atom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): BasicLarsFact = {
    BasicLarsFact(head)
  }
}

case class LarsProgram(rules: Seq[LarsRule]) extends LarsBasedProgram {
  val larsRules: Seq[LarsRule] = rules

  lazy val extendedAtoms: Set[ExtendedAtom] = rules flatMap (r => r.body + r.head) toSet
  lazy val relationalAtoms: Set[RelationAtom] = extendedAtoms collect {
    case r: RelationAtom => r
  }

  lazy val atoms: Set[Atom] = (extendedAtoms map (_.atom)) -- relationalAtoms

  lazy val extensionalAtoms = atoms -- intensionalAtoms -- relationalAtoms

  override def toString(): String = {
    val sb = new StringBuilder
    sb.append("{")
    if (rules.nonEmpty) {
      sb.append(" ").append(rules.head)
      if (rules.size > 1) {
        rules.tail foreach (sb.append("; ").append(_))
      }
      sb.append(" ")
    }
    sb.append("}")
    sb.toString
  }

  def ==(other: LarsProgram): Boolean = {
    this.rules.toSet == other.rules.toSet
  }

  def ++(other: LarsProgram): LarsProgram = LarsProgram(this.rules ++ other.rules)
}

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
  def from(rules: Set[LarsRule]): LarsProgram = LarsProgram(rules.toSeq)
}