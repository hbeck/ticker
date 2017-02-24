package core.lars

import core.{Atom, Fact, RelationAtom}

/**
  * Created by FM on 01.05.16.
  */
trait LarsBasedProgram {
  val larsRules: Seq[LarsRule]

  lazy val windowAtoms: Set[WindowAtom] = larsRules flatMap {
    _.body collect { case w: WindowAtom => w }
  } toSet

  lazy val atAtoms: Set[HeadAtom] = larsRules flatMap {
    _.atoms collect {
      case a: AtAtom => a
      case WindowAtom(_, At(_), a) => a
    }
  } toSet

  lazy val slidingTimeWindowsAtoms: Set[SlidingTimeWindow] = windowAtoms collect {
    case w: WindowAtom if w.windowFunction.isInstanceOf[SlidingTimeWindow] => w.windowFunction.asInstanceOf[SlidingTimeWindow]
  }
  lazy val slidingTupleWindowsAtoms: Set[SlidingTupleWindow] = windowAtoms collect {
    case w: WindowAtom if w.windowFunction.isInstanceOf[SlidingTupleWindow] => w.windowFunction.asInstanceOf[SlidingTupleWindow]
  }

  lazy val maximumWindowSize: TimeWindowSize = slidingTimeWindowsAtoms.isEmpty match {
    case true => TimeWindowSize(0)
    case false => slidingTimeWindowsAtoms.maxBy(_.windowSize).windowSize
  }
  lazy val maximumTupleWindowSize: TupleCount = slidingTupleWindowsAtoms.isEmpty match {
    case true => 0
    case false => slidingTupleWindowsAtoms.maxBy(_.windowSize).windowSize
  }

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
}

object LarsProgram {
  // apply won't work as we have the same signature as the case class :-/
  def from(rules: LarsRule*): LarsProgram = LarsProgram(rules)
}