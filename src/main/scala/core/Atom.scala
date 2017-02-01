package core

import core.asp.{AspFact, BuilderHead}
import core.lars._

/**
  * Created by hb on 12/22/15.
  */
sealed trait Atom extends HeadAtom {

  override val atom = this

  def arity = 0

  def isGround(): Boolean

  val predicate: Predicate

}

case class Predicate(caption: String) {
  override def toString = caption

  def apply(arguments: Any*) = Atom(this, arguments map {
    case a: Argument => a
    case x: Any => Argument.convertToArgument(x.toString)
  })
}

trait GroundAtom extends Atom {
  override def isGround(): Boolean = true

  override def assign(assignment: Assignment) = this
}

object Falsum extends GroundAtom {
  override val predicate = Predicate("⊥")
}

//TODO allow with arguments!
case class ContradictionAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString
}

case class PredicateAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString

  private lazy val precomputedHash = scala.runtime.ScalaRunTime._hashCode(PredicateAtom.this)

  override def hashCode(): Int = precomputedHash
}

trait AtomWithArgument extends Atom {

  def arguments: Seq[Argument]

  def positionOf(argument: Argument): Int = arguments.indexOf(argument)

  override def arity = arguments.size

  def ==(other: AtomWithArgument): Boolean = {
    if (this eq other) return true
    if (this.predicate != other.predicate) return false
    if (this.arguments.length != other.arguments.length) return false
    if (!this.arguments.equals(other.arguments)) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case x: AtomWithArgument => this == x
    case _ => false
  }

  override def toString = {
    val sb = new StringBuilder
    sb.append(predicate).append("(")
    arguments.addString(sb, ",")
    sb.append(")")
    sb.toString
  }

  override def isGround(): Boolean = arguments forall (s => s.isInstanceOf[Value])

}

object AtomWithArgument {
  def apply(predicate: Predicate, arguments: Seq[Argument]): AtomWithArgument = arguments.forall(_.isInstanceOf[Value]) match {
    case true => GroundAtomWithArguments(predicate, arguments.map(_.asInstanceOf[Value]).toList)
    case false => NonGroundAtomWithArguments(predicate, arguments)
  }
}

trait NonGroundAtom extends AtomWithArgument

object NonGroundAtom {
  def apply(predicate: Predicate, argument: Seq[Argument]) = NonGroundAtomWithArguments(predicate, argument)
}

// TODO: Naming
case class NonGroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Argument]) extends NonGroundAtom {
  override def assign(assignment: Assignment): Atom = {
    val newArguments = arguments map { arg =>
      assignment(arg) match {
        case Some(value) => value
        case _ => arg
      }
    }
    Atom(predicate, newArguments)
  }
}

case class GroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Value]) extends GroundAtom with AtomWithArgument {
  override def isGround() = true

  private lazy val precomputedHash = scala.runtime.ScalaRunTime._hashCode(GroundAtomWithArguments.this)

  override def hashCode(): Int = precomputedHash
}

object GroundAtom {
  def apply(predicate: Predicate, arguments: Value*): GroundAtom = {
    if (arguments.isEmpty)
      PredicateAtom(predicate)
    else
      GroundAtomWithArguments(predicate, arguments.toList)
  }
}

trait PinnedAtom extends AtomWithArgument {

  val time: Time
  val tick: Argument

  override def positionOf(argument: Argument): Int = argument match {
    case v: Variable => arguments.
      indexWhere(a => a.isInstanceOf[Variable] && a.asInstanceOf[Variable].name == v.name)
    case _ => super.positionOf(argument)
  }

  lazy val pinnedArguments: Seq[Argument] = Seq(time, tick)

  val arguments: Seq[Argument] = atom match {
    case aa: AtomWithArgument => aa.arguments ++ pinnedArguments
    case _ => pinnedArguments
  }

  def assignmentForTime(assignment: Assignment): Time = time match {
    case t: TimePoint => t
    case v: TimeVariableWithOffset => {
      val timeAssign = assignment.apply(time)
      timeAssign.get match {
        case i: IntValue => v.ground(TimePoint(i.int))
        case t: TimePoint => v.ground(t)
        case _ => v
      }
    }
  }

}

trait PinnedAtAtom extends PinnedAtom {
  override val tick: Argument = time

  override lazy val pinnedArguments: Seq[Argument] = Seq(time)

  override val predicate = Predicate(atom.predicate.caption + "_at")
}

trait PinnedTimeTickAtom extends PinnedAtom {
  override val predicate = Predicate(atom.predicate.caption + "_")
}

object PinnedAtom {
  def apply(atom: Atom, time: Time): PinnedAtom = time match {
    case t: TimePoint => ConcretePinnedAtAtom(atom, t)
    case v: TimeVariableWithOffset => VariablePinnedAtAtom(atom, v)
  }

  def apply(atom: Atom, time: Time, tick: Argument): PinnedAtom = (time, tick) match {
    case (t: TimePoint, tv: Value) => ConcreteTimeTickAtom(atom, t, tv)
    case _ => VariableTimeTickAtom(atom, time, tick)
  }
}

case class ConcreteTimeTickAtom(override val atom: Atom, time: TimePoint, tick: Value) extends PinnedTimeTickAtom with GroundAtom

case class ConcretePinnedAtAtom(override val atom: Atom, time: TimePoint) extends PinnedAtAtom with GroundAtom

case class VariablePinnedAtAtom(override val atom: Atom, time: TimeVariableWithOffset) extends PinnedAtAtom with NonGroundAtom {

  override def isGround(): Boolean = false

  //assume pinned atoms may have variables only in its special time argument
  override def assign(assignment: Assignment): ExtendedAtom = {
    val assignedTime = assignmentForTime(assignment)
    PinnedAtom(atom, assignedTime)
  }
}

case class VariableTimeTickAtom(override val atom: Atom, override val time: Time, override val tick: Argument) extends PinnedTimeTickAtom with NonGroundAtom {

  override def isGround(): Boolean = false

  //assume pinned atoms may have variables only in its special time argument
  override def assign(assignment: Assignment): ExtendedAtom = {
    val timeAssign = assignmentForTime(assignment)
    val tickAssign = assignment.apply(tick)

    tickAssign match {
      case Some(t) => PinnedAtom(atom, timeAssign, t)
      case None => PinnedAtom(atom, timeAssign, tick)
    }
  }
}

object Atom {

  def unapply(atom: Atom): Option[Seq[Argument]] = atom match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = PredicateAtom(Predicate(caption))

  def apply(predicate: Predicate, arguments: Seq[Argument]) = arguments.forall(_.isInstanceOf[Value]) match {
    case true => GroundAtom(predicate, arguments.map(_.asInstanceOf[Value]).toList: _*)
    case false => NonGroundAtom(predicate, arguments)
  }

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def asAtomModification(atom: Atom): AtomModification = AtomModification(atom)
}


//auxiliary atom for arithmetic. stripped off rules by Grounder
trait RelationAtom extends Atom {
  //  def holds(): Boolean
}

abstract class BinaryRelationAtom(x: Argument, y: Argument) extends RelationAtom {

  override def isGround(): Boolean = x.isInstanceOf[Value] && y.isInstanceOf[Value]

  override def assign(assignment: Assignment): ExtendedAtom = {
    val xArg: Argument = assignment(x) match {
      case Some(value) => value;
      case _ => x
    }
    val yArg: Argument = assignment(y) match {
      case Some(value) => value;
      case _ => y
    }
    this (xArg, yArg)
  }

}

abstract class BinaryNumericRelationAtom(x: Argument, y: Argument) extends BinaryRelationAtom(x, y) {

  //  def holds(): Boolean = {
  //    if (!x.isInstanceOf[IntValue] || !y.isInstanceOf[IntValue])
  //      return false
  //
  //    val i = x.asInstanceOf[IntValue].int
  //    val j = y.asInstanceOf[IntValue].int
  //
  //    holds(i,j)
  //  }
  //
  //  def holds(i: Int, j: Int): Boolean
}

abstract class TernaryRelationAtom(x: Argument, y: Argument, z: Argument) extends RelationAtom {

  override def isGround(): Boolean = x.isInstanceOf[Value] && y.isInstanceOf[Value] && z.isInstanceOf[Value]

  override def assign(assignment: Assignment): ExtendedAtom = {
    val xArg: Argument = assignment(x) match {
      case Some(value) => value;
      case _ => x
    }
    val yArg: Argument = assignment(y) match {
      case Some(value) => value;
      case _ => y
    }
    val zArg: Argument = assignment(z) match {
      case Some(value) => value;
      case _ => z
    }
    this (xArg, yArg, zArg)
  }
}

abstract class TernaryNumericRelationAtom(x: Argument, y: Argument, z: Argument) extends TernaryRelationAtom(x, y, z) {

  //  def holds(): Boolean = {
  //    if (!x.isInstanceOf[IntValue] || !y.isInstanceOf[IntValue] || !z.isInstanceOf[IntValue])
  //      return false
  //
  //    val i = x.asInstanceOf[IntValue].int
  //    val j = y.asInstanceOf[IntValue].int
  //    val k = z.asInstanceOf[IntValue].int
  //
  //    holds(i,j,k)
  //  }
  //
  //  def holds(i: Int, j: Int, k: Int): Boolean
}

case class Neq(x: Argument, y: Argument) extends BinaryRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("neq")
  //  override def holds(): Boolean = {
  //    if (!x.isInstanceOf[Value] || !y.isInstanceOf[Value])
  //      return false
  //    //just compare based on string:
  //    val xStr = x.asInstanceOf[Value].toString
  //    val yStr = y.asInstanceOf[Value].toString
  //    xStr != yStr
  //  }
}

case class Leq(x: Argument, y: Argument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("leq")
  //  override def holds(i: Int, j: Int): Boolean = i <= j
}

case class Geq(x: Argument, y: Argument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("geq")
  //  override def holds(i: Int, j: Int): Boolean = i >= j
}

case class Lt(x: Argument, y: Argument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("lt")
  //  override def holds(i: Int, j: Int): Boolean = i < j
}

case class Gt(x: Argument, y: Argument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("gt")
  //  override def holds(i: Int, j: Int): Boolean = i > j
}

case class Sum(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i + j == k
  override val predicate: Predicate = Predicate("sum")
}

case class Product(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i * j == k
  override val predicate: Predicate = Predicate("product")
}
