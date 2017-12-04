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

  override def hashCode(): Int = cachedHash

  lazy val cachedHash = this.toString.hashCode

  override def equals(other: Any): Boolean = other match {
    case a: Atom => this.cachedHash == a.cachedHash
    case _ => false
  }
}

case class Predicate(caption: String) {
  override def toString = caption

  def apply(arguments: Any*): Atom = Atom(this, arguments map {
    case a: Argument => a
    case x: Any => Argument.convertToArgument(x.toString)
  })

}

trait AtomWrapper {
  val atom: Atom
  val predicate = atom.predicate
}

trait GroundAtom extends Atom {
  override def isGround(): Boolean = true

  override def assign(assignment: Assignment): Atom = this
}

object Falsum extends GroundAtom {
  override val predicate = Predicate("⊥")

  override def toString = predicate.toString
}

//(better with arguments)
case class ContradictionAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString
}

case class PredicateAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString
}

trait AtomWithArguments extends Atom {

  val arguments: Seq[Argument]

  def positionOf(argument: Argument): Int = arguments.indexOf(argument)

  override def arity = arguments.size

  override def assign(assignment: Assignment): Atom = {
    val newArguments: Seq[Argument] = this.arguments map { arg =>
      arg match {
        case variable: Variable => assignment(variable) match {
          case Some(value) => value
          case None => variable
        }
        case value: Value => value
      }
    }
    AtomWithArguments(this.predicate, newArguments)
  }


  override def toString = {
    val sb = new StringBuilder
    sb.append(predicate).append("(")
    arguments.addString(sb, ",")
    sb.append(")")
    sb.toString
  }

  override def isGround(): Boolean = arguments forall (_.isInstanceOf[Value])

}

object AtomWithArguments {
  def apply(predicate: Predicate, arguments: Seq[Argument]): AtomWithArguments = arguments.forall(_.isInstanceOf[Value]) match {
    case true => GroundAtomWithArguments(predicate, arguments.map(_.asInstanceOf[Value]).toList)
    case false => NonGroundAtomWithArguments(predicate, arguments)
  }
}

trait NonGroundAtom extends AtomWithArguments

object NonGroundAtom {
  def apply(predicate: Predicate, arguments: Seq[Argument]) = NonGroundAtomWithArguments(predicate, arguments)
}

case class NonGroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Argument]) extends NonGroundAtom {
  override def assign(assignment: Assignment): AtomWithArguments = {
    val newArguments = arguments map { arg =>
      assignment(arg) match {
        case Some(value) => value
        case _ => arg
      }
    }
    AtomWithArguments(predicate, newArguments)
  }

}

case class GroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Value]) extends GroundAtom with AtomWithArguments {
  override def isGround() = true
}

object GroundAtom {
  def apply(predicate: Predicate, arguments: Seq[Value]): GroundAtom = {
    if (arguments.isEmpty)
      PredicateAtom(predicate)
    else
      GroundAtomWithArguments(predicate, arguments.toList)
  }

  def fromArguments(predicate: Predicate, arguments: Value*): GroundAtom = apply(predicate, arguments)

  def assertGround(atom: Atom): GroundAtom = {
    if (!atom.isGround())
      throw new RuntimeException("Atom is not ground!")

    atom match {
      case ground: GroundAtom => ground
      // some atoms could be ground but not constructed as such ==> do an explicit conversion
      case _ => GroundAtom(atom.predicate, Atom.unapply(atom).getOrElse(Seq()).map(_.asInstanceOf[Value]))
    }
  }
}

trait PinnedAtom extends AtomWithArguments {

  //  val tick: Argument

  override def positionOf(argument: Argument): Int = argument match {
    case v: Variable => arguments.
      indexWhere(a => a.isInstanceOf[Variable] && a.asInstanceOf[Variable].name == v.name)
    case _ => super.positionOf(argument)
  }

  val pinnedArguments: Seq[Argument]

  lazy val arguments: Seq[Argument] = atom match {
    case aa: AtomWithArguments => aa.arguments ++ pinnedArguments
    case _ => pinnedArguments
  }
}

//generic class for tick-based view; not used as such due to equivalence with specific @-atom representation
trait PinnedTimeAtom extends PinnedAtom {

  val time: Time

  val pinnedArguments: Seq[Argument] = Seq(time)

  def assignmentForTime(assignment: Assignment): Time = time match {
    case t: TimePoint => t
    case v: TimeVariableWithOffset => {
      val timeAssign = assignment.apply(time)
      timeAssign match {
        case Some(value) => value match {
          case IntValue(i) => TimePoint(i)
          case t: TimePoint => t
          case _ => v
        }
        case _ => v //if time variable does not occur in assignment
      }
    }
  }
}

// a(\vec{X}) --> a_at(\vec{X},T)
trait PinnedAtAtom extends PinnedTimeAtom {
  def resetPin(time: Time): PinnedAtAtom = {
    time match {
      case t: TimePoint if atom.isGround() => GroundPinnedAtAtom(atom, t)
      case t: TimePoint => NonGroundPinnedAtAtom(atom, t)
      case v: TimeVariableWithOffset => NonGroundPinnedAtAtom(atom, v)
    }
  }
}

trait PinnedAtCntAtom extends PinnedAtAtom { // with PinnedCntAtom {
  val cnt: Argument
  override val pinnedArguments = Seq(time, cnt)

  def resetPin(time: Time, count: Argument): PinnedAtCntAtom = {
    (time, count) match {
      case (t: TimePoint, v: Value) => GroundPinnedAtCntAtom(atom, t, v)
      case _ => NonGroundPinnedCntAtAtom(atom, time, count)
    }
  }

  def assignmentForCount(assignment: Assignment): Argument = cnt match {
    case c: IntValue => c
    case v: Variable => {
      val cntAssign = assignment.apply(cnt)
      cntAssign match {
        case Some(value) => value match {
          case i: IntValue => i
          case _ => v
        }
        case _ => v //if time variable does not occur in assignment
      }
    }
  }
}

object PinnedAtom {

  def appendToPredicateCaption(atom: Atom, postFix: String): Atom = {
    val newPredicate = Predicate(atom.predicate.caption + postFix)
    atom match {
      case PredicateAtom(p) => PredicateAtom(newPredicate)
      case GroundAtomWithArguments(p, args) => GroundAtomWithArguments(newPredicate, args)
      case NonGroundAtomWithArguments(p, args) => NonGroundAtomWithArguments(newPredicate, args)
      case _ => throw new RuntimeException("bad use")
    }
  }

  def asPinnedAtAtom(atom: Atom, time: Time): PinnedAtAtom = {
    val newAtom = appendToPredicateCaption(atom, "_at")
    time match {
      case t: TimePoint if atom.isGround() => GroundPinnedAtAtom(newAtom, t)
      case t: TimePoint => NonGroundPinnedAtAtom(newAtom, t)
      case v: TimeVariableWithOffset => NonGroundPinnedAtAtom(newAtom, v)
    }
  }

  def asPinnedAtCntAtom(atom: Atom, time: Time, count: Argument): PinnedAtCntAtom = {
    val newAtom = appendToPredicateCaption(atom, "_at_cnt")
    (time, count) match {
      case (t: TimePoint, tv: Value) => GroundPinnedAtCntAtom(newAtom, t, tv)
      case _ => NonGroundPinnedCntAtAtom(newAtom, time, count)
    }
  }

}

case class GroundPinnedAtCntAtom(override val atom: Atom, time: TimePoint, override val cnt: Value) extends PinnedAtCntAtom with GroundAtom with AtomWrapper

case class GroundPinnedAtAtom(override val atom: Atom, time: TimePoint) extends PinnedAtAtom with GroundAtom with AtomWrapper

//do not distinguish classes for non-ground time variable and non-ground normal variable
case class NonGroundPinnedAtAtom(override val atom: Atom, time: Time) extends PinnedAtAtom with NonGroundAtom with AtomWrapper {

  override def isGround(): Boolean = false

  override def assign(assignment: Assignment): Atom = {
    val assignedTime = assignmentForTime(assignment)
    val assignedAtom = atom.assign(assignment).asInstanceOf[Atom]
    if (assignedAtom.isGround() && assignedTime.isInstanceOf[TimePoint]) {
      GroundPinnedAtAtom(assignedAtom, assignedTime.asInstanceOf[TimePoint])
    } else {
      NonGroundPinnedAtAtom(assignedAtom, assignedTime)
    }
  }
}

case class NonGroundPinnedCntAtAtom(override val atom: Atom, time: Time, cnt: Argument) extends PinnedAtCntAtom with NonGroundAtom with AtomWrapper {

  override def isGround(): Boolean = false

  override def assign(assignment: Assignment): Atom = {
    val assignedTime = assignmentForTime(assignment)
    val assignedCount = assignmentForCount(assignment)
    val assignedAtom = atom.assign(assignment).asInstanceOf[Atom]
    if (assignedAtom.isGround() && assignedTime.isInstanceOf[TimePoint] && assignedCount.isInstanceOf[IntValue]) {
      GroundPinnedAtCntAtom(assignedAtom, assignedTime.asInstanceOf[TimePoint], assignedCount.asInstanceOf[IntValue])
    } else {
      NonGroundPinnedCntAtAtom(assignedAtom, assignedTime, assignedCount)
    }
  }

}

object Atom {

  def unapply(atom: Atom): Option[Seq[Argument]] = atom match {
    case aa: AtomWithArguments => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = PredicateAtom(Predicate(caption))

  def apply(predicate: Predicate): Atom = PredicateAtom(predicate)

  def apply(predicate: Predicate, arguments: Seq[Argument]) = {
    RelationAtom.asRelationAtom(predicate, arguments) match {
      case Some(atom) => atom
      case _ => arguments forall (_.isInstanceOf[Value]) match {
        case true => GroundAtom(predicate, arguments map (_.asInstanceOf[Value]))
        case false => NonGroundAtom(predicate, arguments)
      }
    }
  }

  implicit def headAtomToBuilder(atom: Atom): BuilderHead = new BuilderHead(atom)

  implicit def headAtomToFact(atom: Atom): AspFact[Atom] = AspFact[Atom](atom)

  implicit def asAtomModification(atom: Atom): AtomModification = AtomModification(atom)
}

//
//
//

//auxiliary atom for arithmetic. stripped off rules by Grounder
trait RelationAtom extends AtomWithArguments {
  def holds(): Option[Boolean] = {
    if (!isGround()) return None
    else return Some(groundingHolds())
  }

  def groundingHolds(): Boolean

  def int(arg: Argument) = Integer.parseInt(arg.cachedString)
}

object RelationAtom {
  //return new RelationAtom if the predicate is known as such
  def asRelationAtom(predicate: Predicate, arguments: Seq[Argument]): Option[Atom] = {
    predicate match {
      case Eq.predicate => Some(Eq(arguments(0),arguments(1)))
      case Neq.predicate => Some(Neq(arguments(0),arguments(1)))
      case Leq.predicate => Some(Leq(arguments(0),arguments(1)))
      case Geq.predicate => Some(Geq(arguments(0),arguments(1)))
      case Lt.predicate => Some(Lt(arguments(0),arguments(1)))
      case Gt.predicate => Some(Gt(arguments(0),arguments(1)))
      case Plus.predicate => Some(Plus(arguments(0),arguments(1),arguments(2)))
      case Minus.predicate => Some(Minus(arguments(0),arguments(1),arguments(2)))
      case Times.predicate => Some(Times(arguments(0),arguments(1),arguments(2)))
      case Divide.predicate => Some(Divide(arguments(0),arguments(1),arguments(2)))
      case Modulo.predicate => Some(Modulo(arguments(0),arguments(1),arguments(2)))
      case Power.predicate => Some(Power(arguments(0),arguments(1),arguments(2)))
      case LeqLeq.predicate => Some(LeqLeq(arguments(0),arguments(1),arguments(2)))
      case LtLt.predicate => Some(LtLt(arguments(0),arguments(1),arguments(2)))
      case LtLeq.predicate => Some(LtLeq(arguments(0),arguments(1),arguments(2)))
      case LeqLt.predicate => Some(LeqLt(arguments(0),arguments(1),arguments(2)))
      case Incr.predicate => Some(Incr(arguments(0),arguments(1)))
      case _ => None
    }
  }

}

abstract class BinaryRelationAtom(x: Argument, y: Argument) extends RelationAtom {

  val arguments: Seq[Argument] = Seq(x, y)

  override def isGround(): Boolean = x.isInstanceOf[Value] && y.isInstanceOf[Value]

  def newInstance(nx: Argument, ny: Argument): RelationAtom

  override def assign(assignment: Assignment): Atom = {
    val xArg: Argument = assignment(x) match {
      case Some(value) => value;
      case _ => x
    }
    val yArg: Argument = assignment(y) match {
      case Some(value) => value;
      case _ => y
    }
    newInstance(xArg, yArg)
  }

}

abstract class BinaryNumericRelationAtom(x: NumericArgument, y: NumericArgument) extends BinaryRelationAtom(x, y) {
  override def newInstance(nx: Argument, ny: Argument): RelationAtom
}

abstract class TernaryRelationAtom(x: Argument, y: Argument, z: Argument) extends RelationAtom {

  override def isGround(): Boolean = x.isInstanceOf[Value] && y.isInstanceOf[Value] && z.isInstanceOf[Value]

  def newInstance(nx: Argument, ny: Argument, nz: Argument): RelationAtom

  override def assign(assignment: Assignment): Atom = {
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
    newInstance(xArg, yArg, zArg)
  }
}

abstract class TernaryNumericRelationAtom(x: Argument, y: Argument, z: Argument) extends TernaryRelationAtom(x, y, z) {
  val arguments: Seq[Argument] = Seq(x, y, z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument): RelationAtom
  //= newInstance(nx.asInstanceOf[NumericArgument], ny.asInstanceOf[NumericArgument], nz.asInstanceOf[NumericArgument])
  //def newInstance(x: Argument, y: Argument, nz: NumericArgument)
}

object Eq {
  val predicate: Predicate = Predicate("eq")
}

case class Eq(x: Argument, y: Argument) extends BinaryRelationAtom(x, y) {
  override val predicate: Predicate = Eq.predicate

  override def groundingHolds(): Boolean = x == y

  override def newInstance(nx: Argument, ny: Argument) = Eq(nx, ny)
}

object Neq {
  val predicate: Predicate = Predicate("neq")
}

case class Neq(x: Argument, y: Argument) extends BinaryRelationAtom(x, y) {
  override val predicate: Predicate = Neq.predicate

  override def groundingHolds(): Boolean = x != y

  override def newInstance(nx: Argument, ny: Argument) = Neq(nx, ny)
}

object Leq {
  val predicate: Predicate = Predicate("leq")
}

case class Leq(x: NumericArgument, y: NumericArgument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Leq.predicate

  override def groundingHolds(): Boolean = int(x) <= int(y)

  override def newInstance(nx: Argument, ny: Argument) = Leq(nx, ny)
}

object Geq {
  val predicate: Predicate = Predicate("geq")
}

case class Geq(x: NumericArgument, y: NumericArgument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Geq.predicate

  override def groundingHolds(): Boolean = int(x) >= int(y)

  override def newInstance(nx: Argument, ny: Argument) = Geq(nx, ny)
}

object Lt {
  val predicate: Predicate = Predicate("lt")
}

case class Lt(x: NumericArgument, y: NumericArgument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Lt.predicate

  override def groundingHolds(): Boolean = int(x) < int(y)

  override def newInstance(nx: Argument, ny: Argument) = Lt(nx, ny)
}

object Gt {
  val predicate: Predicate = Predicate("gt")
}

case class Gt(x: NumericArgument, y: NumericArgument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Gt.predicate

  override def groundingHolds(): Boolean = int(x) > int(y)

  override def newInstance(nx: Argument, ny: Argument) = Gt(nx, ny)
}

object Plus {
  val predicate: Predicate = Predicate("plus")
}

case class Plus(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Plus.predicate

  override def groundingHolds(): Boolean = int(x) + int(y) == int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Plus(nx, ny, nz)
}

object Minus {
  val predicate: Predicate = Predicate("minus")
}

case class Minus(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Minus.predicate

  override def groundingHolds(): Boolean = int(x) - int(y) == int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Minus(nx, ny, nz)
}

object Times {
  val predicate: Predicate = Predicate("times")
}

case class Times(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Times.predicate

  override def groundingHolds(): Boolean = int(x) * int(y) == int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Times(nx, ny, nz)
}

object Divide {
  val predicate: Predicate = Predicate("divide")
}

case class Divide(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Divide.predicate

  override def groundingHolds(): Boolean = int(x) / int(y) == int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Divide(nx, ny, nz)
}

object Power {
  val predicate: Predicate = Predicate("power")
}

case class Power(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Power.predicate

  override def groundingHolds(): Boolean = Math.pow(int(x), int(y)).toInt == int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Power(nx, ny, nz)
}

object Modulo {
  val predicate: Predicate = Predicate("modulo")
}

case class Modulo(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = Modulo.predicate

  override def groundingHolds(): Boolean = int(x) > 0 && (int(x) % int(y) == int(z))

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = Modulo(nx, ny, nz)
}

object LeqLeq {
  val predicate: Predicate = Predicate("leqleq")
}

case class LeqLeq(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = LeqLeq.predicate

  override def groundingHolds(): Boolean = int(x) <= int(y) && int(y) <= int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = LeqLeq(nx, ny, nz)
}

object LtLt {
  val predicate: Predicate = Predicate("ltlt")
}

case class LtLt(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = LtLt.predicate

  override def groundingHolds(): Boolean = int(x) < int(y) && int(y) < int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = LtLt(nx, ny, nz)
}

object LtLeq {
  val predicate: Predicate = Predicate("ltleq")
}

case class LtLeq(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = LtLeq.predicate

  override def groundingHolds(): Boolean = int(x) < int(y) && int(y) <= int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = LtLeq(nx, ny, nz)
}

object LeqLt {
  val predicate: Predicate = Predicate("leqlt")
}

case class LeqLt(x: NumericArgument, y: NumericArgument, z: NumericArgument) extends TernaryNumericRelationAtom(x, y, z) {
  override val predicate: Predicate = LeqLt.predicate

  override def groundingHolds(): Boolean = int(x) <= int(y) && int(y) < int(z)

  override def newInstance(nx: Argument, ny: Argument, nz: Argument) = LeqLt(nx, ny, nz)
}

object Incr {
  val predicate: Predicate = Predicate("incr")
}

case class Incr(x: NumericArgument, y: NumericArgument) extends BinaryNumericRelationAtom(x, y) {
  override val predicate: Predicate = Incr.predicate

  override def groundingHolds(): Boolean = int(x) + 1 == int(y)

  override def newInstance(nx: Argument, ny: Argument) = Incr(nx, ny)
}




