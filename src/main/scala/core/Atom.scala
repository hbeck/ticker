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
    case a: Atom => this.toString == a.toString
    case _ => false
  }

}

case class Predicate(caption: String) {
  override def toString = caption

  def apply(arguments: Any*) = Atom(this, arguments map {
    case a: Argument => a
    case x: Any => Argument.convertToArgument(x.toString)
  })

  /*
  override def equals(other: Any) = other match {
    case Predicate(p) => this.caption equals p
    case _ => false
  }
  */
}

trait AtomWrapper {
  val atom: Atom
  val predicate = atom.predicate
}

trait GroundAtom extends Atom {
  override def isGround(): Boolean = true
  override def assign(assignment: Assignment) = this
}

object Falsum extends GroundAtom {
  override val predicate = Predicate("⊥")
  override def toString = predicate.toString
}

//TODO allow with arguments!
case class ContradictionAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString
}

case class PredicateAtom(predicate: Predicate) extends GroundAtom {
  override def toString = predicate.toString

  //private lazy val precomputedHash = scala.runtime.ScalaRunTime._hashCode(PredicateAtom.this)

  //override def hashCode(): Int = this.toString.hashCode

  /*
  override def equals(other: Any) = other match {
    case PredicateAtom(p) => this.predicate equals p
    case _ => false
  }
  */
}

trait AtomWithArgument extends Atom {

  def arguments: Seq[Argument]

  def positionOf(argument: Argument): Int = arguments.indexOf(argument)

  override def arity = arguments.size

  /*
  def ==(other: AtomWithArgument): Boolean = {
    if (this eq other) return true
    if (this.predicate != other.predicate) return false
    if (this.arguments.length != other.arguments.length) return false
    if (!this.arguments.equals(other.arguments)) return false
    true
  }
  */

  /*
  override def equals(other: Any): Boolean = other match {
    case x: AtomWithArgument => this == x
    case _ => false
  }
  */

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

  /*
  override def equals(other: Any) = other match {
    case NonGroundAtomWithArguments(pred,_) if (!(this.predicate equals pred)) => false
    case NonGroundAtomWithArguments(_,args) if (this.arguments.length != args.length) => false
    case NonGroundAtomWithArguments(_,args) if ((0 to arguments.length-1) forall (idx => this.arguments(idx) equals args(idx))) => true
    case _ => false
  }
  */
}

case class GroundAtomWithArguments(override val predicate: Predicate, arguments: Seq[Value]) extends GroundAtom with AtomWithArgument {
  override def isGround() = true

  //private lazy val precomputedHash = scala.runtime.ScalaRunTime._hashCode(GroundAtomWithArguments.this)

  //override def hashCode(): Int = this.toString.hashCode

  /*
  override def equals(other: Any) = other match {
    case GroundAtomWithArguments(pred,_) if (!(this.predicate equals pred)) => false
    case GroundAtomWithArguments(_,args) if (this.arguments.length != args.length) => false
    case GroundAtomWithArguments(_,args) if ((0 to arguments.length-1) forall (idx => this.arguments(idx) equals args(idx))) => true
    case _ => false
  }
  */
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

trait PinnedAtom extends AtomWithArgument {

  //  val tick: Argument

  override def positionOf(argument: Argument): Int = argument match {
    case v: Variable => arguments.
      indexWhere(a => a.isInstanceOf[Variable] && a.asInstanceOf[Variable].name == v.name)
    case _ => super.positionOf(argument)
  }

  val pinnedArguments: Seq[Argument]

  lazy val arguments: Seq[Argument] = atom match {
    case aa: AtomWithArgument => aa.arguments ++ pinnedArguments
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
          case i: IntValue => TimePoint(i.int)
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

//// a(\vec{X}) --> a_cnt(\vec{X},C)
//trait PinnedCntAtom extends PinnedAtom {
//  val cnt: Argument
//  override val pinnedArguments: Seq[Argument] = Seq(cnt)
//  def resetPin(count: Argument): PinnedCntAtom = {
//    count match {
//      case v: Variable => NonGroundCountAtom(atom, count)
//      case v: Value => if (atom.isGround()) {
//        GroundCountAtom(atom, v)
//      } else {
//        NonGroundCountAtom(atom, v)
//      }
//    }
//  }
//}

trait PinnedAtCntAtom extends PinnedAtAtom { // with PinnedCntAtom {
  val cnt: Argument
  override val pinnedArguments = Seq(time, cnt)
  def resetPin(time: Time, count: Argument): PinnedAtCntAtom = {
    (time, count) match {
      case (t: TimePoint, v: Value) => GroundAtCntAtom(atom, t, v)
      case _ => VariableAtCntAtom(atom, time, count)
    }
  }
}

object PinnedAtom {

  def appendToPredicateCaption(atom:Atom, postFix:String):Atom = {
    val newPredicate = Predicate(atom.predicate.caption+postFix)
    atom match {
      case PredicateAtom(p) => PredicateAtom(newPredicate)
      case GroundAtomWithArguments(p,args) => GroundAtomWithArguments(newPredicate,args)
      case NonGroundAtomWithArguments(p,args) => NonGroundAtomWithArguments(newPredicate,args)
      case _ => throw new RuntimeException("bad use")
    }
  }

  //TODO better rename to asPinnedAtAtom (below)
  @deprecated
  def apply(atom: Atom, time: Time): PinnedAtAtom = {
    val newAtom = appendToPredicateCaption(atom,"_at")
    time match {
      case t: TimePoint if atom.isGround() => GroundPinnedAtAtom(newAtom, t)
      case t: TimePoint => NonGroundPinnedAtAtom(newAtom, t)
      case v: TimeVariableWithOffset => NonGroundPinnedAtAtom(newAtom, v)
    }
  }

  def asPinnedAtAtom(atom: Atom, time: Time): PinnedAtAtom = {
    val newAtom = appendToPredicateCaption(atom,"_at")
    time match {
      case t: TimePoint if atom.isGround() => GroundPinnedAtAtom(newAtom, t)
      case t: TimePoint => NonGroundPinnedAtAtom(newAtom, t)
      case v: TimeVariableWithOffset => NonGroundPinnedAtAtom(newAtom, v)
    }
  }

  //TODO better rename to asPinnedAtCntAtom (below)
  @deprecated
  def apply(atom: Atom, time: Time, count: Argument): PinnedAtCntAtom = {
    val newAtom = appendToPredicateCaption(atom,"_at_cnt")
    (time, count) match {
      case (t: TimePoint, tv: Value) => GroundAtCntAtom(newAtom, t, tv)
      case _ => VariableAtCntAtom(newAtom, time, count)
    }
  }

  def asPinnedAtCntAtom(atom: Atom, time: Time, count: Argument): PinnedAtCntAtom = {
    val newAtom = appendToPredicateCaption(atom,"_at_cnt")
    (time, count) match {
      case (t: TimePoint, tv: Value) => GroundAtCntAtom(newAtom, t, tv)
      case _ => VariableAtCntAtom(newAtom, time, count)
    }
  }

//  def asPinnedCntAtom(atom: Atom, count: Argument): PinnedCntAtom = {
//    val newAtom = appendToPredicateCaption(atom,"_cnt")
//    count match {
//      case v: Variable => NonGroundCountAtom(newAtom, count)
//      case v: Value => if (atom.isGround()) {
//        GroundCountAtom(newAtom, v)
//      } else {
//        NonGroundCountAtom(newAtom, v)
//      }
//    }
//  }

}

case class GroundAtCntAtom(override val atom: Atom, time: TimePoint, cnt: Value) extends PinnedAtCntAtom with GroundAtom with AtomWrapper

case class GroundPinnedAtAtom(override val atom: Atom, time: TimePoint) extends PinnedAtAtom with GroundAtom with AtomWrapper

//do not distinguish classes for non-ground time variable and non-ground normal variable
case class NonGroundPinnedAtAtom(override val atom: Atom, time: Time) extends PinnedAtAtom with NonGroundAtom with AtomWrapper {

  override def isGround(): Boolean = false

  override def assign(assignment: Assignment): ExtendedAtom = {
    val assignedTime = assignmentForTime(assignment)
    val assignedAtom = atom.assign(assignment).asInstanceOf[Atom]
    if (assignedAtom.isGround() && assignedTime.isInstanceOf[TimePoint]) {
      GroundPinnedAtAtom(assignedAtom, assignedTime.asInstanceOf[TimePoint])
    } else {
      NonGroundPinnedAtAtom(assignedAtom, assignedTime)
    }
  }
}

//case class GroundCountAtom(override val atom: Atom, cnt: Value) extends PinnedCntAtom with GroundAtom with AtomWrapper
//
//case class NonGroundCountAtom(override val atom: Atom, cnt: Argument) extends PinnedCntAtom with NonGroundAtom with AtomWrapper {
//
//  override def isGround(): Boolean = false
//
//  //assume pinned atoms may have variables only in its special time argument TODO
//  override def assign(assignment: Assignment): ExtendedAtom = assignment.apply(cnt) match {
//    case Some(v) => PinnedAtom.asPinnedCntAtom(atom.assign(assignment).asInstanceOf[Atom], v)
//    case None => this
//  }
//}

case class VariableAtCntAtom(override val atom: Atom, override val time: Time, override val cnt: Argument) extends PinnedAtCntAtom with NonGroundAtom with AtomWrapper {

  override def isGround(): Boolean = false

  //assume pinned atoms may have variables only in its special time argument //TODO !!!
  override def assign(assignment: Assignment): ExtendedAtom = {
    val timeAssign = assignmentForTime(assignment)
    val tickAssign = assignment.apply(cnt)

    val assignedAtom = atom.assign(assignment).asInstanceOf[Atom]

    tickAssign match {
      case Some(t) => PinnedAtom(assignedAtom, timeAssign, t)
      case None => PinnedAtom(assignedAtom, timeAssign, cnt)
    }
  }
}

object Atom {

  def unapply(atom: Atom): Option[Seq[Argument]] = atom match {
    case aa: AtomWithArgument => Some(aa.arguments)
    case _ => None
  }

  def apply(caption: String): Atom = PredicateAtom(Predicate(caption))

  def apply(predicate: Predicate) = PredicateAtom(predicate)

  def apply(predicate: Predicate, arguments: Seq[Argument]) = arguments forall (_.isInstanceOf[Value]) match {
    case true => GroundAtom(predicate, arguments map (_.asInstanceOf[Value]))
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

case class Eq(x: Argument, y: Argument) extends BinaryRelationAtom(x, y) {
  override val predicate: Predicate = Predicate("eq")
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
  override val predicate: Predicate = Predicate("prod")
}

// x <= y <= z
case class LeqLeq(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i * j == k
  override val predicate: Predicate = Predicate("leqleq")
}

// x < y < z
case class LeLe(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i * j == k
  override val predicate: Predicate = Predicate("lele")
}

// x < y <= z
case class LeLeq(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i * j == k
  override val predicate: Predicate = Predicate("leleq")
}

// x <= y < z
case class LeqLe(x: Argument, y: Argument, z: Argument) extends TernaryNumericRelationAtom(x, y, z) {
  //  override def holds(i: Int, j: Int, k: Int): Boolean = i * j == k
  override val predicate: Predicate = Predicate("leqle")
}




