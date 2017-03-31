package core

import core.lars.{Assignment, ExtendedAtom, HeadAtom, TimeVariableWithOffset}

/**
  * Created by FM on 15.06.16.
  */
trait Rule[THead <: HeadAtom, TBody <: ExtendedAtom] {
  val head: THead
  val pos: Set[TBody]
  val neg: Set[TBody]

  lazy val body = pos union neg

  lazy val isFact: Boolean = pos.isEmpty && neg.isEmpty

  //lazy val atoms: = body ++ Set(head) //TODO type
  def atoms: Set[TBody]

  lazy val isGround: Boolean = atoms forall (_.isGround)

  def assign(assignment: Assignment): Rule[THead, TBody] = {
    val assignedHead: THead = head.assign(assignment).asInstanceOf[THead]
    val assignedPosBody = pos map (_.assign(assignment).asInstanceOf[TBody])
    val assignedNegBody = neg map (_.assign(assignment).asInstanceOf[TBody])
    from(assignedHead, assignedPosBody, assignedNegBody)
  }

  lazy val variables: Set[Variable] = {
    atoms flatMap {
      case a: AtomWithArguments => a.arguments collect {
        case t: TimeVariableWithOffset => t.variable
        case vv: VariableWithOffset => vv.variable
        case v: Variable => v
      }
      case _ => Set()
    }
  }

  //naming it 'apply' causes problems in case classes (ambiguity with use of constructor)
  def from(head: THead, pos: Set[TBody], neg: Set[TBody]): Rule[THead, TBody]

  def ==(other: Rule[THead, TBody]): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case r: Rule[THead, TBody] => this == r
    case _ => {
      println("this:  " + this.getClass)
      println("other: " + other.getClass)
      false
    }
  }

  override def toString(): String = {
    val sb = new StringBuilder

    def result: String = sb.toString

    sb.append(head)
    if (pos.isEmpty && neg.isEmpty) {
      return result
    }
    sb.append(" :- ")

    //pos
    if (pos.size == 1) {
      sb.append(pos.head)
    } else if (pos.size > 1) {
      sb.append(pos.head)
      pos.tail foreach (sb.append(", ").append(_))
    }

    if (neg.isEmpty) {
      return result
    }
    if (pos.nonEmpty) {
      sb.append(", not ")
    }

    //neg
    if (neg.size == 1) {
      sb.append(neg.head)
    } else if (neg.size > 1) {
      sb.append(neg.head)
      neg.tail foreach (sb.append(", not ").append(_))
    }

    return result
  }
}

trait Fact[THead <: HeadAtom, TBody <: ExtendedAtom] extends Rule[THead, TBody] {
  val pos: Set[TBody] = Set()
  val neg: Set[TBody] = Set()
  //override def isGround(): Boolean = head.isGround()
  override lazy val isFact: Boolean = true
}