package core

import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by FM on 15.06.16.
  */
trait Rule[THead <: HeadAtom, TBody <: ExtendedAtom] {
  val head: THead
  val pos: Set[TBody]
  val neg: Set[TBody]

  lazy val body = pos union neg

  lazy val isFact: Boolean = pos.isEmpty && neg.isEmpty

  def ==(other: Rule[THead, TBody]): Boolean = {
    if (this.head != other.head) return false
    if (this.pos != other.pos) return false
    if (this.neg != other.neg) return false
    true
  }

  override def equals(other: Any): Boolean = other match {
    case r: Rule[THead, TBody] => this == r
    case _ => {
      println("this:  "+this.getClass)
      println("other: "+other.getClass)
      false
    }
  }
}

trait Fact[THead <: HeadAtom, TBody <: ExtendedAtom] extends Rule[THead, TBody] {
  val pos: Set[TBody] = Set()
  val neg: Set[TBody] = Set()
}