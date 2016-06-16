package core

import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by FM on 15.06.16.
  */
trait Rule[THead <: HeadAtom, TBody<: ExtendedAtom] {
  val head: THead
  val pos: Set[TBody]
  val neg: Set[TBody]
  lazy val body = pos union neg
}

trait Fact[THead <: HeadAtom, TBody<: ExtendedAtom] extends Rule[THead, TBody] {
  val pos: Set[TBody] = Set()
  val neg: Set[TBody] = Set()
}