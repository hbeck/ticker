package core

/**
  * Created by FM on 15.06.16.
  */
trait Rule[THead, TBody] {
  val head: THead
  val pos: Set[TBody]
  val neg: Set[TBody]
  val body = pos union neg
}

trait Fact[THead, TBody] extends Rule[THead, TBody] {
  val pos: Set[TBody] = Set()
  val neg: Set[TBody] = Set()
}