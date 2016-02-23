package jtms

/**
  * Created by hb on 12/22/15.
  */
sealed trait Node {
  def caption: String
}

object Node {
  def apply(caption: String) = UserDefinedNode(caption)
}

case class UserDefinedNode(caption: String) extends Node

case class ContradictionNode(caption: String) extends Node