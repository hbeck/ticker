package jtms


class JustificationBuilder(I: Set[Node] = Set(), O: Set[Node] = Set()) {
  def in(nodes: Node*) = new JustificationBuilder(I ++ nodes, O)

  def out(nodes: Node*) = new JustificationBuilder(I, O ++ nodes)

  def node(n: Node) = new UserDefinedJustification(I, O, n)
}

object Premise {
  def apply(n: Node) = Justification.premise(n)
}

object Justification {
  def in(nodes: Node*) = new JustificationBuilder(nodes.toSet)

  def out(nodes: Node*) = new JustificationBuilder(Set(), nodes.toSet)

  def premise(n: Node) = new UserDefinedJustification(Set(), Set(), n)
}

sealed trait Justification {
  val I: Set[Node]
  val O: Set[Node]
  val n: Node
}

/**
  * Created by hb on 12/22/15.
  */
case class UserDefinedJustification(I: Set[Node], O: Set[Node], n: Node) extends Justification

case class JustificationFromBacktracking(I: Set[Node], O: Set[Node], n: Node) extends Justification