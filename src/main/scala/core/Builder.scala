package core

import core.asp.{NormalRule, UserDefinedAspRule}
import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by FM on 04.05.16.
  */
class BuilderCollection[TAtom <: ExtendedAtom, THead <: HeadAtom](val head: THead, val positiveBody: Set[TAtom] = Set(), val negativeBody: Set[TAtom] = Set()) {

  def not(atom: TAtom) = {
    new BuilderCollection(head, positiveBody, negativeBody ++ Set(atom))
  }

  def and(atom: TAtom) = new BuilderCollection(head, positiveBody ++ Set(atom), negativeBody)
}

object BuilderCollection {
  implicit def toRule(builder: BuilderCollection[Atom, Atom]): NormalRule = new UserDefinedAspRule(builder.head, builder.positiveBody, builder.negativeBody)

  implicit def toLarsRule(builder: BuilderCollection[ExtendedAtom, HeadAtom]): core.lars.Rule = new core.lars.Rule(builder.head, builder.positiveBody, builder.negativeBody)
}

case class not(val atom: Atom)
