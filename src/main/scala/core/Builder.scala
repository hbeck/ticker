package core

import core.asp.{NormalRule, UserDefinedAspRule}
import core.lars.{ExtendedAtom, HeadAtom, WindowAtom}

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

  implicit def toLarsRule(builder: BuilderCollection[ExtendedAtom, HeadAtom]): core.lars.UserDefinedLarsRule = new core.lars.UserDefinedLarsRule(builder.head, builder.positiveBody, builder.negativeBody)
}

case class not[TAtom <: ExtendedAtom](atom: TAtom)

object not {
  def apply(w: WindowAtom) = not[ExtendedAtom](w)
}