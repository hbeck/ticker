package core

import core.asp.{UserDefinedAspRule, AspRule}
import core.lars.{HeadAtom, ExtendedAtom}

/**
  * Created by FM on 04.05.16.
  */


//class BuilderCollection[TAtom <: ExtendedAtom, THead <: HeadAtom](val head: THead, val positiveBody: Set[TAtom] = Set(), val negativeBody: Set[TAtom] = Set()) {
//
//  //  def and(builderItem: BuilderItem[TAtom]) = builderItem match {
//  //    case PosBuilderAtom(atom) => new BuilderCollection(head, positiveBody ++ Set[TAtom](atom), negativeBody)
//  //    case NegBuilderAtom(atom) => new BuilderCollection(head, positiveBody, negativeBody ++ Set[TAtom](atom))
//  //  }
//
//  def and = new NotBuilderCollection(head, positiveBody, negativeBody)
//
//
//  //  def not[TAtom <: ExtendedAtom](posBuilderAtom: PosBuilderAtom[TAtom]): NegBuilderAtom[TAtom] = NegBuilderAtom(posBuilderAtom.atom)
//  //
//  //  def not(atom: Atom): NegBuilderAtom[Atom] = NegBuilderAtom[Atom](atom)
//}

class NotBuilderCollection[TAtom <: ExtendedAtom, THead <: HeadAtom](val head: THead, val positiveBody: Set[TAtom] = Set(), val negativeBody: Set[TAtom] = Set()) {

  //  def apply(atom: TAtom) = new NotBuilderCollection(head, positiveBody ++ Set[TAtom](atom), negativeBody)

  def not(atom: TAtom) = {
    new NotBuilderCollection(head, positiveBody, negativeBody ++ Set[TAtom](atom))
  }

  def and(atom: TAtom) = new NotBuilderCollection(head, positiveBody ++ Set(atom), negativeBody)
}

object NotBuilderCollection {
  implicit def toRule(builder: NotBuilderCollection[Atom, Atom]): AspRule = new UserDefinedAspRule(builder.head, builder.positiveBody, builder.negativeBody)

  implicit def toLarsRule(builder: NotBuilderCollection[ExtendedAtom, HeadAtom]): core.lars.Rule = new core.lars.Rule(builder.head, builder.positiveBody, builder.negativeBody)
}

case class not(val atom: Atom)

//object not {
//  def apply(atom: Atom): NegBuilderAtom[Atom] = NegBuilderAtom[Atom](atom)
//
//  def apply[TAtom <: ExtendedAtom](posBuilderAtom: PosBuilderAtom[TAtom]): NegBuilderAtom[TAtom] = NegBuilderAtom(posBuilderAtom.atom)
//}

object BuilderItem {
  implicit def toBuilderItem(atom: Atom): PosBuilderAtom[Atom] = new PosBuilderAtom[Atom](atom)
}

sealed trait BuilderItem[TAtom <: ExtendedAtom]

case class PosBuilderAtom[TAtom <: ExtendedAtom](atom: TAtom) extends BuilderItem[TAtom]

case class NegBuilderAtom[TAtom <: ExtendedAtom](atom: TAtom) extends BuilderItem[TAtom]
