package engine.parser.factory

import core.Rule
import core.lars._

/**
  * Created by et on 22.03.17.
  */
case class RuleFactory(head: Option[AtomTrait], body: Option[BodyFactory]) {
  val ruleHead: Option[HeadAtom] = createHead(head)
  val posBody: Option[Set[ExtendedAtom]] = createBody(body,false)
  val negBody: Option[Set[ExtendedAtom]] = createBody(body,true)

  def createHead(head: Option[AtomTrait]): Option[HeadAtom] = {
    if (head.isDefined) {
      head.get match {
        case a:AtomFactory => if (a.neg) {}/*TODO raise error*/ else return Option(a.atom)
        case at:AtAtomFactory => if (at.neg) {}/*TODO raise error*/ else return Option(at.atom)
      }
    }
    None
  }

  def createBody(body: Option[BodyFactory], neg: Boolean): Option[Set[ExtendedAtom]] = body match {
    case None => None
    case b: Option[BodyFactory] =>  Option(wrapperListToAtomSet(b.get.list filter {
                                        case a:AtomTrait => a.neg == neg
                                    }))
  }

  def wrapperListToAtomSet(list: List[BodyTrait]): Set[ExtendedAtom] = {
   list collect {
        case a:AtomFactory => a.atom
        case a:AtAtomFactory => a.atom
        case a:WAtomFactory => a.atom
   } toSet
  }

  def createRule(head: Option[HeadAtom], pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): Rule[HeadAtom, ExtendedAtom] = {
    //TODO handling of head == None
      if (pos.isEmpty && neg.isEmpty) {
        return LarsFact(head.get)
      }
      UserDefinedLarsRule(head.get, pos, neg)
  }

  def getRule: Rule[HeadAtom,ExtendedAtom] = createRule(ruleHead,posBody.getOrElse(Set()),negBody.getOrElse(Set()))

}