package engine.parser.wrapper

import core.Rule
import core.lars._

/**
  * Created by et on 22.03.17.
  */
case class RuleWrapper(head: Option[AtomTrait],body: Option[BodyWrapper]) {

  val ruleHead: Option[HeadAtom] = createHead(head)
  val posBody: Option[Set[ExtendedAtom]] = createBody(body,false)
  val negBody: Option[Set[ExtendedAtom]] = createBody(body,true)

  def createHead(head: Option[AtomTrait]): Option[HeadAtom] = {
    if (head.isDefined) {
      head.get match {
        case a:AtomWrapper => if (a.neg) {}/*TODO raise error*/ else return Option(a.atom)
        case at:AtAtomWrapper => if (at.neg) {}/*TODO raise error*/ else return Option(at.atom)
      }
    }
    None
  }

  def createBody(body: Option[BodyWrapper], neg: Boolean): Option[Set[ExtendedAtom]] = body match {
    case None => None
    case b: Option[BodyWrapper] =>  Option(wrapperListToAtomSet(b.get.list filter(_.neg == neg)))
  }

  def wrapperListToAtomSet(list: List[AtomTrait]): Set[ExtendedAtom] = {
   list collect {
        case a:AtomWrapper => a.atom
        case a:AtAtomWrapper => a.atom
        case a:WAtomWrapper => a.atom
   } toSet
  }

  def createRule(head: Option[HeadAtom], pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): Rule[HeadAtom, ExtendedAtom] = {
    //TODO handling of head == None
      if (pos.isEmpty && neg.isEmpty) {
        return LarsFact(head.get)
      }
      UserDefinedLarsRule(head.get, pos, neg)
  }
}
