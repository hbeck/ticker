package engine.parser.factories

import core.Rule
import core.lars._
import engine.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class RuleFactory(head: AtomTrait, body: List[BodyTrait]) {
  lazy val ruleHead: HeadAtom = createHead(head)
  lazy val posBody: Option[Set[ExtendedAtom]] = createBody(body,false)
  lazy val negBody: Option[Set[ExtendedAtom]] = createBody(body,true)
  lazy val rule: Rule[HeadAtom,ExtendedAtom] = createRule(ruleHead,posBody.getOrElse(Set()),negBody.getOrElse(Set()))

  @throws[InvalidSyntaxException]
  private def createHead(head: AtomTrait): HeadAtom = {
    if(head.neg) throw new InvalidSyntaxException("Only positive head atoms are allowed.")

      head match {
        case a:AtomFactory => a.atom
        case at:AtAtomFactory => at.atom
      }
  }

  private def createBody(body: List[BodyTrait], neg: Boolean): Option[Set[ExtendedAtom]] = {
    if(body.nonEmpty) {
      return Option(listToAtomSet(body filter {
        case a:AtomTrait => a.neg == neg
        case _:OperationFactory => !neg
      }))
    }
    None
  }

  private def listToAtomSet(list: List[BodyTrait]): Set[ExtendedAtom] = list collect {
        case a:AtomFactory => a.atom
        case a:AtAtomFactory => a.atom
        case a:WAtomFactory => a.atom
        case a:OperationFactory => a.operation
  } toSet

  private def createRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]):
  Rule[HeadAtom, ExtendedAtom] = {
      if (pos.isEmpty && neg.isEmpty) {
        return LarsFact(head)
      }
      UserDefinedLarsRule(head, pos, neg)
  }
}
