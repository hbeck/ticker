package engine.parser.factory

import core.Rule
import core.lars._
import engine.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class RuleFactory(head: AtomTrait, body: List[BodyTrait]) {
  val ruleHead: HeadAtom = createHead(head)
  val posBody: Option[Set[ExtendedAtom]] = createBody(body,false)
  val negBody: Option[Set[ExtendedAtom]] = createBody(body,true)
  val rule: Rule[HeadAtom,ExtendedAtom] = createRule(ruleHead,posBody.getOrElse(Set()),negBody.getOrElse(Set()))

  private def createHead(head: AtomTrait): HeadAtom = {
      head match {
        case a:AtomFactory => if (a.neg) {throw new InvalidSyntaxException("Only positive head atoms are allowed.")} else a.atom
        case at:AtAtomFactory => if (at.neg) {throw new InvalidSyntaxException("Only positive head atoms are allowed.")} else at.atom
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

  private def createRule(head: HeadAtom, pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): Rule[HeadAtom, ExtendedAtom] = {
      if (pos.isEmpty && neg.isEmpty) {
        return LarsFact(head)
      }
      UserDefinedLarsRule(head, pos, neg)
  }
}
