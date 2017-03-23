package engine.parser.wrapper

import core.Rule
import core.lars.{ExtendedAtom, HeadAtom}

/**
  * Created by et on 22.03.17.
  */
case class RuleWrapper(head: Option[AtomTrait],body: Option[BodyWrapper]) {

  val ruleHead: Option[HeadAtom] = createHead(head)
  val posBody: Option[Set[ExtendedAtom]] = None
  val negBody: Option[Set[ExtendedAtom]] = None

  def createHead(head: Option[AtomTrait]): Option[HeadAtom] = {
    if (head.isDefined) {
      head.get match {
        case a:AtomWrapper => if (a.neg) {}/*TODO raise error*/ else return Option(a.atom)
        case at:AtAtomWrapper => if (at.neg) {}/*TODO raise error*/ else return Option(at.atAtom)
      }
    }
    None
  }

  def createPosBody(body: Option[BodyWrapper]): Option[Set[ExtendedAtom]] = {
    if (body.isDefined) {

    }
    None
  }

  def createBody(head: Option[HeadAtom], pos: Set[ExtendedAtom], neg: Set[ExtendedAtom]): Rule[HeadAtom, ExtendedAtom] = {
  }
}
