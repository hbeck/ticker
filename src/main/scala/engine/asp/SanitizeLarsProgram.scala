package engine.asp

import core.Atom
import core.lars.{Diamond, ExtendedAtom, LarsProgram, LarsRule, W}

/**
  * Created by FM on 12.08.16.
  */
case class SanitizeLarsProgram(program: LarsProgram) {
  val intensionalAtoms = program.rules map (_.head) toSet
  val extensionalAtoms: Set[ExtendedAtom] = program.atoms diff intensionalAtoms.toSet[ExtendedAtom]

  val sanitizedProgram = LarsProgram(program.rules map sanitize)

  def sanitize(rule: LarsRule): LarsRule = {
    if (rule.body intersect extensionalAtoms nonEmpty) {
      LarsRule(
        rule.head,
        rule.pos map sanitize,
        rule.neg map sanitize
      )
    }
    rule
  }

  def sanitize(atom: ExtendedAtom): ExtendedAtom = atom match {
    case a: Atom if extensionalAtoms.contains(a) => W(0, Diamond, a)
    case _ => atom
  }

}
