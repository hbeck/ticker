package engine.parser.factory

import core.Rule
import core.lars.{ExtendedAtom, HeadAtom, LarsProgram}

/**
  * Created by et on 22.03.17.
  */
case class ProgramFactory(imports: List[ImportFactory], ruleLst: List[RuleFactory]) {

  lazy val rules: List[Rule[HeadAtom,ExtendedAtom]] = createRules(ruleLst)
  lazy val program: LarsProgram = createProgram(rules)

  private def createRules(rules: List[RuleFactory]): List[Rule[HeadAtom,ExtendedAtom]] = rules match {
    case Nil => Nil
    case x::xs => x.rule :: createRules(xs)
  }

  private def createProgram(rules: List[Rule[HeadAtom, ExtendedAtom]]): LarsProgram = LarsProgram(rules)
}
