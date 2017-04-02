package engine.parser.factory

import core.Rule
import core.lars.{ExtendedAtom, HeadAtom, LarsProgram}

/**
  * Created by et on 22.03.17.
  */
case class ProgramFactory(imports: List[ImportFactory], ruleLst: List[RuleFactory]) {

  val rules: List[Rule[HeadAtom,ExtendedAtom]] = createRules(ruleLst)
  val program: LarsProgram = createProgram(rules)

  //TODO i am not sure if i actually need this here, because ImportFactory has a companion object, which allows me to use the object's methods from anywhere
//  def createImports(imports: List[ImportFactory]): Unit

  def createRules(rules: List[RuleFactory]): List[Rule[HeadAtom,ExtendedAtom]] = rules match {
    case Nil => Nil
    case x::xs => x.rule :: createRules(xs)
  }

  def createProgram(rules: List[Rule[HeadAtom, ExtendedAtom]]): LarsProgram = LarsProgram(rules)

}
