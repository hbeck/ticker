package engine.parser.wrapper

import core.Rule
import core.lars.{ExtendedAtom, HeadAtom, LarsProgramBuilder, UserDefinedLarsRule}

/**
  * Created by et on 22.03.17.
  */
case class ProgramWrapper(imports: List[ImportWrapper], rules: List[RuleWrapper]) {

/*  def createRules(rules: List[RuleWrapper]): List[Rule[HeadAtom,ExtendedAtom]] = rules match {
    case Nil => Nil
    case x::xs => x.createRule :: createRules(xs)
  }*/

}
