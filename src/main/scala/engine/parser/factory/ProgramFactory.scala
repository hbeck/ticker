package engine.parser.factory

/**
  * Created by et on 22.03.17.
  */
case class ProgramFactory(imports: List[ImportFactory], rules: List[RuleFactory]) {

/*  def createRules(rules: List[RuleWrapper]): List[Rule[HeadAtom,ExtendedAtom]] = rules match {
    case Nil => Nil
    case x::xs => x.createRule :: createRules(xs)
  }*/

}
