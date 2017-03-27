package engine.parser.wrapper

/**
  * Created by et on 22.03.17.
  */
case class ProgramWrapper(imports: List[ImportWrapper], rules: List[RuleWrapper]) {

/*  def createRules(rules: List[RuleWrapper]): List[Rule[HeadAtom,ExtendedAtom]] = rules match {
    case Nil => Nil
    case x::xs => x.createRule :: createRules(xs)
  }*/

}
