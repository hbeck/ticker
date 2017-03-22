package engine.parser.wrapper

/**
  * Created by et on 21.03.17.
  */
case class AtomWrapper(not: Option[Any], predicate: String, args: List[Any]) extends AtomTrait {

  /*  {
                case pre~dicate~None => Atom(pre.toString+dicate.toString)
                case pre~dicate~params => Atom(Predicate(pre.toString+dicate.toString),params.map(a => Argument.convertToArgument(a.toString())).toSeq)
    }*/
}
