package engine.parser
import engine.parser.factory.{SlidingWindowFactory, WindowFactory}

/**
  * Created by et on 01.04.17.
  */
class SlidingWindowParser extends AbstractLarsParser {
  override def customWindow: Parser[WindowFactory] = "[" ~> str ~ opt(space ~> param ~ opt("," ~> param)) <~ "]" ^^ {
    case wType ~ None                            => SlidingWindowFactory(wType)
    case wType ~ params if params.get._2.isEmpty => SlidingWindowFactory(wType,Some(params.get._1))
    case wType ~ params if params.get._2.get     => SlidingWindowFactory(wType,Some(params.get._1),Some(params.get._2.get))
  }
}
