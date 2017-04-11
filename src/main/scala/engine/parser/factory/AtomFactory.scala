package engine.parser.factory

import core.{Argument, Atom, Predicate}

/**
  * Created by et on 21.03.17.
  */
case class AtomFactory(override val neg: Boolean, predicate: String, args: List[Any]) extends AtomTrait {

  val atom: Atom = create(predicate,args)

  private def create(predicateStr: String, args: List[Any]): Atom = {
    val predicate = Predicate(predicateStr)

    argSeq(args) match {
      case Seq() => Atom(predicate)
      case a:Seq[Argument] => Atom(predicate,a)
    }
  }

  private def argSeq(args: List[Any]): Seq[Argument] = args collect {
      case arg:Double => Argument.convertToArgument(arg.toInt.toString)
      case arg:Char => Argument.convertToArgument(arg.toString)
  }
}
