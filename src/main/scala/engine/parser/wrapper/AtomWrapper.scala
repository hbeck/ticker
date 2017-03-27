package engine.parser.wrapper

import core.{Atom, Predicate}
import core.lars.ExtendedAtom

/**
  * Created by et on 21.03.17.
  */
case class AtomWrapper(not: Option[Any], predicate: String, args: List[Any]) extends AtomTrait {

  val atom: Atom = create(predicate,args)
  override val neg: Boolean = not.isDefined

  def create(predicate: String, args: List[Any]): Atom = {
    Predicate(predicate).apply(args)
  }
}
