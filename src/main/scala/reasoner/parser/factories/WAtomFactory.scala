package reasoner.parser.factories

import core.Variable
import core.lars._
import reasoner.parser.InvalidSyntaxException

/**
  * Created by et on 22.03.17.
  */
case class WAtomFactory(not: Boolean = false, predicate: AtomTrait, tempMod: TemporalModality,
                        window: WindowFactory) extends AtomTrait {

  lazy val atom: WindowAtom = create(predicate,tempMod,window)
  override val neg: Boolean = not

  @throws[InvalidSyntaxException]
  private def create(predicate: AtomTrait, mod: TemporalModality, window: WindowFactory): WindowAtom =
    predicate match {
      case at: AtAtomFactory  => WindowAtom(window.wfn,At(Variable(at.time)),at.atom.atom)
      case a: AtomFactory     => WindowAtom(window.wfn,mod,a.atom)
      case _                  => throw new InvalidSyntaxException("Unknown atom cannot be parsed.")
  }
}
