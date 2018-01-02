package reasoner.parser.factories

import core.{Atom, Predicate}
import core.lars.{AtAtom, Time}
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class AtAtomFactoryTest extends FlatSpec {

  behavior of "AtAtomFactoryTest"

  it should "create" in {
    val neg = false
    val predicate = "a"
    val factory = AtAtomFactory(neg,AtomFactory(neg,predicate,List()),"25000")
    val atAtom = AtAtom(Time.convertToTimePoint(25000),Atom(Predicate(predicate)))
    assert(factory.atom == atAtom)
  }

}
