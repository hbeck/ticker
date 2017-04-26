package engine.parser.factory

import core.Atom
import core.lars.{Diamond, SlidingTimeWindow, WindowAtom}
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class WAtomFactoryTest extends FlatSpec {

  behavior of "WAtomFactoryTest"

  it should "create" in {
    val factory = WAtomFactory(false,AtomFactory(false,"a",List()),Diamond,WindowFactory("t"))
    val windowAtom = WindowAtom(SlidingTimeWindow(300),Diamond,Atom("a"))

    assert(factory.atom == windowAtom)
  }

}
