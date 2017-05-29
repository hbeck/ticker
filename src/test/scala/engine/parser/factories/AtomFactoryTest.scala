package engine.parser.factories

import core.{Argument, Atom, Predicate, not}
import org.scalatest.FlatSpec

/**
  * Created by et on 22.04.17.
  */
class AtomFactoryTest extends FlatSpec {

  behavior of "AtomFactoryTest"

  it should "create an atom" in {
    val neg = false
    val predicate = "a"
    assert(AtomFactory(neg,predicate,List()).atom == Atom(predicate))
  }

  it should "create an atom with parameters" in {
    val neg = false
    val predicate = "a"
    val A: Argument = "A"
    val B: Argument = "B"
    val C: Argument = "6"
    assert(AtomFactory(neg,predicate,List("A","B",6.0)).atom == Atom(Predicate(predicate),Seq(A,B,C)))
//    assert(AtomFactory(neg,predicate,List("A","B",6.0)).atom == not Atom(Predicate(predicate),Seq(A,B,C)))
  }
}
