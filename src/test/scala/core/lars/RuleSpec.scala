package core.lars

import core.{Atom, Value}
import org.scalatest.FlatSpec

/**
  * Created by Christian on 24.08.2016.
  */
class RuleSpec extends FlatSpec{
  "Two rules with same atoms" should "have same hashcode"in{
    val a = Atom("a")
    val b = Atom("b")
    val r1 :LarsRule = a <= b
    val r2 :LarsRule = a <= b

    assert(r1.hashCode() == r2.hashCode())
  }

  "Two rules with same atoms and arguments" should "have same hashcode"in{
    val a = Atom("a")(Value("1"))
    val b = Atom("b")(Value("2"))
    val r1 :LarsRule = a <= b
    val r2 :LarsRule = a <= b

    assert(r1.hashCode() == r2.hashCode())
  }
}
