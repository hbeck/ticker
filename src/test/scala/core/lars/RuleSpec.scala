package core.lars

import com.sun.prism.impl.BaseResourcePool.Predicate
import core.{Atom, Predicate, Value}
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
    val a = Predicate("a")(Value("1"))
    val b = Predicate("b")(Value("2"))
    val r1 :LarsRule = a <= b
    val r2 :LarsRule = a <= b

    assert(r1.hashCode() == r2.hashCode())
  }
}
