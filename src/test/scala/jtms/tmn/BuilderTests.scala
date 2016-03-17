package jtms.tmn

import core.{Rule, Atom}
import org.scalatest.FlatSpec

/**
  * Created by FM on 17.03.16.
  */
class BuilderTests extends FlatSpec {

//  def bar(atoms: List[Atom]) = {
//    atoms match {
//
//    }
//  }

  "foo" should "work" in {
    val atoms = List(Atom("0"), Atom("a"), Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"))
    var program = bar(atoms)
    assert(program.size == 6)
  }

  val bar: PartialFunction[List[Atom], Set[Rule]] = {
    case a :: b :: c :: d :: e :: f :: nil => Set(
      Rule.fact(a),
      Rule.fact(b),
      Rule.fact(c),
      Rule.fact(d),
      Rule.fact(e),
      Rule.fact(f)
    )
  }


}
