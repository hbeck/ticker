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
    //List.range()

    //        val atoms = List(Atom("0"), Atom("a"), Atom("b"), Atom("c"), Atom("d"), Atom("e"), Atom("f"), Atom("g"))
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))
    //    val program = bar(atoms.take( 10).toList)
    val program = bar(atoms)
    assert(program.size == 6)
  }

  val bar: PartialFunction[Seq[Atom], Set[Rule]] = {
    case a #:: b #:: c #:: d #:: e #:: f #:: atoms => Set(
      Rule.fact(a),
      Rule.fact(b),
      Rule.fact(c),
      Rule.fact(d),
      Rule.fact(e),
      Rule.fact(f)
    )
  }


}
