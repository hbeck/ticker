package jtms.tmn

import core._
import jtms.TMN
import org.scalatest.FlatSpec

import scala.collection.immutable.Stream.#::

/**
  * Created by FM on 17.03.16.
  */
class BuilderTests extends FlatSpec {

  //  def bar(atoms: List[Atom]) = {
  //    atoms match {
  //
  //    }
  //  }

  "simple test cases" should "compile correctly" in {

    val a = Atom("a")
    val b = Atom("b")
    val c = Atom("c")
    val d = Atom("d")
    val e = Atom("e")
    val f = Atom("f")

    val j1 = Rule.pos(c).head(a)
    val j2 = Rule.neg(a).head(b)
    val j3 = Rule.pos(a).head(c)
    val j4a = Rule.pos(b).head(d)
    val j4b = Rule.pos(c).head(d)
    val j5 = Fact(e)
    val j6 = Rule.pos(c, e).head(f)

    //    val program2 = ProgramBuilder(j2)(j3)(j4a)
    val program3 = ProgramBuilder rule
      j1 rule
      a :- b rule
      c :- d
    //    .rule(j3)
    //  j4a
    //  j4b
    //  j5
    //  j6)

    val program4 = ProgramBuilder
      .rule(a :- c and d and not(e) and not(f))
      .rule(:-(d) and not(e))
    //    .rule(j3)
    //  j4a
    //  j4b
    //  j5
    //  j6)

    val program5 = ProgramBuilder rule (a :- c and d and not(e) and not(f))

    val p2 = program3.toProgram
    assert(p2.rules.size == 3)
  }

  "a :- b, not c. c :- b. b :- not d. :- d. b." should "be definable as a program" in {

    val program = ProgramBuilder({
      case a #:: b #:: c #:: d #:: atoms => Set(
        a :- b and not(c),
        c :- b,
        b :- not(d),
        :-(d),
        //        :- d,
        b
      )
    })

    val tmn = TMN(program)

    assert(tmn.getModel().isDefined)
  }

  "simple atoms from stream" should "generate a model with 6 facts" in {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))
    val program = testProgram(atoms)
    assert(program.size == 6)
  }

  val testProgram: PartialFunction[Seq[Atom], Set[Rule]] = {
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
