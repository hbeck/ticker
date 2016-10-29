package jtms.tmn

import core._
import core.asp._
import jtms.algorithms.JtmsDoyle
import org.scalatest.FlatSpec

import scala.collection.immutable.Stream.#::

/**
  * Created by FM on 17.03.16.
  */
class AspBuilderTests extends FlatSpec {

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

    val j1 = a :- c // Rule.pos(c).head(a) //Rule(a,c)  // a :- c
    val j2 = b :- not( a) //Rule.neg(a).head(b) //Rule(b,none,Set(a))
    val j3 = AspRule.pos(a).head(c)
    val j4a = AspRule.pos(b).head(d)
    val j4b = AspRule.pos(c).head(d)
    val j5 = AspFact(e)
    val j6 = AspRule.pos(c, e).head(f)

    //    val program2 = ProgramBuilder(j2)(j3)(j4a)
    val program3 = AspProgramBuilder rule
      j1 rule
      a :- b rule
      c :- d
    //   d :- d and b and not (c) and not(d)
    //    .rule(j3)
    //  j4a
    //  j4b
    //  j5
    //  j6)

    val program4 = AspProgramBuilder
      .rule(a :- c and d not (e) not (f))
      .rule(Falsum :- d not (e))
    //    .rule(j3)
    //  j4a
    //  j4b
    //  j5
    //  j6)

    val program5 = AspProgramBuilder rule (a :- c and d not (e) not (f))

    val p2 = program3.toProgram
    assert(p2.rules.size == 3)
  }

  "a :- b, not c. c :- b. b :- not d. :- d. b." should "be a valid program" in {

    val program = AspProgramBuilder({
      case a #:: b #:: c #:: d #:: atoms => Set(
        a :- b not (c("1", "a")),
        c :- b("1", "a"),
        b :- not(d) not (d),
        Falsum :- d,
        //        :- d,
        b
      )
    })

    val tmn = JtmsDoyle(program) //TODO

    assert(tmn.getModel().isDefined)
  }

  "simple atoms from stream" should "generate a model with 6 facts" in {
    val atoms = Stream.iterate(0)(x => x + 1).map(x => Atom("atom" + x))
    val program = testProgram(atoms)
    assert(program.size == 6)
  }

  val testProgram: PartialFunction[Seq[Atom], Set[AspRule[Atom]]] = {
    case a #:: b #:: c #:: d #:: e #:: f #:: atoms => Set(
      AspRule.fact(a),
      AspRule.fact(b),
      AspRule.fact(c),
      AspRule.fact(d),
      AspRule.fact(e),
      AspRule.fact(f)
    )
  }
}
