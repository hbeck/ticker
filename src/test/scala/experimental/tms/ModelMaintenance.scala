package experimental.tms

import core.Atom
import core.asp.{AspFact, AspRule}
import fixtures.AtomTestFixture
import org.scalatest.FunSuite
import reasoner.incremental.jtms.algorithms.JtmsDoyle

/**
  * Created by hb on 12.06.18.
  */
class ModelMaintenance extends FunSuite with AtomTestFixture{

  val none = Set[Atom]()

  test("stick with a") {

    val jtms = JtmsDoyle() //not Jtms, which uses the already-in-heuristic
    jtms.add(AspRule(a, none, Set(b)))
    jtms.add(AspRule(b, none, Set(a))) //-> {a} supported by b==out
    jtms.add(AspRule(a, c))
    jtms.add(AspFact(c))

    var model = jtms.getModel.get
    assert(model == Set(a, c))

    jtms.remove(AspFact(c))
    model = jtms.getModel.get
    //theory:
    //assert(model == Set(a) || model == Set(b))

    //practice:
    assert(model == Set(a))

  }

  test("may jump to b") {

    //same program as in "stick with a", but different rule insertion order

    val jtms = JtmsDoyle() //not Jtms, which uses the already-in-heuristic
    jtms.add(AspFact(c))
    jtms.add(AspRule(a, c)) //a supported by c
    jtms.add(AspRule(a, none, Set(b)))
    jtms.add(AspRule(b, none, Set(a)))

    var model = jtms.getModel.get
    assert(model == Set(a, c))

    jtms.remove(AspFact(c))
    model = jtms.getModel.get
    //theory:
    //assert(model == Set(a) || model == Set(b))

    //practice:
    assert(model == Set(b))

  }

  test("different reasons to infer a") {

    val jtms = JtmsDoyle() //not Jtms, which uses the already-in-heuristic
    jtms.add(AspRule(a, none, Set(c))) //a supported by c
    jtms.add(AspRule(a, none, Set(b)))
    jtms.add(AspRule(b, none, Set(a)))

    var model = jtms.getModel.get
    assert(model == Set(a))

    jtms.add(AspFact(c))
    model = jtms.getModel.get
    //theory:
    //assert(model == Set(a,c) || model == Set(b,c))

    //practice:
    assert(model == Set(b,c))

  }

}
