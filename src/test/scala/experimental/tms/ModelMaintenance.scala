package experimental.tms

import core.Atom
import core.asp.{AspFact, AspRule}
import fixtures.AtomTestFixture
import org.scalatest.FunSuite
import reasoner.incremental.jtms.algorithms.JtmsDoyle

import scala.util.Random

/**
  * Created by hb on 12.06.18.
  */
class ModelMaintenance extends FunSuite with AtomTestFixture{

  val none = Set[Atom]()

  test("stick with a") {

    var n=0

    for (i <- 1 to 1000) {

      val random = new Random(i)

      val jtms = JtmsDoyle(random) //not Jtms, which uses the already-in-heuristic
      jtms.add(AspRule(a, none, Set(b)))
      jtms.add(AspRule(b, none, Set(a))) //-> {a} supported by b==out
      jtms.add(AspRule(a, c))
      jtms.add(AspFact(c))

      var model = jtms.getModel.get
      assert(model == Set(a, c))

      jtms.remove(AspFact(c))
      model = jtms.getModel.get
      assert(model == Set(a) || model == Set(b))

      if (model == Set(b)) {
        n = n + 1
      }
    }

    assert(n == 0)

  }

  test("may jump to b") {

    //same program as in "stick with a", but different rule insertion order

    var n=0

    for (i <- 1 to 1000) {

      val random = new Random(i)

      val jtms = JtmsDoyle(random) //not Jtms, which uses the already-in-heuristic
      jtms.add(AspFact(c))
      jtms.add(AspRule(a, c)) //a supported by c
      jtms.add(AspRule(a, none, Set(b)))
      jtms.add(AspRule(b, none, Set(a)))

      var model = jtms.getModel.get
      assert(model == Set(a, c))

      jtms.remove(AspFact(c))
      model = jtms.getModel.get
      assert(model == Set(a) || model == Set(b))

      if (model == Set(b)) {
        n = n + 1
      }

    }

    assert(n == 1000)

  }

}
