package jtms.asp.examples

import core.Atom
import core.asp.{AspFact, AspRule}
import fixtures.AtomTestFixture
import jtms.algorithms.{JtmsBeierleFixed, JtmsGreedy, JtmsLearn}
import org.scalatest.FunSuite

/**
  * Created by hb on 12.03.16.
  */
class AspConsistency extends FunSuite with AtomTestFixture{

  val O = Set[Atom]()

  //def jtmsImpl = JtmsBeierleFixed
  //def jtmsImpl = JtmsDoyle
  //def jtmsImpl = JtmsGreedy
  def jtmsImpl() = new JtmsLearn()

  val times = 1 to 100

  test("a") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      assert(m.get.isEmpty)

      tms.add(AspFact(a))
      assert(m.get == Set(a))

      tms.remove(AspFact(a))
      assert(m.get == Set())

    }
  }

  test("a :- not b. then b.") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(b)))
      assert(m.get == Set(a))

      tms.add(AspFact(b))
      assert(m.get == Set(b))

      tms.remove(AspFact(b))
      assert(m.get == Set(a))

    }
  }

  test("a :- not b. b :- not a.  b.") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(b)))
      tms.add(AspRule(b, O, Set(a)))
      assert(m.get == Set(a))

      tms.add(AspFact(b))
      assert(m.get == Set(b))

      tms.remove(AspFact(b))
      assert(m.get == Set(a) || m.get == Set(b)) //!

    }
  }

  //consistent: 'inactive' odd loop
  test("a. a :- not a.") {

    times foreach { _ =>

      var tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspFact(a))
      tms.add(AspRule(a, Set(), Set(a)))
      assert(m.get == Set(a))
      tms.remove(AspFact(a))
      assert(m == None)

      tms = jtmsImpl()
      tms.add(AspRule(a, Set(), Set(a)))
      assert(m == None)
      tms.add(AspFact(a))
      assert(m.get == Set(a))

    }
  }

  test("inc1 (odd loop): a :- not b. b :- a") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(b)))
      assert(m.get == Set(a))

      tms.forceChoiceOrder(Seq(a,b))
      tms.add(AspRule(b, a))
      assert(m == None)

      tms.removeChoiceOrder()
      tms.remove(AspRule(b, a))
      assert(m.get == Set(a))

      tms.forceChoiceOrder(Seq(b,a))
      tms.add(AspRule(b,a))
      assert(m == None)

      tms.removeChoiceOrder()
      tms.remove(AspRule(b, a))
      assert(m.get == Set(a))

    }
  }


  test("inc2 (odd loop): a :- not b. b :- not c. c :- not a") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(b)))
      assert(m.get == Set(a))

      tms.add(AspRule(b, O, Set(c)))
      assert(m.get == Set(b))

      tms.add(AspRule(c, O, Set(a)))
      assert(m == None)

      tms.remove(AspRule(c, O, Set(a)))
      assert(m.get == Set(b))

    }
  }

  test("inc3 (odd loop): a :- d, not b, not c. b :- a, d. d :- not e. e.") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, Set(d), Set(b, c)))
      assert(m.get == Set())

      tms.add(AspRule(b, Set(a, d)))
      assert(m.get == Set())

      tms.add(AspRule(d, O, Set(e)))
      assert(m == None)

      tms.add(AspFact(e))
      assert(m.get == Set(e))

      tms.remove(AspFact(e))
      assert(m == None)

    }
  }

  test("inc4 (odd loop c)") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, Set(b, c)))
      tms.add(AspRule(b, Set(d), Set(e)))
      tms.add(AspRule(c, Set(b), Set(f)))

      assert(m.get == Set())

      tms.add(AspRule(d, O, Set(c)))
      assert(m == None)

      tms.add(AspFact(b))
      assert(m.get == Set(a, b, c))

      tms.remove(AspRule(c, Set(b), Set(f)))
      assert(m.get == Set(b, d))

      tms.add(AspRule(c, Set(b), Set(a)))
      assert(m == None)

      tms.add(AspFact(a))
      assert(m.get == Set(a, b, d))

    }
  }

  test("inc5 (odd loop c)") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, Set(b), Set(c)))
      tms.add(AspRule(c, Set(a), Set(d)))
      tms.add(AspRule(b, O, Set(c)))
      assert(m == None)

      tms.add(AspRule(d, O, Set(e)))
      assert(m.get == Set(a, b, d))

      tms.add(AspRule(e, a))
      assert(m == None)

      tms.remove(AspRule(b, O, Set(c)))
      assert(m.get == Set(d))

    }
  }

  test("inc6 (odd loop c)") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, Set(d), Set(c)))
      tms.add(AspRule(b, Set(d), Set(c)))
      tms.add(AspRule(c, Set(a, b)))
      tms.add(AspRule(d, O, Set(e)))
      assert(m == None)

      //notably, this will not work, due to the internal order
      //tms.add(AspRule(e,O,Set(d)))
      //assert(m.get == Set(e))

      //so we can simulate the switch to the right guess:
      tms.remove(AspRule(d, O, Set(e)))
      tms.add(AspRule(e, O, Set(d)))
      assert(m.get == Set(e))

      //now adding the former rule will have no effect
      tms.add(AspRule(d, O, Set(e)))
      assert(m.get == Set(e))

    }
  }

  test("inc7: choice a,b, force b.") {

    times foreach { _ =>
      //illustrates the essence of inc6 more clearly
      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(b)))
      tms.add(AspRule(b, O, Set(a)))
      assert(m.get == Set(a)) //due the order in which rules are inserted

      //due to the order, this update does not work in extended version
      tms.add(AspRule(b,a))
      tms match {
        case x:JtmsBeierleFixed => assert(m.get == Set(b))
        case x:JtmsGreedy => m == None //TODO
        case _ => assert(m.get == Set(b))
      }

      //we can simulate the switch to model by as follows:
      tms.remove(AspRule(a, O, Set(b)))
      assert(m.get == Set(b))
      tms.add(AspRule(a, O, Set(b)))
      assert(m.get == Set(b))
      tms.add(AspRule(b, a))
      assert(m.get == Set(b))

    }
  }

  test("inc8: choice x,y, constraint on x.") {

    //times foreach { _ =>
      //illustrates the essence of inc6 more clearly
      val tms = jtmsImpl()
      if (tms.isInstanceOf[JtmsGreedy]) {
        tms.asInstanceOf[JtmsGreedy].shuffle=false
      }

      def m = tms.getModel

      tms add AspRule(x, O, Set(y))
      tms add AspRule(y, O, Set(x))
      assert(m.get == Set(x)) //due the order in which rules are inserted

      //due to the order, this update does not work in extended version
      tms add AspRule(a,Set(x),Set(a))
      tms match {
        case x:JtmsBeierleFixed => assert(m.get == Set(y))
        case x:JtmsGreedy => assert(m == None) //assert(m.get == Set(y)) //TODO limitations in some insertion orders
        case _ => assert(m.get == Set(y))
      }

      tms remove AspRule(x,O,Set(y))
      assert(m.get == Set(y))

      tms add AspRule(x,O,Set(y))

      tms match {
        case x:JtmsBeierleFixed => assert(m.get == Set(y))
        case x:JtmsGreedy => assert(m.get == Set(y))
        case _ => assert(m.get == Set(y))
      }

    //}
  }

  test("inc9") {

    times foreach { _ =>
      //illustrates the essence of inc6 more clearly
      val tms = jtmsImpl()
      def m = tms.getModel

      tms add AspRule(a, Set(c), Set(b))
      tms add AspRule(b, Set(c), Set(a))
      tms add AspFact(c)

      assert(m.get == Set(b,c) || m.get == Set(a,c))

      tms add AspRule(x,Set(b),Set(x))
      assert(m == None || m.get == Set(a,c))

       /*
      //due to the order, this update does not work in extended version
      tms.add(AspRule(b,a))

      tms match {
        case x:JtmsBeierleFixed => assert(m.get == Set(b))
        case x:JtmsGreedy => m == None //TODO
        case _ => assert(m.get == Set(b))
      }
          */

    }
  }

  test("odd loop 1: a :- not a.") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, O, Set(a)))
      assert(m == None)

      tms.add(AspFact(a))
      assert(m.get == Set(a))

    }
  }

  test("odd loop 2: a :- b. b :- not a.") {

    times foreach { _ =>

      val tms = jtmsImpl()
      def m = tms.getModel

      tms.add(AspRule(a, b))
      tms.add(AspRule(b, O, Set(a)))
      assert(m == None)

      tms.add(AspFact(b))
      assert(m.get == Set(a, b))

      tms.remove(AspFact(b))
      assert(m == None)

      //tms.add(AspRule(a,O,Set(b)))
      //assert(m.get == Set(a)) -- not computed if b is picked first

    }
  }

  test("odd loop 3: a :- b. b :- c. c :- not a.") {

    times foreach { _ =>

    val tms = jtmsImpl()
    def m = tms.getModel

    tms.add(AspRule(a,b))
    tms.add(AspRule(b,c))

    tms.forceChoiceOrder(Seq(c,b,a)) //this is where beierle fails
    tms.add(AspRule(c,O,Set(a)))
    assert(m == None)

    tms.remove(AspRule(a,b))
    assert(m.get == Set(b,c))
    }
  }

}
