package engine

import core._
import core.lars._
import fixtures.JtmsIncrementalEngine
import org.scalatest.FunSuite

/**
  * Created by hb on 02.03.17.
  */
class IncrementalTmsTests extends FunSuite with JtmsIncrementalEngine {

  test("test 1") {

    val b = Atom(Predicate("b"),Seq(StringVariable("X")))
    val h = Atom(Predicate("h"),Seq(StringVariable("X")))
    val g = Atom(Predicate("g"),Seq(StringVariable("X")))
    val gy = Atom(Predicate("g"),Seq(StringValue("y")))

    //h(X) :- win2 D b(X)
    val program = LarsProgram.from(
      h <= g and WindowAtom(SlidingTimeWindow(2), Diamond, b),
      gy
    )

    val by = Atom(Predicate("b"),Seq(StringValue("y")))
    val hy = Atom(Predicate("h"),Seq(StringValue("y")))


    println(program)

    val engine = defaultEngine(program)

    var model = engine.evaluate(TimePoint(0)).model
    assert(model.size == 0) // due to filter on intensional atoms + signals

    //b(y)
    val signal = Atom(Predicate("b"),Seq(StringValue("y")))

    engine.append(TimePoint(0))(signal)

    model = engine.evaluate(TimePoint(0)).model
    println(model)
    assert(model contains signal)

    val inference = Atom(Predicate("h"),Seq(StringValue("y")))

    println("evaluate 0")
    model = engine.evaluate(TimePoint(0)).model
    assert(model contains inference)

    println("evaluate 1")
    model = engine.evaluate(TimePoint(1)).model
    assert(model contains inference)

    println("evaluate 2")
    model = engine.evaluate(TimePoint(2)).model
    assert(model contains inference)

    println("evaluate 3 (time incr)")
    model = engine.evaluate(TimePoint(3)).model
    assert(!(model contains inference))

    //additional
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("1")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("2")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("0")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("1")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("2")))))
    assert(!(model contains Atom(Predicate("h_at"),Seq(StringValue("y"),StringValue("3")))))

    println("evaluate 3 (count incr with b(y))")
    engine.append(TimePoint(3))(signal)
    model = engine.evaluate(TimePoint(3)).model

    assert(model contains inference)
    assert(model contains Atom(Predicate("b"),Seq(StringValue("y"))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))))
    assert(!(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2")))))

    println("evaluate 5")
    model = engine.evaluate(TimePoint(5)).model

    assert(model contains inference)
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))))
    assert(!(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2")))))

    println("evaluate 6")
    model = engine.evaluate(TimePoint(6)).model

    assert(!(model contains inference))
    assert(!(model contains Atom(Predicate("b"),Seq(StringValue("y")))))
    assert(!(model contains Atom(Predicate("b_at"),Seq(StringValue("y"),StringValue("3")))))
    assert(!(model contains Atom(Predicate("b_at_cnt"),Seq(StringValue("y"),StringValue("3"),StringValue("2")))))

  }

  val p = Atom(Predicate("p"))
  val p1 = Atom(Predicate("p1"))
  val p2 = Atom(Predicate("p2"))
  val t_dp = Atom(Predicate("t_dp"))
  val t_bp = Atom(Predicate("t_bp"))
  val t_a3p = Atom(Predicate("t_a3p"))
  val t_aTp = Atom(Predicate("t_aTp"))
  val c_dp = Atom(Predicate("c_dp"))
  val c_bp = Atom(Predicate("c_bp"))
  val c_a3p = Atom(Predicate("c_a3p"))
  val c_aTp = Atom(Predicate("c_aTp"))
  val h_p = Atom(Predicate("h_p"))
  val h_a3p = Atom(Predicate("h_a3p"))
  val h_aTp = Atom(Predicate("h_aTp"))
  val n_t_bp = Atom(Predicate("n_t_bp"))
  val n_t_dp = Atom(Predicate("n_t_dp"))
  val n_t_a3p = Atom(Predicate("n_t_a3p"))
  //val qn4 = Atom(Predicate("qn4"))
  val n_c_bp = Atom(Predicate("n_c_bp"))
  val n_c_dp = Atom(Predicate("n_c_dp"))
  val n_c_a3p = Atom(Predicate("n_c_a3p"))
  //val qnb8 = Atom(Predicate("qnb8"))
  val n_h_p = Atom(Predicate("n_h_p"))
  val n_h_a3p = Atom(Predicate("n_h_a3p"))
  //val qn11 = Atom(Predicate("qn11"))
  val T = TimeVariableWithOffset("T")

  val propositionalProgram = LarsProgram.from(
    t_dp <= WindowAtom(SlidingTimeWindow(10), Diamond, p),
    t_bp <= WindowAtom(SlidingTimeWindow(10), Box, p),
    t_a3p <= WindowAtom(SlidingTimeWindow(10), At(3), p),
    t_aTp <= WindowAtom(SlidingTimeWindow(10), At(T), p),
    c_dp <= WindowAtom(SlidingTupleWindow(10), Diamond, p),
    c_bp <= WindowAtom(SlidingTupleWindow(10), Box, p),
    c_a3p <= WindowAtom(SlidingTupleWindow(10), At(3), p),
    c_aTp <= WindowAtom(SlidingTupleWindow(10), At(T), p),
    h_p <= p,
    h_a3p <= AtAtom(3,p),
    h_aTp <= AtAtom(T,p),
    n_t_bp <= not(WindowAtom(SlidingTimeWindow(10), Box, p)),
    n_t_dp <= not(WindowAtom(SlidingTimeWindow(10), Diamond, p)),
    n_t_a3p <= not(WindowAtom(SlidingTimeWindow(10), At(3), p)),
    //qn4 <= not(WindowAtom(SlidingTimeWindow(10), At(T), p)), //grounding limitation
    n_c_bp <= not(WindowAtom(SlidingTupleWindow(10), Box, p)),
    n_c_dp <= not(WindowAtom(SlidingTupleWindow(10), Diamond, p)),
    n_c_a3p <= not(WindowAtom(SlidingTupleWindow(10), At(3), p)),
    //qn8b <= not(WindowAtom(SlidingTupleWindow(10), At(T), p)), //grounding limitation
    UserDefinedLarsRule(n_h_p,Set(),Set(p)),
    UserDefinedLarsRule(n_h_a3p,Set(),Set(AtAtom(3,p)))
    //UserDefinedLarsRule(qn11,Set(),Set(AtAtom(T,p))) //grounding limitation
  )

  test("basic propositional S1") {

    val stream = Map[Int,Set[Atom]](3 -> Set(p))

    def c = complement(20) _

    val expectedEntailmentTimePoints:Map[Atom,Set[Int]] = Map(
      p -> Set(3),
      t_dp -> set(3,13),
      t_bp -> Set(),
      t_a3p -> set(3,13),
      t_aTp -> set(3,13),
      c_dp -> set(3,20),
      c_bp -> Set(),
      c_a3p -> set(3,20),
      c_aTp -> set(3,20),
      h_p -> Set(3),
      h_a3p -> set(3,20),
      h_aTp -> set(3,20),
      n_t_bp -> set(0,20),
      n_t_dp -> c(set(3,13)),
      n_t_a3p -> c(set(3,13)),
      n_c_bp -> set(0,20),
      n_c_dp -> set(0,2),
      n_c_a3p -> set(0,2),
      n_h_p -> c(Set(3)),
      n_h_a3p -> set(0,2)
    )

    checkEntailments(propositionalProgram,expectedEntailmentTimePoints,stream)

  }

  test("basic propositional S2") {

    val stream = Map[Int,Set[Atom]](0 -> Set(p), 1 -> Set(p), 2 -> Set(p), 3 -> Set(p), 4 -> Set(p), 5 -> Set(p))

    def c = complement(20) _

    val expectedEntailmentTimePoints:Map[Atom,Set[Int]] = Map(
      p -> set(0,5),
      t_dp -> set(0,15),
      t_bp -> set(0,5),
      t_a3p -> set(3,13),
      t_aTp -> set(0,15),
      c_dp -> set(0,20),
      c_bp -> set(0,5),
      c_a3p -> set(3,20),
      c_aTp -> set(0,20),
      h_p -> set(0,5),
      h_a3p -> set(3,20),
      h_aTp -> set(0,20),
      n_t_bp -> set(6,20),
      n_t_dp -> set(16,20),
      n_t_a3p -> c(set(3,13)),
      n_c_bp -> set(6,20),
      n_c_dp -> Set(),
      n_c_a3p -> set(0,2),
      n_h_p -> set(6,20),
      n_h_a3p -> set(0,2)
    )

//    var first: Double = 0
//    var other: Double = 0
//    var nFirst = 3
//    var nOther = 10
//
//    (-(nFirst-1) to nOther) foreach { x =>
//      val t = Util.stopTime {

        checkEntailments(propositionalProgram, expectedEntailmentTimePoints, stream)

//      }
//      if (x <= 0) first = first + (1.0*t)
//      else other = other + (1.0*t)
//    }
//
//    println("first: "+Util.timeString(first/nFirst))
//    println("other: "+Util.timeString(other/nOther))

  }

  test("basic propositional S3") {

    val stream = Map[Int,Set[Atom]](0 -> Set(p1,p2), 1 -> Set(p1,p2), 2 -> Set(p1,p2), 3 -> Set(p1,p2), 5 -> Set(p1,p2,p))

    def c = complement(20) _

    val expectedEntailmentTimePoints:Map[Atom,Set[Int]] = Map(
      p -> Set(5),
      t_dp -> set(5,15),
      t_bp -> Set(),
      t_a3p -> Set(),
      t_aTp -> set(5,15),
      c_dp -> set(5,20),
      c_bp -> Set(),
      c_a3p -> Set(),
      c_aTp -> set(5,20),
      h_p -> Set(5),
      h_a3p -> Set(),
      h_aTp -> set(5,20),
      n_t_dp -> c(set(5,15)),
      n_t_bp -> set(0,20),
      n_t_a3p -> set(0,20),
      n_c_dp -> set(0,4),
      n_c_bp -> set(0,20),
      n_c_a3p -> set(0,20),
      n_h_p -> c(Set(5)),
      n_h_a3p -> set(0,20)
    )

    checkEntailments(propositionalProgram, expectedEntailmentTimePoints, stream)

  }

  test("basic propositional S4") {

    val stream = Map[Int,Set[Atom]](0 -> Set(p1,p2), 1 -> Set(p1,p2), 2 -> Set(p1,p2), 3 -> Set(p1,p2), 5 -> Set(p1,p2,p),
      6 -> Set(p1), 7 -> Set(p1), 8 -> Set(p1), 9 -> Set(p1), 10 -> Set(p1), 11 -> Set(p1), 12 -> Set(p1), 13 -> Set(p1),
      14 -> Set(p1), 15 -> Set(p1))

    def c = complement(20) _

    val expectedEntailmentTimePoints:Map[Atom,Set[Int]] = Map(
      p -> Set(5),
      t_dp -> set(5,15),
      t_bp -> Set(),
      t_a3p -> Set(),
      t_aTp -> set(5,15),
      c_dp -> set(5,14), //diff to S3
      c_bp -> Set(),
      c_a3p -> Set(),
      c_aTp -> set(5,14), //diff to S3
      h_p -> Set(5),
      h_a3p -> Set(),
      h_aTp -> set(5,20),
      n_t_dp -> c(set(5,15)),
      n_t_bp -> set(0,20),
      n_t_a3p -> set(0,20),
      n_c_dp -> c(set(5,14)), //diff to S3
      n_c_bp -> set(0,20),
      n_c_a3p -> set(0,20),
      n_h_p -> c(Set(5)),
      n_h_a3p -> set(0,20)
    )

    checkEntailments(propositionalProgram, expectedEntailmentTimePoints, stream)

  }

  test("single") {
    val stream = Map[Int,Set[Atom]](0 -> Set(p), 1 -> Set(p), 2 -> Set(p), 3 -> Set(p), 4 -> Set(p), 5 -> Set(p))

    val expectedEntailmentTimePoints:Map[Atom,Set[Int]] = Map(
      p -> (0 to 5).toSet,
      n_c_a3p -> (0 to 2).toSet
    )

    val rule = n_c_a3p <= not(WindowAtom(SlidingTupleWindow(10), At(3), p))

    checkEntailments(LarsProgram.from(rule),expectedEntailmentTimePoints,stream)
  }

  def checkEntailments(program: LarsProgram, expectedEntailmentTimePoints: Map[Atom,Set[Int]], stream:Map[Int,Set[Atom]]): Unit = {

    val engine = defaultEngine(program)

    val maxInt:Int = expectedEntailmentTimePoints.values reduce { (l,r) => l ++ r} reduce Math.max
    println("timeline: [0,"+maxInt+"]")

    for (t <- 0 to maxInt) {

      stream.get(t) match {
        case Some(atoms) => atoms foreach (atom => engine.append(TimePoint(t))(atom))
        case None =>
      }

      val model = engine.evaluate(TimePoint(t)).model

      for (atom <- expectedEntailmentTimePoints.keys) {
        if (expectedEntailmentTimePoints(atom) contains t) {
          if (!(model contains atom)) {
            println(f"t=$t: atom ${atom} not in model ${model}")
          }
          assert(model contains atom)
        } else {
          if (model contains atom) {
            println(f"t=$t: atom ${atom} in model ${model}")
          }
          assert(!(model contains atom))
        }
      }

    }
  }

  def set(start: Int, end: Int): Set[Int] = (start to end).toSet
  def complement(upTo: Int)(s: Set[Int]) = set(0,upTo) -- s


}
