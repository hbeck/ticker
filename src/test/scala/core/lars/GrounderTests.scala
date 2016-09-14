package core.lars

import common.Util.printTime
import core._
import core.asp._
import jtms.evaluation.Util
import Util._
import jtms._
import org.scalatest.FunSuite

import scala.collection.immutable.HashMap

/**
  * Created by hb on 8/23/16.
  */
class GrounderTests extends FunSuite {

  test("parsing") {
    assert(xatom("a")==PredicateAtom(p("a")))
    assert(xatom("a(x)")==GroundAtomWithArguments(Predicate("a"),Seq[Value](StringValue("x"))))
    assert(xatom("a(x,y)")==GroundAtomWithArguments(Predicate("a"),Seq[Value](StringValue("x"),StringValue("y"))))
    assert(xatom("a(X)")==NonGroundAtom(Predicate("a"),Seq[Argument]("X")))
    assert(xatom("a(X,y)")==NonGroundAtom(Predicate("a"),Seq[Argument]("X",StringValue("y"))))
    assert(xatom("a(y,X)")==NonGroundAtom(Predicate("a"),Seq[Argument](StringValue("y"),"X")))
  }

  //
  //
  //

  test("gt1") {
    val r1:LarsRule = fact("a")
    val rules:Seq[LarsRule] = Seq(r1)
    val p = LarsProgram(rules)
    assert(ground(p) == p)
  }

  test("gt2") {
    val r1 = fact("a(x)")
    val rules:Seq[LarsRule] = Seq(r1)
    val p = LarsProgram(rules)
    assert(ground(p) ==p)
  }

  test("gt3") {

    val r1 = fact("a(x)")
    val r2 = rule("b(V) :- a(V)")
    val p = program(r1,r2)

    val gr1 = r1
    val gr2 = rule("b(x) :- a(x)")
    val gp = program(gr1,gr2)
    val grounder = Grounder(p)

    //grounder.inspect.rules foreach println

    assert(grounder.groundProgram==gp)

    val model = modelFromClingo("a(x) b(x)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)

  }

  test("gt4") {

    val r1 = fact("a(x)")
    val r2 = rule("b(V) :- c(V)")
    val r3 = rule("c(V) :- a(V)")
    val p = program(r1,r2,r3)

    val gr1 = r1
    val gr2 = rule("b(x) :- c(x)")
    val gr3 = rule("c(x) :- a(x)")
    val gp = program(gr1,gr2,gr3)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == Set(strVal("x")))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == Set(strVal("x")))

    assert(grounder.groundProgram==gp)

    val model = modelFromClingo("a(x) c(x) b(x)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  val X = v("X")
  val Y = v("Y")
  val Z = v("Z")
  val x1 = strVal("x1")
  val x2 = strVal("x2")
  val y1 = strVal("y1")
  val y2 = strVal("y2")
  val z1 = strVal("z1")
  val z2 = strVal("z2")

  test("cross 1") {
    val xSets = Set(Set((X,x1)),Set((X,x2)))
    val ySets = Set(Set((Y,y1)),Set((Y,y2)))

    val expectedCross = Set(Set((X,x1),(Y,y1)),Set((X,x1),(Y,y2)),Set((X,x2),(Y,y1)),Set((X,x2),(Y,y2)))

    assert(Grounder.cross(xSets,ySets) == expectedCross)
  }

  test("cross 2") {
    val cross1:Set[Set[(Variable,Value)]] = Set(Set((X,x1),(Y,y1)),Set((X,x1),(Y,y2)),Set((X,x2),(Y,y1)),Set((X,x2),(Y,y2)))
    val zSets = Set(Set((Z,z1)),Set((Z,z2)))

    val expected: Set[Set[(Variable,Value)]] =
      Set(Set((X,x1),(Y,y1),(Z,z1)),Set((X,x1),(Y,y2),(Z,z1)),Set((X,x2),(Y,y1),(Z,z1)),Set((X,x2),(Y,y2),(Z,z1)),
        Set((X,x1),(Y,y1),(Z,z2)),Set((X,x1),(Y,y2),(Z,z2)),Set((X,x2),(Y,y1),(Z,z2)),Set((X,x2),(Y,y2),(Z,z2)))

    assert(Grounder.cross(cross1,zSets) == expected)
  }

  test("cross reduce ") {
    val xSet = Set(Set((X,x1)),Set((X,x2)))
    val ySet = Set(Set((Y,y1)),Set((Y,y2)))
    val zSet = Set(Set((Z,z1)),Set((Z,z2)))

    val expected: Set[Set[(Variable,Value)]] =
      Set(Set((X,x1),(Y,y1),(Z,z1)),Set((X,x1),(Y,y2),(Z,z1)),Set((X,x2),(Y,y1),(Z,z1)),Set((X,x2),(Y,y2),(Z,z1)),
          Set((X,x1),(Y,y1),(Z,z2)),Set((X,x1),(Y,y2),(Z,z2)),Set((X,x2),(Y,y1),(Z,z2)),Set((X,x2),(Y,y2),(Z,z2)))

    val result: Set[Set[(Variable, Value)]] = Seq(xSet,ySet,zSet).reduce((s1, s2) => Grounder.cross(s1,s2))

    assert(result == expected)
  }

  /*
  test("create assignment 1") {

    val possibleValuesPerVariable: Map[Variable,Set[Value]] = Map(X -> Set(x1,x2), Y -> Set(y1,y2), Z -> Set(z1,z2))

    val a1 = Assignment(Map[Variable,Value](X -> x1, Y -> y1, Z -> z1))
    val a2 = Assignment(Map[Variable,Value](X -> x1, Y -> y1, Z -> z2))
    val a3 = Assignment(Map[Variable,Value](X -> x1, Y -> y2, Z -> z1))
    val a4 = Assignment(Map[Variable,Value](X -> x1, Y -> y2, Z -> z2))
    val a5 = Assignment(Map[Variable,Value](X -> x2, Y -> y1, Z -> z1))
    val a6 = Assignment(Map[Variable,Value](X -> x2, Y -> y1, Z -> z2))
    val a7 = Assignment(Map[Variable,Value](X -> x2, Y -> y2, Z -> z1))
    val a8 = Assignment(Map[Variable,Value](X -> x2, Y -> y2, Z -> z2))

    val expectedAssignments: Set[Assignment] = Set(a1,a2,a3,a4,a4,a5,a6,a7,a8)

    assert(Grounder.createAssignments(possibleValuesPerVariable) == expectedAssignments)
  }
  */

  test("gt5") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val r3 = rule("b(V) :- a(V)")
    val p = program(ax,ay,r3)

    val gax = ax
    val gay = ay
    val gr3x = rule("b(x) :- a(x)")
    val gr3y = rule("b(y) :- a(y)")
    val gp = program(gax,gay,gr3x,gr3y)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVals("x","y"))
    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x) a(y) b(x) b(y)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt6") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val r2 = rule("b(V) :- c(V)")
    val r3 = rule("c(V) :- a(V)")
    val p = program(ax,ay,r2,r3)

    val gax = ax
    val gay = ay
    val gr2x = rule("b(x) :- c(x)")
    val gr2y = rule("b(y) :- c(y)")
    val gr3x = rule("c(x) :- a(x)")
    val gr3y = rule("c(y) :- a(y)")
    val gp = program(gax,gay,gr2x,gr2y,gr3x,gr3y)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == strVals("x","y"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVals("x","y"))

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x) a(y) c(x) c(y) b(x) b(y)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt7") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val az = fact("a(z)")
    val r3 = rule("c(V) :- a(V)")
    val p = program(ax,ay,az,r3)

    val gax = ax
    val gay = ay
    val gaz = az
    val gr3x = rule("c(x) :- a(x)")
    val gr3y = rule("c(y) :- a(y)")
    val gr3z = rule("c(z) :- a(z)")
    val gp = program(gax,gay,gaz,gr3x,gr3y,gr3z)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVals("x","y","z"))

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x) a(y) a(z) c(x) c(y) c(z)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt8") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val az = fact("a(z)")
    val r2 = rule("b(V) :- c(V)")
    val r3 = rule("c(V) :- a(V)")
    val p = program(ax,ay,az,r2,r3)

    val gax = ax
    val gay = ay
    val gaz = az
    val gr2x = rule("b(x) :- c(x)")
    val gr2y = rule("b(y) :- c(y)")
    val gr2z = rule("b(z) :- c(z)")
    val gr3x = rule("c(x) :- a(x)")
    val gr3y = rule("c(y) :- a(y)")
    val gr3z = rule("c(z) :- a(z)")
    val gp = program(gax,gay,gaz,gr2x,gr2y,gr2z,gr3x,gr3y,gr3z)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == strVals("x","y","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVals("x","y","z"))

//    println("expected program")
//    gp.rules foreach println
//
//    println("\ngrounded program")
//    grounder.groundProgram.rules foreach println

//    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//    println("only in computed: "+onlyInComputed)
//    println("only in expected: "+onlyInExpected)

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x) a(y) a(z) c(x) c(y) c(z) b(x) b(y) b(z)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("grounding rules") {
    val ri1 = rule("i(X,Y) :- a(X), b(Y)")
    assert(!ri1.isGround)
    assert(ri1.atoms forall (_.isInstanceOf[NonGroundAtom]))
    val a1 = Assignment(Map(v("X") -> "x1", v("Y") -> "y1"))
    val gri1 = ri1.assign(a1)
    assert(gri1.isGround)
    assert(gri1 == rule("i(x1,y1) :- a(x1), b(y1)"))

    val ri2 = rule("i(X,Y) :- a(X), b(Y), not c(Y), not d(Y)")
    assert(!ri2.isGround)
    assert(ri2.atoms forall (_.isInstanceOf[NonGroundAtom]))
    val a2 = Assignment(Map(v("X") -> "x1", v("Y") -> "y1"))
    val gri2 = ri2.assign(a2)
    assert(gri2.isGround)
    assert(gri2 == rule("i(x1,y1) :- a(x1), b(y1), not c(y1), not d(y1)"))
  }

  test("gt9") {

    val ax1 = fact("a(x1)")
    val ax2 = fact("a(x2)")
    val bx1 = fact("b(y1)")
    val bx2 = fact("b(y2)")

    val ri = rule("i(X,Y) :- a(X), b(Y)")
    val rj = rule("j(X) :- i(X,Y)")
    val p = program(ax1,ax2,bx1,bx2,ri,rj)

    val manualGrounding: Set[LarsRule] = {
      for (x <- Set("x1", "x2"); y <- Set("y1", "y2")) yield {
        val a = Assignment(Map(v("X") -> x, v("Y") -> y))
        val gri: LarsRule = ri.assign(a)
        val grj: LarsRule = rj.assign(a)
        Set[LarsRule](gri, grj)
      }
    }.flatten

    val rules = Seq[LarsRule](ax1,ax2,bx1,bx2) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(ri,v("X")) == strVals("x1","x2"))
    assert(grounder.inspect.possibleValuesForVariable(ri,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(rj,v("X")) == strVals("x1","x2"))
    assert(grounder.inspect.possibleValuesForVariable(rj,v("Y")) == strVals("y1","y2"))
    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x1) a(x2) b(y1) b(y2) i(x1,y1) i(x2,y1) i(x1,y2) i(x2,y2) j(x1) j(x2)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt10") {

    val ax1 = fact("a(x1)")
    val ax2 = fact("a(x2)")
    val by3 = fact("b(y3)")
    val by4 = fact("b(y4)")

    val ri1 = rule("i(X,Y) :- a(X), b(Y)")
    val ri2 = rule("i(X,Y) :- i(Y,X)")

    val p = program(ax1,ax2,by3,by4,ri1,ri2)

    val manualGrounding: Set[LarsRule] = {
      for (x <- Set("x1", "x2"); y <- Set("y3", "y4")) yield {
        val a1 = Assignment(Map(v("X") -> x, v("Y") -> y))
        val gri1: LarsRule = ri1.assign(a1)
        val a2 = Assignment(Map(v("Y") -> x, v("X") -> y))
        val gri2: LarsRule = ri2.assign(a2)
        Set[LarsRule](gri1, gri2)
      }
    }.flatten

    val rules = Seq[LarsRule](ax1,ax2,by3,by4) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(ri1,v("X")) == strVals("x1","x2"))
    assert(grounder.inspect.possibleValuesForVariable(ri1,v("Y")) == strVals("y3","y4"))
    assert(grounder.inspect.possibleValuesForVariable(ri2,v("X")) == strVals("y3","y4"))
    assert(grounder.inspect.possibleValuesForVariable(ri2,v("Y")) == strVals("x1","x2"))

    val r:LarsRule = rule("i(y3,x1) :- i(x1,y3)")
    assert(grounder.groundProgram.rules.contains(r))

    //println(LarsProgram(grounder.groundProgram.rules))

//    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//    println("only in computed: "+LarsProgram(onlyInComputed))
//    println("only in expected: "+LarsProgram(onlyInExpected))

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x1) a(x2) b(y3) b(y4) i(x1,y3) i(x2,y3) i(x1,y4) i(x2,y4) i(y3,x1) i(y3,x2) i(y4,x1) i(y4,x2)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt11") {

    val a12 = fact("a(x1,x2)")
    val a23 = fact("a(x2,x3)")
    val a34 = fact("a(x3,x4)")

    val ri1 = rule("i(X,Y) :- a(X,Y)")
    val ri2 = rule("i(X,Y) :- i(X,Z), i(Z,Y)")

    val p = program(a12,a23,a34,ri1,ri2)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x3,x4) :- a(x3,x4)"),
      rule("i(x2,x2) :- a(x2,x2)"),
      rule("i(x1,x3) :- a(x1,x3)"),
      rule("i(x1,x4) :- a(x1,x4)"),
      rule("i(x3,x2) :- a(x3,x2)"),
      rule("i(x2,x3) :- a(x2,x3)"),
      rule("i(x1,x2) :- a(x1,x2)"),
      rule("i(x2,x4) :- a(x2,x4)"),
      rule("i(x3,x3) :- a(x3,x3)"),
      rule("i(x3,x2) :- i(x3,x3), i(x3,x2)"),
      rule("i(x3,x3) :- i(x3,x1), i(x1,x3)"),
      rule("i(x3,x4) :- i(x3,x1), i(x1,x4)"),
      rule("i(x3,x3) :- i(x3,x2), i(x2,x3)"),
      rule("i(x1,x3) :- i(x1,x1), i(x1,x3)"),
      rule("i(x1,x3) :- i(x1,x4), i(x4,x3)"),
      rule("i(x3,x3) :- i(x3,x4), i(x4,x3)"),
      rule("i(x2,x2) :- i(x2,x1), i(x1,x2)"),
      rule("i(x3,x2) :- i(x3,x2), i(x2,x2)"),
      rule("i(x1,x4) :- i(x1,x3), i(x3,x4)"),
      rule("i(x1,x4) :- i(x1,x2), i(x2,x4)"),
      rule("i(x1,x4) :- i(x1,x4), i(x4,x4)"),
      rule("i(x3,x3) :- i(x3,x3)"),
      rule("i(x3,x4) :- i(x3,x2), i(x2,x4)"),
      rule("i(x2,x3) :- i(x2,x2), i(x2,x3)"),
      rule("i(x1,x4) :- i(x1,x1), i(x1,x4)"),
      rule("i(x1,x2) :- i(x1,x1), i(x1,x2)"),
      rule("i(x1,x3) :- i(x1,x2), i(x2,x3)"),
      rule("i(x2,x3) :- i(x2,x3), i(x3,x3)"),
      rule("i(x1,x2) :- i(x1,x2), i(x2,x2)"),
      rule("i(x2,x4) :- i(x2,x2), i(x2,x4)"),
      rule("i(x3,x2) :- i(x3,x1), i(x1,x2)"),
      rule("i(x3,x2) :- i(x3,x4), i(x4,x2)"),
      rule("i(x2,x4) :- i(x2,x1), i(x1,x4)"),
      rule("i(x1,x2) :- i(x1,x3), i(x3,x2)"),
      rule("i(x3,x4) :- i(x3,x4), i(x4,x4)"),
      rule("i(x2,x3) :- i(x2,x1), i(x1,x3)"),
      rule("i(x2,x3) :- i(x2,x4), i(x4,x3)"),
      rule("i(x3,x4) :- i(x3,x3), i(x3,x4)"),
      rule("i(x2,x2) :- i(x2,x2)"),
      rule("i(x2,x2) :- i(x2,x3), i(x3,x2)"),
      rule("i(x1,x2) :- i(x1,x4), i(x4,x2)"),
      rule("i(x2,x2) :- i(x2,x4), i(x4,x2)"),
      rule("i(x2,x4) :- i(x2,x4), i(x4,x4)"),
      rule("i(x1,x3) :- i(x1,x3), i(x3,x3)"),
      rule("i(x2,x4) :- i(x2,x3), i(x3,x4)")
    )

    val rules = Seq[LarsRule](a12,a23,a34) ++ manualGrounding
    val gp = LarsProgram(rules)

    val grounder = Grounder(p)

    //printInspect(grounder)

    assert(grounder.inspect.possibleValuesForVariable(ri1,v("X")) == strVals("x1","x2","x3"))
    assert(grounder.inspect.possibleValuesForVariable(ri1,v("Y")) == strVals("x2","x3","x4"))
    assert(grounder.inspect.possibleValuesForVariable(ri2,v("X")) == strVals("x1","x2","x3"))
    assert(grounder.inspect.possibleValuesForVariable(ri2,v("Y")) == strVals("x2","x3","x4"))
    assert(grounder.inspect.possibleValuesForVariable(ri2,v("Z")) == strVals("x1","x2","x3","x4"))

    //println(grounder.groundProgram.rules)

//    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//    println("only in computed: "+LarsProgram(onlyInComputed))
//    println("only in expected: "+LarsProgram(onlyInExpected))

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x1,x2) a(x2,x3) a(x3,x4) i(x1,x2) i(x2,x3) i(x3,x4) i(x1,x3) i(x2,x4) i(x1,x4)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)

  }



  test("gt12") {

    val a1 = fact("a(x)")
    val b11 = fact("b(x,y1)")
    val b12 = fact("b(x,y2)")
    val c1 = fact("c(y1)")
    val c2 = fact("c(y2)")
    val d12 = fact("d(x,y2)")

    val r1 = rule("i(X,Y) :- a(X), i(X,Y)")
    val r2 = rule("i(X,Y) :- j(X,Y), not d(X,Y)")
    val r3 = rule("j(X,Y) :- b(X,Z), c(Y)")

    val p = program(a1,b11,b12,c1,c2,d12,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y1) :- a(x), i(x,y1)"),
      rule("i(x,y2) :- a(x), i(x,y2)"),
      rule("i(x,y1) :- j(x,y1), not d(x,y1)"),
      rule("i(x,y2) :- j(x,y2), not d(x,y2)"),
      rule("j(x,y1) :- b(x,y1), c(y1)"),
      rule("j(x,y2) :- b(x,y1), c(y2)"),
      rule("j(x,y1) :- b(x,y2), c(y1)"),
      rule("j(x,y2) :- b(x,y2), c(y2)")
    )

    val rules = Seq[LarsRule](a1,b11,b12,c1,c2,d12) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r1,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r1,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Z")) == strVals("y1","y2"))

    //    println(LarsProgram(grounder.groundProgram.rules))
    //
    //    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
    //    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
    //
    //    println("only in computed: "+LarsProgram(onlyInComputed))
    //    println("only in expected: "+LarsProgram(onlyInExpected))
    //
    //    printInspect(grounder)

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x) b(x,y1) b(x,y2) c(y1) c(y2) d(x,y2) j(x,y1) j(x,y2) i(x,y1)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt12w") {

    val a1 = fact("a(x)")
    val b11 = fact("b(x,y1)")
    val b12 = fact("b(x,y2)")
    val c1 = fact("c(y1)")
    val c2 = fact("c(y2)")
    val d12 = fact("d(x,y2)")

    val r1 = rule("i(X,Y) :- a(X), i(X,Y), w_d_7_sig1(Y)")
    val r2 = rule("i(X,Y) :- j(X,Y), not d(X,Y), not w_d_7_sig2(X,Y)")
    val r3 = rule("j(X,Y) :- b(X,Z), c(Y), not w_d_7_sig3(X,Z,Y)")

    val p = program(a1,b11,b12,c1,c2,d12,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y1) :- a(x), i(x,y1), w_d_7_sig1(y1)"),
      rule("i(x,y2) :- a(x), i(x,y2), w_d_7_sig1(y2)"),
      rule("i(x,y1) :- j(x,y1), not d(x,y1), not w_d_7_sig2(x,y1)"),
      rule("i(x,y2) :- j(x,y2), not d(x,y2), not w_d_7_sig2(x,y2)"),
      rule("j(x,y1) :- b(x,y1), c(y1), not w_d_7_sig3(x,y1,y1)"),
      rule("j(x,y2) :- b(x,y1), c(y2), not w_d_7_sig3(x,y1,y2)"),
      rule("j(x,y1) :- b(x,y2), c(y1), not w_d_7_sig3(x,y2,y1)"),
      rule("j(x,y2) :- b(x,y2), c(y2), not w_d_7_sig3(x,y2,y2)")
    )

    val rules = Seq[LarsRule](a1,b11,b12,c1,c2,d12) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r1,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r1,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Y")) == strVals("y1","y2"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Z")) == strVals("y1","y2"))

    //    println(LarsProgram(grounder.groundProgram.rules))
    //
//        val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//        val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//        println("only in computed: "+LarsProgram(onlyInComputed))
//        println("only in expected: "+LarsProgram(onlyInExpected))
//
//        printInspect(grounder)

    assert(grounder.groundProgram == gp)

    //no semantics comparison with asp
  }

  test("gt13") {

    val a = fact("a(x,y)")
    val b = fact("b(y)")

    val r1 = rule("i(X,Y) :- a(X,Y), b(Y)")
    val r2 = rule("i(z,z) :- i(X,Y), not d(X,Y)")
    val r3 = rule("j(X,Y) :- i(X,Y)")

    val p = program(a,b,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y) :- a(x,y), b(y)"),
      rule("i(z,z) :- i(x,y), not d(x,y)"),
      rule("i(z,z) :- i(z,z), not d(z,z)"),
      rule("i(z,z) :- i(z,y), not d(z,y)"),
      rule("i(z,z) :- i(x,z), not d(x,z)"),
      rule("j(z,y) :- i(z,y)"),
      rule("j(x,z) :- i(x,z)"),
      rule("j(x,y) :- i(x,y)"),
      rule("j(z,z) :- i(z,z)")
    )

    val rules = Seq[LarsRule](a,b) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r1,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r1,v("Y")) == strVals("y"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("X")) == strVals("x","z"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("Y")) == strVals("y","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("X")) == strVals("x","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Y")) == strVals("y","z"))

//    println(LarsProgram(grounder.groundProgram.rules))
//
//    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//    println("only in computed: "+LarsProgram(onlyInComputed))
//    println("only in expected: "+LarsProgram(onlyInExpected))
//
//    printInspect(grounder)

    assert(grounder.groundProgram == gp)

    val model = modelFromClingo("a(x,y) b(y) i(x,y) i(z,z) j(x,y) j(z,z)")

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt13w") {

    val a = fact("a(x,y)")
    val b = fact("b(y)")

    val r1 = rule("i(X,Y) :- a(X,Y), b(Y), w_d_7_sig2(X,Y)")
    val r2 = rule("i(z,z) :- i(X,Y), w_d_7_sig1(X), not d(X,Y)")
    val r3 = rule("j(X,Y) :- i(X,Y), w_d_7_sig1(X), w_d_7_sig3(X,X,Y)")

    val p = program(a,b,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y) :- a(x,y), b(y), w_d_7_sig2(x,y)"),
      rule("i(z,z) :- i(x,y), w_d_7_sig1(x), not d(x,y)"),
      rule("i(z,z) :- i(z,z), w_d_7_sig1(z), not d(z,z)"),
      rule("i(z,z) :- i(z,y), w_d_7_sig1(z), not d(z,y)"),
      rule("i(z,z) :- i(x,z), w_d_7_sig1(x), not d(x,z)"),
      rule("j(z,y) :- i(z,y), w_d_7_sig1(z), w_d_7_sig3(z,z,y)"),
      rule("j(x,z) :- i(x,z), w_d_7_sig1(x), w_d_7_sig3(x,x,z)"),
      rule("j(x,y) :- i(x,y), w_d_7_sig1(x), w_d_7_sig3(x,x,y)"),
      rule("j(z,z) :- i(z,z), w_d_7_sig1(z), w_d_7_sig3(z,z,z)")
    )

    val rules = Seq[LarsRule](a,b) ++ manualGrounding

    val gp = LarsProgram(rules)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r1,v("X")) == strVals("x"))
    assert(grounder.inspect.possibleValuesForVariable(r1,v("Y")) == strVals("y"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("X")) == strVals("x","z"))
    assert(grounder.inspect.possibleValuesForVariable(r2,v("Y")) == strVals("y","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("X")) == strVals("x","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("Y")) == strVals("y","z"))

    //    println(LarsProgram(grounder.groundProgram.rules))
    //
    //    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
    //    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
    //
    //    println("only in computed: "+LarsProgram(onlyInComputed))
    //    println("only in expected: "+LarsProgram(onlyInExpected))
    //
    //    printInspect(grounder)

    assert(grounder.groundProgram == gp)

    //no semantics comparison with asp
  }

  test("gt rel 1") {
    /*
      % every machine can process only one task at a time
      :- assign(M,T1,P1), assign(M,T2,P2), neq(T1,T2), leq(P1,P2),
         duration(T1,D), sum(P1,D,Z), lt(P2,Z).
     */

    val r = rule("vals(M,T1,T2,P1,P2,D,Z) :- timepoint(Z), assign(M,T1,P1), assign(M,T2,P2), neq(T1,T2), leq(P1,P2), duration(T1,D), sum(P1,D,Z), lt(P2,Z)")

    val facts: Seq[LarsRule] = Seq(
      fact("assign(m1,t1,0)"),
      fact("assign(m2,t2,1)"),
      fact("assign(m2,t3,2)"),
      fact("duration(t1,4)"),
      fact("duration(t2,2)"),
      fact("duration(t3,3)"),
      fact("timepoint(0)"),
      fact("timepoint(1)"),
      fact("timepoint(2)"),
      fact("timepoint(3)"),
      fact("timepoint(4)"),
      fact("timepoint(5)"),
      fact("timepoint(6)"),
      fact("timepoint(7)"),
      fact("timepoint(8)"),
      fact("timepoint(9)"),
      fact("timepoint(10)")
    )

    assert(facts forall (_.head.isInstanceOf[GroundAtom]))

    val inputProgram = LarsProgram(facts ++ Seq(r))
    val grounder = Grounder(inputProgram)

    //println(grounder.groundProgram)

    //
    // initial tests, variables to iterate over
    //

    val list0to10 = (for (i <- 0 to 10) yield ""+i).toList

    val valuesM = strVals("m1","m2")
    val valuesT1 = strVals("t1","t2","t3")
    val valuesT2 = valuesT1
    val valuesP1 = intVals("0","1","2")
    val valuesP2 = valuesP1
    val valuesZ = intVals(list0to10: _*)
    val valuesD = intVals("4","2","3")

    assert(grounder.inspect.possibleValuesForVariable(r,v("M")) == valuesM)
    assert(grounder.inspect.possibleValuesForVariable(r,v("T1")) == valuesT1)
    assert(grounder.inspect.possibleValuesForVariable(r,v("T2")) == valuesT2)
    assert(grounder.inspect.possibleValuesForVariable(r,v("P1")) == valuesP1)
    assert(grounder.inspect.possibleValuesForVariable(r,v("P2")) == valuesP2)
    assert(grounder.inspect.possibleValuesForVariable(r,v("Z")) == valuesZ)
    assert(grounder.inspect.possibleValuesForVariable(r,v("D")) == valuesD)

    //
    //  craft expected ground program
    //

    //note that template does not include the auxiliary relation atoms!
    val template = "vals(M,T1,T2,P1,P2,D,Z) :- timepoint(Z), assign(M,T1,P1), assign(M,T2,P2), duration(T1,D)"

    val manualGrounding: Set[LarsRule] =
      for (m <- valuesM; t1 <- valuesT1; t2 <- valuesT2;
           p1 <- valuesP1; p2 <- valuesP2; z <- valuesZ; d <- valuesD
           if {
             t1 != t2 && asInt(p1) <= asInt(p2) && (asInt(p1) + asInt(d) == asInt(z)) && asInt(p2) < asInt(z)
           }
      ) yield {
        val str = template
          .replaceAll("M", m.toString)
          .replaceAll("T1", t1.toString)
          .replaceAll("T2", t2.toString)
          .replaceAll("P1", p1.toString)
          .replaceAll("P2", p2.toString)
          .replaceAll("D", d.toString)
          .replaceAll("Z", z.toString)

        rule(str)
      }

    val rules = facts ++ manualGrounding

    //println("#rules: "+rules.size)

    val gp = LarsProgram(rules)
//
//    val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//    val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//    println("only in computed: "+LarsProgram(onlyInComputed))
//    println("only in expected: "+LarsProgram(onlyInExpected))

    // printInspect(grounder)

    assert(grounder.groundProgram == gp)

    // ground rule that needs to fire:
    // from clingo:
    // vals(m2,t2,t3,1,2,2,3) :- 1+2=3, assign(m2,t2,1), 1<=2, duration(t2,2), 2<3, t2!=t3, timepoint(3), assign(m2,t3,2).
    val firingRule = rule("vals(m2,t2,t3,1,2,2,3) :- assign(m2,t2,1), duration(t2,2), timepoint(3), assign(m2,t3,2)")
    assert(grounder.groundProgram.rules contains firingRule)

    val clingoModelStr =
      "assign(m1,t1,0) assign(m2,t2,1) assign(m2,t3,2) duration(t1,4) duration(t2,2) duration(t3,3) "+
        "timepoint(0) timepoint(1) timepoint(2) timepoint(3) timepoint(4) timepoint(5) timepoint(6) "+
        "timepoint(7) timepoint(8) timepoint(9) timepoint(10) vals(m2,t2,t3,1,2,2,3)"

    val model = modelFromClingo(clingoModelStr)

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    assert(tms.getModel.get == model)
  }

  test("gt rel 2") {

    val r1 = rule("c(X,Y) :- a(X), b(Y), not d(X,Y), int(Z), sum(X,Y,Z), lt(Z,2)")
    val r2 = rule("d(X,Y) :- a(X), b(Y), not c(X,Y), int(Z), sum(X,Y,Z), lt(Z,2)")

    val facts: Seq[LarsRule] = Seq(
      fact("int(0)"),
      fact("int(1)"),
      fact("int(2)"),
      fact("a(0)"),
      fact("a(1)"),
      fact("b(0)"),
      fact("b(1)")
    )

    assert(facts forall (_.head.isInstanceOf[GroundAtom]))

    val inputProgram = LarsProgram(facts ++ Seq(r1,r2))
    val grounder = Grounder(inputProgram)

    //println(grounder.groundProgram)

    //
    // initial tests, variables to iterate over
    //

    val valuesA = intVals("0","1")
    val valuesB = intVals("0","1")
    val valuesInt = intVals("0","1","2")

    Seq(r1,r2) foreach { r =>
      assert(grounder.inspect.possibleValuesForVariable(r,v("X")) == valuesA)
      assert(grounder.inspect.possibleValuesForVariable(r,v("Y")) == valuesB)
      assert(grounder.inspect.possibleValuesForVariable(r,v("Z")) == valuesInt)
    }


    //
    //  craft expected ground program
    //

    //note that template does not include the auxiliary relation atoms!
    val tmp1 = "c(X,Y) :- a(X), b(Y), not d(X,Y), int(Z)"
    val tmp2 = "d(X,Y) :- a(X), b(Y), not c(X,Y), int(Z)"

    val groupsOfGroundings: Set[Set[LarsRule]] =
      for (x <- (valuesA map asInt); y <- (valuesB map asInt); z <- (valuesInt map asInt)
           if {
             (x + y == z) && (z < 2)
           }
      ) yield {
        def replaceIn(template:String) = template
          .replaceAll("X", ""+x)
          .replaceAll("Y", ""+y)
          .replaceAll("Z", ""+z)

        Set(rule(replaceIn(tmp1)),rule(replaceIn(tmp2)))
      }

    val manualGrounding: Set[LarsRule] = groupsOfGroundings.flatten

    val rules = facts ++ manualGrounding

    //println("#rules: "+rules.size)
    //rules foreach { r => println(LarsProgram(Seq(r))) }

    val gp = LarsProgram(rules)
    //
//        val onlyInComputed = for (r <- grounder.groundProgram.rules if (!gp.rules.contains(r))) yield r
//        val onlyInExpected = for (r <- gp.rules if (!grounder.groundProgram.rules.contains(r))) yield r
//
//        println("only in computed: "+LarsProgram(onlyInComputed))
//        println("only in expected: "+LarsProgram(onlyInExpected))

    // printInspect(grounder)

    assert(grounder.groundProgram == gp)

    /* clingo models projected to c/2: */
    val clingoModelStrings = Set(
      "c(0,0) c(0,1) c(1,0)",
      "c(0,0) c(1,0)",
      "c(0,0) c(0,1)",
      "c(0,0)",
      "c(0,1) c(1,0)",
      "c(0,1)",
      "c(1,0)")

    val models = clingoModelStrings map modelFromClingo

    val asp = asAspProgram(grounder.groundProgram)
    val tms = JtmsGreedy(asp)
    val projectedModel = tms.getModel.get filter (_.predicate.caption == "c")
    //    println("projected model: "+projectedModel)

    assert(models contains projectedModel)
  }

  //
  // scheduling examples
  //

  //convention has contradiction atoms with xx (for easy parsing above)
  val schedulingProgram = LarsProgram(Seq[LarsRule](
    rule("task(T) :- duration(T,D)"),
    rule("assign(M,T,P) :- machine(M), task(T), timepoint(P), not n_assign(M,T,P)"),
    rule("n_assign(M,T,P) :- machine(M), task(T), timepoint(P), not assign(M,T,P)"),
    rule("xx1 :- assign(M,T,P), n_assign(M,T,P), not xx1"),
    rule("some_assign(T) :- assign(M,T,P)"),
    rule("xx2 :- task(T), not some_assign(T), not xx2"),
    rule("xx3 :- assign(M1,T,P1), assign(M2,T,P2), neq(M1,M2), not xx3"),
    rule("xx4 :- assign(M1,T,P1), assign(M2,T,P2), neq(P1,P2), not xx4"),
    rule("xx5 :- assign(M,T1,P1), assign(M,T2,P2), neq(T1,T2), leq(P1,P2), duration(T1,D), sum(P1,D,Z), lt(P2,Z), timepoint(Z), not xx5"),
    rule("finish(T,Z) :- assign(M,T,P), duration(T,D), sum(P,D,Z), timepoint(Z)"),
    rule("xx6 :- finish(T,Z), deadline(E), lt(E,Z), not xx6"), //using different variable for deadline than for duration!
    rule("busy_at(M,P2) :- assign(M,T,P1), duration(T,D), sum(P1,D,Z), timepoint(P2), leq(P1,P2), lt(P2,Z), timepoint(Z)"),
    rule("idle_at(M,P) :- machine(M), timepoint(P), not busy_at(M,P)"),
    rule("xx7 :- idle_at(M,P1), assign(M2,T,P2), lt(P1,P2), not xx7"),
    rule("some_busy_at(P) :- busy_at(M,P)"),
    rule("none_busy_at(P) :- timepoint(P), not some_busy_at(P)"),
    rule("xx8 :- none_busy_at(P1), some_busy_at(P2), lt(P1,P2), not xx8"),
    rule("n_max_finish(P1) :- finish(T1,P1), finish(T2,P2), lt(P1,P2)"),
    rule("max_finish(P) :- finish(T,P), not n_max_finish(P)")
  ))

  test("gt scheduling 0") {
    //schedulingProgram.rules foreach println

    val facts: Seq[LarsRule] = Seq(
      fact("deadline(2)"),
      fact("timepoint(0)"),
      fact("timepoint(1)"),
      fact("timepoint(2)"),
      fact("timepoint(3)"),
      fact("timepoint(4)"),
      fact("machine(m1)"),
      fact("machine(m2)"),
      fact("duration(t1,1)"),
      fact("duration(t2,2)")
    )

    val inputProgram = LarsProgram(schedulingProgram.rules ++ facts)
    val grounder = Grounder(inputProgram)


    //
    // variables to iterate over
    //

    val possibleValuesMap: Map[Variable,Set[Value]] = Map(
      v("M") -> strVals("m1","m2"),
      v("M1") -> strVals("m1","m2"),
      v("M2") -> strVals("m1","m2"),
      v("T") -> strVals("t1","t2"),
      v("T1") -> strVals("t1","t2"),
      v("T2") -> strVals("t1","t2"),
      v("P") -> intVals("0","1","2","3","4"),
      v("P1") -> intVals("0","1","2","3","4"),
      v("P1") -> intVals("0","1","2","3","4"),
      v("D") -> intVals("1","2"),
      v("Z") -> intVals("0","1","2","3","4"),
      v("E") -> intVals("2")
    )

    inputProgram.rules foreach { r =>
      for ((variable,possibleValues) <- possibleValuesMap) {
        if (r.variables.contains(variable)) {
          if (grounder.inspect.possibleValuesForVariable(r,variable) != possibleValues) {
            println("rule: "+r)
            println("variable: "+variable.name)
            println("expected values: "+possibleValues)
            println("actual values:   "+grounder.inspect.possibleValuesForVariable(r,variable))
            assert(false)
          }
        }
      }
    }

    println("#rules in ground program: "+grounder.groundProgram.rules.size)

    // printInspect(grounder)

    // clingo models projected to c/2:
    val clingoModelStrings = Set(
      "assign(m1,t1,0) assign(m2,t2,0)",
      "assign(m1,t2,0) assign(m2,t1,0)"
    )

    val models = clingoModelStrings map modelFromClingo

    val asp = asAspProgram(grounder.groundProgram)

//    println("contradiction atoms:")
//    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println


    val tms = JtmsLearn(asp)
    tms.shuffle = false

    //asp.rules foreach println

    var failures = 0
    /*
    for (attempt <- 1 to 1000) {
      tms.getModel match {
        case Some(model) => {
          val projectedModel = model filter (_.predicate.caption == "assign")
          println("projected model: "+projectedModel)
          assert(models contains projectedModel)
          tms.recompute()
        }
        case None => {
          failures = failures + 1
          tms.recompute()
        }
      }
    }
    */

    val additionalFacts: Seq[NormalRule] = Seq(
      fact("assign(m1,t1,0)"),
      fact("assign(m2,t2,0)"),
      fact("n_assign(m1,t1,1)"), //notable, the above ('positive') assignments do not help much, since there is not knowledge about the expressed explicit condradiction with n_assign
      fact("n_assign(m1,t1,2)"),
      fact("n_assign(m1,t1,3)"),
      fact("n_assign(m1,t1,4)"),
      fact("n_assign(m1,t2,0)"),
      fact("n_assign(m1,t2,1)"),
      fact("n_assign(m1,t2,2)"),
      fact("n_assign(m1,t2,3)"),
      fact("n_assign(m1,t2,4)"),
      fact("n_assign(m2,t1,0)"),
      fact("n_assign(m2,t1,1)"),
      fact("n_assign(m2,t1,2)"),
      fact("n_assign(m2,t1,3)"),
      fact("n_assign(m2,t1,4)"),
      fact("n_assign(m2,t2,1)"),
      fact("n_assign(m2,t2,2)"),
      fact("n_assign(m2,t2,3)"),
      fact("n_assign(m2,t2,4)"),
      fact("some_assign(t1)"),
      fact("some_assign(t2)"),
      fact("finish(t1,1)"),
      fact("finish(t2,2)"),
      fact("busy_at(m1,0)"),
      fact("busy_at(m2,0)"),
      fact("busy_at(m2,1)"),
      fact("idle_at(m1,1)"),
      fact("idle_at(m1,2)"),
      fact("idle_at(m1,3)"),
      fact("idle_at(m1,4)"),
      fact("idle_at(m2,2)"),
      fact("idle_at(m2,3)"),
      fact("idle_at(m2,4)"),
      fact("some_busy_at(0)"),
      fact("some_busy_at(1)"),
      fact("none_busy_at(2)"),
      fact("none_busy_at(3)"),
      fact("none_busy_at(4)"),
      fact("n_max_finish(1)"),
      fact("max_finish(2)")
    ) map asAspRule

    var haveModel: Boolean = false
    for (f <- additionalFacts) {
      tms.add(f)
      tms.getModel match {
        case Some(model) => {
          val projectedModel = model filter (_.predicate.caption == "assign")
          if (!haveModel) {
            println("have it after inserting " + f)
            println("projected model: " + projectedModel)
            haveModel = true
          }
          assert(models contains projectedModel)
        }
        case None => {
          failures = failures + 1

          var attempts = 0
          while (tms.getModel == None && attempts < 50) {
            attempts = attempts + 1
            tms.recompute()
          }
          if (tms.getModel.isDefined){
            println("computed model after "+attempts+" recomputations")
            val projectedModel = tms.getModel.get filter (_.predicate.caption == "assign")
            assert(models contains projectedModel)
          } else {
            println("fail: "+f)
          }
          if (haveModel) println("failed again after inserting "+f)
          haveModel = false

        }
      }
    }

    println("failures: "+failures)
    val tabu = tms.tabu
    val currentRulesTabu = tabu.currentRulesTabu
    println("size of avoidance current map: "+currentRulesTabu.avoidanceMap.size)
    //println(tms.status)

  }

  test("bit 1") {

    val useGrounding = false //false = save time

    val asp = if (useGrounding) {

      /*
      highest_exponent(5). % 2^X
      max_level(M) :- highest_exponent(E), M = E - 1.
      level(0..M) :- max_level(M).
      */

      val bitEncodingProgram = LarsProgram(Seq[LarsRule](
        rule("bit(L,1) :- level(L), not bit(L,0)"),
        rule("bit(L,0) :- level(L), not bit(L,1)"),
        rule("flip(L) :- level(L), not n_flip(L)"),
        rule("n_flip(L) :- level(L), not flip(L)"),
        rule("use_bit(L,1) :- bit(L,1), not flip(L)"),
        rule("use_bit(L,0) :- bit(L,1), flip(L)"),
        rule("use_bit(L,0) :- bit(L,0), not flip(L)"),
        rule("use_bit(L,1) :- bit(L,0), flip(L)"),
        rule("sum_at(0,B) :- use_bit(0,B)"),
        rule("sum_at(L,C) :- sum_at(L0,C0), sum(L0,1,L), use_bit(L,1), pow(2,L,X), sum(C0,X,C), int(X), int(C)"),
        rule("sum_at(L,C) :- sum_at(L0,C), sum(L0,1,L), use_bit(L,0), int(C)"),
        rule("id(C) :- max_level(M), sum_at(M,C)"),
        rule("xx1 :- id(C), mod(C,10,K), geq(K,8), int(K), not xx1") //geq(K,8) this allows 80% of assignments
      ))

      val highestExponent = 5 //2^X
      val maxLevel = highestExponent - 1

      val levels:Seq[Int] = for (l <- 0 to maxLevel) yield l
      val ints:Seq[Int] = for (i <- 0 to Math.pow(2,highestExponent).toInt) yield i

      val facts: Seq[LarsRule] =
        (levels map (l => fact("level("+l+")"))) ++
          (ints map (i => fact("int("+i+")"))) :+
          fact("max_level("+maxLevel+")")


      val inputProgram = LarsProgram(bitEncodingProgram.rules ++ facts)

      //println(inputProgram)
      val grounder = printTime("grounding time") {
        Grounder(inputProgram)
      }

      //grounder.groundRules foreach (r => println(LarsProgram(Seq(r))))
      println(LarsProgram(grounder.groundRules))

      //printInspect(grounder)

      //
      // variables to iterate over
      //

      val possibleValuesMap: Map[Variable, Set[Value]] = Map(
        v("L") -> (levels map (IntValue(_)) toSet),
        v("L0") -> (levels map (IntValue(_)) toSet),
        v("C") -> (ints map (IntValue(_)) toSet),
        v("C0") -> (ints map (IntValue(_)) toSet),
        v("X") -> (ints map (IntValue(_)) toSet),
        v("B") -> (Set(0, 1) map (IntValue(_)) toSet),
        v("M") -> Set(IntValue(maxLevel)),
        v("K") -> (ints map (IntValue(_)) toSet)
      )

      inputProgram.rules foreach { r =>
        for ((variable, possibleValues) <- possibleValuesMap) {
          if (r.variables.contains(variable)) {
            if (grounder.inspect.possibleValuesForVariable(r, variable) != possibleValues) {
              println("rule: " + r)
              println("variable: " + variable.name)
              println("expected values: " + possibleValues)
              println("actual values:   " + grounder.inspect.possibleValuesForVariable(r, variable))
              assert(false)
            }
          }
        }
      }

      println("#rules in ground program: " + grounder.groundProgram.rules.size)

      // printInspect(grounder)

      asAspProgram(grounder.groundProgram)

    } else { //no grounding, use predefined program
      val filename = "/ground-programs/bit1.rules"
      val groundLarsProgram = readProgramFromFile(filename)
      asAspProgram(groundLarsProgram)
    }

    //    println("contradiction atoms:")
    //    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println

    val tms = new JtmsLearn()
    tms.shuffle = false
    printTime("time to all all ground rules") {
      asp.rules foreach tms.add
    }

    //asp.rules foreach println

    var failures = if (tms.getModel == None) 1 else 0
    var modelFoundInAttempt:Int = 0

    val start2 = System.currentTimeMillis()
    var end2 = -1L

    for (attempt <- 1 to 10000) {
      tms.getModel match {
        case Some(model) => {
//          val projectedModel = model filter (Set("use_bit","id") contains _.predicate.caption)
//          println("projected model: "+projectedModel)
          end2 = System.currentTimeMillis()
          if (modelFoundInAttempt==0) modelFoundInAttempt = attempt
        }
        case None => {
          //println("fail")
          failures = failures + 1
          tms.recompute()
        }
      }
    }

    tms.getModel match {
      case Some(model) => {
        val projectedModel = model filter (Set("use_bit","id") contains _.predicate.caption)
        println("projected model: "+projectedModel)
        val time = (1.0*(end2-start2)/1000.0)
        println("time to compute model: "+time+" sec")
        println("after "+modelFoundInAttempt+" attempts, i.e., "+(time/(1.0*modelFoundInAttempt)+" sec/attempt"))
      }
      case _ =>
    }

    println("failures: "+failures)

    val tabu = tms.tabu
    val currentRulesTabu = tabu.currentRulesTabu
    println("size of avoidance current map: "+currentRulesTabu.avoidanceMap.size)
    //println(currentRulesTabu.avoidanceMap)
    //println(tms.status)


  }

  test("bit 2") {

    val useGrounding = true
    println("use grounding: "+useGrounding)

    val asp = if (useGrounding) {

      /*
      highest_exponent(5). % 2^X
      max_level(M) :- highest_exponent(E), M = E - 1.
      level(0..M) :- max_level(M).
      */

      val bitEncodingProgram = LarsProgram(Seq[LarsRule](
        rule("bit(L,1) :- level(L), not bit(L,0)"),
        rule("bit(L,0) :- level(L), not bit(L,1)"),
        rule("sum_at(0,B) :- bit(0,B)"),
        rule("sum_at(L,C) :- sum_at(L0,C0), sum(L0,1,L), bit(L,1), pow(2,L,X), sum(C0,X,C), int(X), int(C)"),
        rule("sum_at(L,C) :- sum_at(L0,C), sum(L0,1,L), bit(L,0), int(C)"),
        rule("id(C) :- max_level(M), sum_at(M,C)"),
        rule("xx1 :- id(C), mod(C,10,K), geq(K,2), int(K), not xx1")
      ))

      val highestExponent = 2 //2^X; prepared program has 2^7
      val maxLevel = highestExponent - 1

      val levels:Seq[Int] = for (l <- 0 to maxLevel) yield l
      val ints:Seq[Int] = for (i <- 0 to Math.pow(2,highestExponent).toInt) yield i

      val facts: Seq[LarsRule] =
        (levels map (l => fact("level("+l+")"))) ++
          (ints map (i => fact("int("+i+")"))) :+
          fact("max_level("+maxLevel+")")


      val inputProgram = LarsProgram(bitEncodingProgram.rules ++ facts)

      //println(inputProgram)
      val grounder = printTime("grounding time") {
        Grounder(inputProgram)
      }

      //grounder.groundRules foreach (r => println(LarsProgram(Seq(r))))
      println(LarsProgram(grounder.groundRules))

      //printInspect(grounder)

      //
      // variables to iterate over
      //

      val possibleValuesMap: Map[Variable, Set[Value]] = Map(
        v("L") -> (levels map (IntValue(_)) toSet),
        v("L0") -> (levels map (IntValue(_)) toSet),
        v("C") -> (ints map (IntValue(_)) toSet),
        v("C0") -> (ints map (IntValue(_)) toSet),
        v("X") -> (ints map (IntValue(_)) toSet),
        v("B") -> (Set(0, 1) map (IntValue(_)) toSet),
        v("M") -> Set(IntValue(maxLevel)),
        v("K") -> (ints map (IntValue(_)) toSet)
      )

      inputProgram.rules foreach { r =>
        for ((variable, possibleValues) <- possibleValuesMap) {
          if (r.variables.contains(variable)) {
            if (grounder.inspect.possibleValuesForVariable(r, variable) != possibleValues) {
              println("rule: " + r)
              println("variable: " + variable.name)
              println("expected values: " + possibleValues)
              println("actual values:   " + grounder.inspect.possibleValuesForVariable(r, variable))
              assert(false)
            }
          }
        }
      }

      println("#rules in ground program: " + grounder.groundProgram.rules.size)

      // printInspect(grounder)

      asAspProgram(grounder.groundProgram)

    } else { //no grounding, use predefined program
      val filename = "/ground-programs/bit2.rules"
      val groundLarsProgram = readProgramFromFile(filename)
      asAspProgram(groundLarsProgram)
    }

    //    println("contradiction atoms:")
    //    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println

    val tms = new JtmsLearn()
    tms.shuffle = false
    printTime("time to add all ground rules") {
      asp.rules foreach tms.add
    }

    //asp.rules foreach println

    var failures = if (tms.getModel == None) 1 else 0
    var modelFoundInAttempt:Int = 0

    val start2 = System.currentTimeMillis()
    var end2 = -1L

    /*
    tms.add(asAspRule(rule("bit(0,0)")))
    tms.add(asAspRule(rule("bit(1,0)")))
    tms.add(asAspRule(rule("bit(2,0)")))
    tms.add(asAspRule(rule("bit(3,1)")))
    */

    for (attempt <- 1 to 10000) {
      tms.getModel match {
        case Some(model) => {
          //          val projectedModel = model filter (Set("use_bit","id") contains _.predicate.caption)
          //          println("projected model: "+projectedModel)
          end2 = System.currentTimeMillis()
          if (modelFoundInAttempt==0) modelFoundInAttempt = attempt
        }
        case None => {
          //println("fail")
          failures = failures + 1
          tms.recompute()
        }
      }
    }

    tms.getModel match {
      case Some(model) => {
        val projectedModel = model filter (Set("bit","id") contains _.predicate.caption)
        println("projected model: "+projectedModel)
        val time = (1.0*(end2-start2)/1000.0)
        println("time to compute model: "+time+" sec")
        println("after "+modelFoundInAttempt+" attempts, i.e., "+(time/(1.0*modelFoundInAttempt)+" sec/attempt"))
      }
      case _ =>
    }

    println("failures: "+failures)

    val tabu = tms.tabu
    val currentRulesTabu = tabu.currentRulesTabu
    println("size of current avoidance map: "+currentRulesTabu.avoidanceMap.size)
    //println(currentRulesTabu.avoidanceMap)
    //println(tms.status)

  }



  test("bit 3") {
    //object eval extends BitProgram
    //val program = printTime("grounding time"){ eval.groundLarsProgram() }
    //println("#rules: "+program.rules.size)

    val timePoints = 250
    val tmsNames = Seq("greedy","learn")
    //val windowSizes = Seq(1,50,100)
    val windowSizes = Seq(10,100)
    val insertProbabilities = Seq[Double](0.001,0.01,0.1)
    //val insertProbabilities = Seq(0.0)
    val iterationsEach = 2

    //

    val useGroundProgramFromFile = true

    val highestExponent = 6 //2^X; prepared program has 2^7
    val maxLevel = highestExponent - 1

    val asp = if (useGroundProgramFromFile) {
      //val filename = "/ground-programs/bits6_mod64_geq1.rules"
      val filename = "/ground-programs/bits7_mod128_geq1.rules"
      val groundLarsProgram = readProgramFromFile(filename)
      asAspProgram(groundLarsProgram)
    } else {

      val nonGroundRules = Seq[LarsRule](
        rule("bit(L,1) :- level(L), not bit(L,0)"),
        rule("bit(L,0) :- level(L), not bit(L,1)"),
        rule("sum_at(0,B) :- bit(0,B)"),
        rule("sum_at(L,C) :- sum_at(L0,C0), sum(L0,1,L), bit(L,1), pow(2,L,X), sum(C0,X,C), int(X), int(C)"),
        rule("sum_at(L,C) :- sum_at(L0,C), sum(L0,1,L), bit(L,0), int(C)"),
        rule("id(C) :- max_level(M), sum_at(M,C)"),
        rule("xx1 :- id(C), mod(C,16,K), geq(K,4), int(K), not xx1"),
        rule("bit(L,0) :- level(L), from_window0(L)"), //!
        rule("bit(L,1) :- level(L), from_window1(L)") //!
      )

      val levels = Seq(fact(f"max_level(${maxLevel})")) ++ ((0 to maxLevel) map (i => fact(f"level($i)")))
      val ints = (0 to Math.pow(2, highestExponent).toInt) map (i => fact(f"int($i)"))

      val facts = levels ++ ints

      val inputProgram = LarsProgram(nonGroundRules ++ facts)

      val grounding = printTime("grounding") {
        Grounder(inputProgram)
      }
      println("#rules: " + grounding.groundRules.size)
      println(grounding.groundProgram)

      asAspProgram(grounding.groundProgram)

    }

    //

    for (windowSize <- windowSizes) {
      for (insertProbability <- insertProbabilities) {
        for (tmsName <- tmsNames) {
          for (iteration <- 1 to iterationsEach) {

            val tms = tmsName match {
              case "greedy" => new JtmsGreedy()
              case "learn" => new JtmsLearn()
            }

            println(f"\n${tmsName}#${iteration}, windowSize: ${windowSize}, insertProbability: ${insertProbability}")

            printTime("add ground rules") {
              asp.rules foreach tms.add
            }

            tms.getModel match {
              case Some(m) => println("initial model: " + (m filter (_.predicate.caption == "id")))
              case None => println("no initial model")
            }

            var failures = 0
            var models = 0

            printTime("runtime") {

              var printed = false

              //initialize
              for (t <- 1 to windowSize) {
                for (level <- 0 to maxLevel) {
                  val aspRule0 = asAspRule(rule(f"from_window($level) :- #signal0($level,$t)"))
                  val aspRule1 = asAspRule(rule(f"from_window($level) :- #signal1($level,$t)"))
                  tms.add(aspRule0)
                  tms.add(aspRule1)
                  if (!printed && tms.getModel.isDefined) {
                    println(f"add. t=$t, level: $level")
                    printed = true
                  }
                }
              }

              //actual loop
              var factMap = HashMap[Int,Set[NormalRule]]()
              for (t <- (windowSize + 1) to (windowSize + timePoints)) {
                for (level <- 0 to maxLevel) {
                  if (tms.random.nextDouble() < insertProbability) {
                    val fact = if (tms.random.nextDouble() < 0.5) {
                      asAspRule(rule(f"#signal0($level,$t)"))
                    } else {
                      asAspRule(rule(f"#signal1($level,$t)"))
                    }
                    tms.add(fact)
                    val set = factMap.getOrElse(t,Set())
                    factMap = factMap.updated(t,set + fact)
                  }
                }
                for (level <- 0 to maxLevel) {
                  val aspRule0 = asAspRule(rule(f"from_window0($level) :- #signal0($level,$t)"))
                  val aspRule1 = asAspRule(rule(f"from_window1($level) :- #signal1($level,$t)"))
                  tms.add(aspRule0)
                  tms.add(aspRule1)
                  if (!printed && tms.getModel.isDefined) {
                    println(f"add. t=$t, level: $level")
                    printed = true
                  }
                }
                for (level <- 0 to maxLevel) {
                  val aspRule0 = asAspRule(rule(f"from_window0($level) :- #signal0($level,${t - windowSize})"))
                  val aspRule1 = asAspRule(rule(f"from_window1($level) :- #signal1($level,${t - windowSize})"))
                  tms.remove(aspRule0)
                  tms.remove(aspRule1)
                  if (!printed && tms.getModel.isDefined) {
                    println(f"remove. t=$t, level: $level")
                    printed = true
                  }
                }
                factMap.get(t-windowSize) match {
                  case Some(facts) => {
                    facts foreach tms.remove
                    factMap = factMap - (t-windowSize)
                  }
                  case None =>
                }

                tms.getModel() match {
                  case Some(m) => models = models + 1 //println(m filter (_.predicate.caption == "id"))
                  case None => failures = failures + 1
                }
              }
            }

            println(f"models: ${models}/${timePoints} = ${(1.0*models) / timePoints}")
          }
        }
      }

    }

  }


}
