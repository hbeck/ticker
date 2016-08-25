package core.lars

import core.Model
import core._
import core.asp._
import jtms.JtmsGreedy
import org.scalatest.FunSuite

/**
  * Created by hb on 8/23/16.
  */
class GrounderTests extends FunSuite {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def atom(s:String): Atom = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("(")) return PredicateAtom(p(str))
    val commasOnly = str.substring(0,str.size-1).replace("(",",")
    val seq: Seq[String] = commasOnly.split(",").toSeq
    atom(seq)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def atom(ss:Seq[String]): Atom = {
    assert(ss.size>0)
    if (ss.size == 1)
      PredicateAtom(p(ss(0)))
    else {
      val p = Predicate(ss(0))
      val args = ss.tail map arg
      if (args forall (_.isInstanceOf[StringValue]))
        GroundAtomWithArguments(p, args map (_.asInstanceOf[Value]))
      else
        NonGroundAtom(p, args)
    }
  }

  test("parsing") {
    assert(atom("a")==PredicateAtom(p("a")))
    assert(atom("a(x)")==GroundAtomWithArguments(Predicate("a"),Seq[Value](StringValue("x"))))
    assert(atom("a(x,y)")==GroundAtomWithArguments(Predicate("a"),Seq[Value](StringValue("x"),StringValue("y"))))
    assert(atom("a(X)")==NonGroundAtom(Predicate("a"),Seq[Argument]("X")))
    assert(atom("a(X,y)")==NonGroundAtom(Predicate("a"),Seq[Argument]("X",StringValue("y"))))
    assert(atom("a(y,X)")==NonGroundAtom(Predicate("a"),Seq[Argument](StringValue("y"),"X")))
  }

  //
  //
  //

  def p(s:String) = Predicate(s)
  def strVal(s:String): Value = StringValue(s)
  def strVals(ss:String*): Set[Value] = ss map (StringValue(_)) toSet
  def v(s:String) = Variable(s)
  def arg(s:String): Argument = if (s.charAt(0).isUpper) Variable(s) else StringValue(s)

  def fact(s:String):LarsRule = LarsFact(atom(s))
  def rule(all:String): LarsRule = {
    if (!all.contains(" :- ")) return fact(all)
    val hbStr = all.split(" :- ")
    val head:HeadAtom = atom(hbStr(0)) //not Atom! using atom to parse predicate symbol
    val bodyParts = hbStr(1).split(", ")
    val posBodyParts = bodyParts filterNot (_.trim.startsWith("not"))
    val negBodyParts = bodyParts filter (_.trim.startsWith("not"))
    val pos:Set[ExtendedAtom] = posBodyParts map (atom(_)) toSet
    val neg:Set[ExtendedAtom] = negBodyParts map (atom(_)) toSet

    LarsRule(head,pos,neg)
  }

  def program(rules:LarsRule*):LarsProgram = LarsProgram(rules)
  def ground(p:LarsProgram) = Grounder(p).groundProgram

  //a(x,y) b(y,z) ==> use , only within atoms and use white space only to split atoms
  def parseSpaceSeparatedAtoms(s: String): Set[ExtendedAtom] = {
    s.split(" ") map (atom(_)) toSet
  }

  //use only for asp fragment!
  def asAspProgram(larsProgram: LarsProgram): NormalProgram = {
    val aspRules: Seq[NormalRule] = larsProgram.rules map (asAspRule(_))
    AspProgram(aspRules.toList)
  }

  def asAspRule(larsRule: LarsRule): NormalRule = {
    val head = larsRule.head.atom //we are not using @ here
    val pos = larsRule.pos map (_.asInstanceOf[Atom])
    val neg = larsRule.neg map (_.asInstanceOf[Atom])
    UserDefinedAspRule(head,pos,neg)
  }

  def modelFromClingo(s:String): Model = {
    s.split(" ") map (atom(_)) toSet
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

  test("gt12w") {

    val a1 = fact("a(x)")
    val b11 = fact("b(x,y1)")
    val b12 = fact("b(x,y2)")
    val c1 = fact("c(y1)")
    val c2 = fact("c(y2)")
    val d12 = fact("d(x,y2)")

    val r1 = rule("i(X,Y) :- a(X), i(X,Y), w_sig1(Y)")
    val r2 = rule("i(X,Y) :- j(X,Y), not d(X,Y), not w_sig2(X,Y)")
    val r3 = rule("j(X,Y) :- b(X,Z), c(Y), not w_sig3(X,Z,Y)")

    val p = program(a1,b11,b12,c1,c2,d12,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y1) :- a(x), i(x,y1), w_sig1(y1)"),
      rule("i(x,y2) :- a(x), i(x,y2), w_sig1(y2)"),
      rule("i(x,y1) :- j(x,y1), not d(x,y1), not w_sig2(x,y1)"),
      rule("i(x,y2) :- j(x,y2), not d(x,y2), not w_sig2(x,y2)"),
      rule("j(x,y1) :- b(x,y1), c(y1), not w_sig3(x,y1,y1)"),
      rule("j(x,y2) :- b(x,y1), c(y2), not w_sig3(x,y1,y2)"),
      rule("j(x,y1) :- b(x,y2), c(y1), not w_sig3(x,y2,y1)"),
      rule("j(x,y2) :- b(x,y2), c(y2), not w_sig3(x,y2,y2)")
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

  //TODO window atoms

  //
  //
  //

  def printInspect(grounder: Grounder): Unit = {
    val i = grounder.inspect

    println("facts: atoms:")
    println("  ground atoms:     "+i.groundFactAtoms)
    println("  non-ground preds: "+i.nonGroundFactAtomPredicates)
    println("intensional:")
    println("  ground atoms:     "+i.groundIntensionalAtoms)
    println("  non-ground preds: "+i.nonGroundIntensionalPredicates)
    println()
    println("non-ground fact atoms/var in rule:\n")
    printNestedMap(i.nonGroundFactAtomsPerVariableInRule)
    println()
    println("non-ground intensional atoms/var in rule:\n")
    printNestedMap(i.nonGroundIntensionalAtomsPerVariableInRule)
    println()
    println("ground fact atom values lookup:\n")
    printNestedMap(i.groundFactAtomValuesLookup)
    println()
    println("values for predicate arg:\n")
    printNestedMap(i.valuesForPredicateArg)

  }

  def printNestedMap[T1,T2,T3](map: Map[T1,Map[T2,Set[T3]]]): Unit = {
    for ((k,v) <- map) {
      println(k+":")
      for ((k2,set) <- v) {
        print("  "+k2+" -> {")
        if (set.nonEmpty){
          print(set.head)
          if (set.size > 1) {
            set.tail foreach (elem => print(", "+elem))
          }
        }
        println("}")
      }
    }
  }

}
