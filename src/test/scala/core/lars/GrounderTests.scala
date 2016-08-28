package core.lars

import core.asp._
import core.{Model, _}
import jtms.{JtmsGreedy, JtmsLearn}
import org.scalatest.FunSuite

/**
  * Created by hb on 8/23/16.
  */
class GrounderTests extends FunSuite {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def atom(s:String): ExtendedAtom = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("(")) return noArgsAtom(str)
    val commasOnly = str.substring(0,str.size-1).replace("(",",")
    val seq: Seq[String] = commasOnly.split(",").toSeq
    atom(seq)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def atom(ss:Seq[String]): ExtendedAtom = {
    assert(ss.size>0)
    if (ss.size == 1)
      noArgsAtom(ss(0))
    else {
      var s = ss(0)
      var isWindowAtom = false
      if (s.startsWith("w_")) { //convention for name of window atom
        isWindowAtom = true
        s = s.split("_")(1)
      }
      val p = Predicate(s)
      val args = ss.tail map arg
      val atom = Atom(p,args)
//        if (args forall (_.isInstanceOf[Value]))
//          GroundAtomWithArguments(p, args map (_.asInstanceOf[Value]))
//        else
//          NonGroundAtom(p, args)
      if (isWindowAtom)
        WindowAtom(SlidingTimeWindow(7),Diamond,atom) //doesn't matter which one
      else
        atom
    }
  }

  def noArgsAtom(s:String):GroundAtom = {
    val pred = Predicate(s)
    if (s.startsWith("xx"))
      ContradictionAtom(pred)
    else
      PredicateAtom(pred)

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
  def strVals(ss:List[String]): Set[Value] = ss map (StringValue(_)) toSet
  def intVals(list: String*): Set[Value] = list map (IntValue(_)) toSet

  def v(s:String) = Variable(s)
  def arg(s:String): Argument = if (s.charAt(0).isUpper) Variable(s) else Value(s)

  def fact(s:String):LarsRule = LarsFact(atom(s).asInstanceOf[Atom])
  def rule(all:String): LarsRule = {
    if (!all.contains(" :- ")) return fact(all)
    val hbStr = all.split(" :- ")
    val head:HeadAtom = atom(hbStr(0)).asInstanceOf[Atom] //not *A*tom! using atom to parse predicate symbol
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
    s.split(" ") map (atom(_).asInstanceOf[Atom]) toSet
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

    val r1 = rule("i(X,Y) :- a(X,Y), b(Y), w_sig2(X,Y)")
    val r2 = rule("i(z,z) :- i(X,Y), w_sig1(X), not d(X,Y)")
    val r3 = rule("j(X,Y) :- i(X,Y), w_sig1(X), w_sig3(X,X,Y)")

    val p = program(a,b,r1,r2,r3)

    val manualGrounding: Set[LarsRule] = Set(
      rule("i(x,y) :- a(x,y), b(y), w_sig2(x,y)"),
      rule("i(z,z) :- i(x,y), w_sig1(x), not d(x,y)"),
      rule("i(z,z) :- i(z,z), w_sig1(z), not d(z,z)"),
      rule("i(z,z) :- i(z,y), w_sig1(z), not d(z,y)"),
      rule("i(z,z) :- i(x,z), w_sig1(x), not d(x,z)"),
      rule("j(z,y) :- i(z,y), w_sig1(z), w_sig3(z,z,y)"),
      rule("j(x,z) :- i(x,z), w_sig1(x), w_sig3(x,x,z)"),
      rule("j(x,y) :- i(x,y), w_sig1(x), w_sig3(x,x,y)"),
      rule("j(z,z) :- i(z,z), w_sig1(z), w_sig3(z,z,z)")
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

  def asInt(v:Value):Int = v match {
    case StringValue(s) => Integer.parseInt(s)
    case IntValue(i) => i
    case _ => throw new RuntimeException("argument %s cannot be casted to int".format(v))
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
      val start = System.currentTimeMillis()
      val grounder = Grounder(inputProgram)
      val end = System.currentTimeMillis()

      println("grounding time: " + (1.0 * (end - start) / 1000.0) + " sec.")

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
      val groundLarsProgram = LarsProgram(bitEncodingExample1GroundRules20PercentAcceptance map rule)
      asAspProgram(groundLarsProgram)
    }

    //    println("contradiction atoms:")
    //    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println

    val tms = new JtmsLearn()
    tms.shuffle = false
    val start1 = System.currentTimeMillis()
    asp.rules foreach tms.add
    val end1 = System.currentTimeMillis()
    println("time to add all ground rules: "+(1.0*(end1-start1)/1000.0)+" sec")

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

  val bitEncodingExample1GroundRules20PercentAcceptance = Seq[String](
    "bit(4,1) :- level(4), not bit(4,0)",
    "bit(1,1) :- level(1), not bit(1,0)",
    "bit(2,1) :- level(2), not bit(2,0)",
    "bit(3,1) :- level(3), not bit(3,0)",
    "bit(0,1) :- level(0), not bit(0,0)",
    "bit(1,0) :- level(1), not bit(1,1)",
    "bit(0,0) :- level(0), not bit(0,1)",
    "bit(3,0) :- level(3), not bit(3,1)",
    "bit(2,0) :- level(2), not bit(2,1)",
    "bit(4,0) :- level(4), not bit(4,1)",
    "flip(2) :- level(2), not n_flip(2)",
    "flip(1) :- level(1), not n_flip(1)",
    "flip(0) :- level(0), not n_flip(0)",
    "flip(3) :- level(3), not n_flip(3)",
    "flip(4) :- level(4), not n_flip(4)",
    "n_flip(0) :- level(0), not flip(0)",
    "n_flip(3) :- level(3), not flip(3)",
    "n_flip(1) :- level(1), not flip(1)",
    "n_flip(4) :- level(4), not flip(4)",
    "n_flip(2) :- level(2), not flip(2)",
    "use_bit(4,1) :- bit(4,1), not flip(4)",
    "use_bit(3,1) :- bit(3,1), not flip(3)",
    "use_bit(1,1) :- bit(1,1), not flip(1)",
    "use_bit(2,1) :- bit(2,1), not flip(2)",
    "use_bit(0,1) :- bit(0,1), not flip(0)",
    "use_bit(4,0) :- bit(4,1), flip(4)",
    "use_bit(2,0) :- bit(2,1), flip(2)",
    "use_bit(1,0) :- bit(1,1), flip(1)",
    "use_bit(0,0) :- bit(0,1), flip(0)",
    "use_bit(3,0) :- bit(3,1), flip(3)",
    "use_bit(0,0) :- bit(0,0), not flip(0)",
    "use_bit(3,0) :- bit(3,0), not flip(3)",
    "use_bit(1,0) :- bit(1,0), not flip(1)",
    "use_bit(2,0) :- bit(2,0), not flip(2)",
    "use_bit(4,0) :- bit(4,0), not flip(4)",
    "use_bit(3,1) :- bit(3,0), flip(3)",
    "use_bit(1,1) :- bit(1,0), flip(1)",
    "use_bit(2,1) :- bit(2,0), flip(2)",
    "use_bit(4,1) :- bit(4,0), flip(4)",
    "use_bit(0,1) :- bit(0,0), flip(0)",
    "sum_at(0,1) :- use_bit(0,1)",
    "sum_at(0,0) :- use_bit(0,0)",
    "sum_at(2,19) :- int(19), int(4), sum_at(1,15), use_bit(2,1)",
    "sum_at(3,16) :- int(8), sum_at(2,8), use_bit(3,1), int(16)",
    "sum_at(1,10) :- sum_at(0,8), int(10), use_bit(1,1), int(2)",
    "sum_at(1,31) :- int(31), sum_at(0,29), use_bit(1,1), int(2)",
    "sum_at(3,30) :- int(8), int(30), use_bit(3,1), sum_at(2,22)",
    "sum_at(4,30) :- int(30), use_bit(4,1), int(16), sum_at(3,14)",
    "sum_at(2,28) :- int(4), int(28), use_bit(2,1), sum_at(1,24)",
    "sum_at(4,32) :- int(32), use_bit(4,1), int(16), sum_at(3,16)",
    "sum_at(1,14) :- sum_at(0,12), use_bit(1,1), int(14), int(2)",
    "sum_at(3,27) :- int(8), int(27), sum_at(2,19), use_bit(3,1)",
    "sum_at(3,24) :- sum_at(2,16), int(8), int(24), use_bit(3,1)",
    "sum_at(2,23) :- int(4), int(23), sum_at(1,19), use_bit(2,1)",
    "sum_at(2,18) :- int(4), int(18), use_bit(2,1), sum_at(1,14)",
    "sum_at(3,17) :- sum_at(2,9), int(17), int(8), use_bit(3,1)",
    "sum_at(3,26) :- int(8), int(26), use_bit(3,1), sum_at(2,18)",
    "sum_at(3,9) :- int(8), int(9), use_bit(3,1), sum_at(2,1)",
    "sum_at(1,7) :- sum_at(0,5), use_bit(1,1), int(7), int(2)",
    "sum_at(3,31) :- sum_at(2,23), int(31), int(8), use_bit(3,1)",
    "sum_at(2,12) :- int(4), sum_at(1,8), use_bit(2,1), int(12)",
    "sum_at(2,6) :- int(4), int(6), sum_at(1,2), use_bit(2,1)",
    "sum_at(4,28) :- int(28), sum_at(3,12), use_bit(4,1), int(16)",
    "sum_at(4,23) :- int(23), use_bit(4,1), sum_at(3,7), int(16)",
    "sum_at(4,16) :- use_bit(4,1), sum_at(3,0), int(16)",
    "sum_at(3,12) :- int(8), sum_at(2,4), use_bit(3,1), int(12)",
    "sum_at(2,22) :- int(4), use_bit(2,1), sum_at(1,18), int(22)",
    "sum_at(4,18) :- int(18), sum_at(3,2), use_bit(4,1), int(16)",
    "sum_at(3,23) :- int(23), int(8), use_bit(3,1), sum_at(2,15)",
    "sum_at(3,8) :- int(8), use_bit(3,1), sum_at(2,0)",
    "sum_at(1,22) :- use_bit(1,1), sum_at(0,20), int(22), int(2)",
    "sum_at(3,29) :- int(29), sum_at(2,21), int(8), use_bit(3,1)",
    "sum_at(4,21) :- sum_at(3,5), int(21), use_bit(4,1), int(16)",
    "sum_at(1,15) :- int(15), sum_at(0,13), use_bit(1,1), int(2)",
    "sum_at(2,17) :- int(4), int(17), sum_at(1,13), use_bit(2,1)",
    "sum_at(4,26) :- int(26), use_bit(4,1), int(16), sum_at(3,10)",
    "sum_at(2,5) :- int(4), int(5), use_bit(2,1), sum_at(1,1)",
    "sum_at(3,25) :- int(25), int(8), use_bit(3,1), sum_at(2,17)",
    "sum_at(2,27) :- int(4), int(27), use_bit(2,1), sum_at(1,23)",
    "sum_at(1,12) :- sum_at(0,10), use_bit(1,1), int(12), int(2)",
    "sum_at(4,22) :- use_bit(4,1), int(16), sum_at(3,6), int(22)",
    "sum_at(3,20) :- int(20), int(8), sum_at(2,12), use_bit(3,1)",
    "sum_at(1,9) :- int(9), use_bit(1,1), sum_at(0,7), int(2)",
    "sum_at(1,29) :- int(29), sum_at(0,27), use_bit(1,1), int(2)",
    "sum_at(1,3) :- sum_at(0,1), use_bit(1,1), int(3), int(2)",
    "sum_at(1,13) :- int(13), use_bit(1,1), sum_at(0,11), int(2)",
    "sum_at(2,16) :- int(4), sum_at(1,12), use_bit(2,1), int(16)",
    "sum_at(2,14) :- int(4), sum_at(1,10), int(14), use_bit(2,1)",
    "sum_at(4,24) :- use_bit(4,1), int(24), int(16), sum_at(3,8)",
    "sum_at(2,4) :- int(4), sum_at(1,0), use_bit(2,1)",
    "sum_at(2,11) :- sum_at(1,7), int(4), int(11), use_bit(2,1)",
    "sum_at(3,28) :- sum_at(2,20), int(28), int(8), use_bit(3,1)",
    "sum_at(1,11) :- sum_at(0,9), int(11), use_bit(1,1), int(2)",
    "sum_at(2,20) :- int(4), int(20), use_bit(2,1), sum_at(1,16)",
    "sum_at(2,21) :- int(4), sum_at(1,17), int(21), use_bit(2,1)",
    "sum_at(2,8) :- int(4), int(8), use_bit(2,1), sum_at(1,4)",
    "sum_at(2,9) :- int(4), sum_at(1,5), int(9), use_bit(2,1)",
    "sum_at(3,15) :- sum_at(2,7), int(8), int(15), use_bit(3,1)",
    "sum_at(4,25) :- sum_at(3,9), int(25), use_bit(4,1), int(16)",
    "sum_at(1,25) :- sum_at(0,23), int(25), use_bit(1,1), int(2)",
    "sum_at(3,22) :- int(8), use_bit(3,1), sum_at(2,14), int(22)",
    "sum_at(4,27) :- int(27), use_bit(4,1), sum_at(3,11), int(16)",
    "sum_at(2,26) :- int(4), int(26), use_bit(2,1), sum_at(1,22)",
    "sum_at(4,17) :- int(17), sum_at(3,1), use_bit(4,1), int(16)",
    "sum_at(3,13) :- int(8), int(13), use_bit(3,1), sum_at(2,5)",
    "sum_at(1,27) :- sum_at(0,25), int(27), use_bit(1,1), int(2)",
    "sum_at(1,5) :- sum_at(0,3), int(5), use_bit(1,1), int(2)",
    "sum_at(1,17) :- int(17), use_bit(1,1), int(2), sum_at(0,15)",
    "sum_at(4,29) :- int(29), sum_at(3,13), use_bit(4,1), int(16)",
    "sum_at(2,25) :- int(4), int(25), use_bit(2,1), sum_at(1,21)",
    "sum_at(1,26) :- int(26), use_bit(1,1), sum_at(0,24), int(2)",
    "sum_at(3,32) :- sum_at(2,24), int(8), int(32), use_bit(3,1)",
    "sum_at(1,24) :- sum_at(0,22), int(24), use_bit(1,1), int(2)",
    "sum_at(2,10) :- int(4), int(10), sum_at(1,6), use_bit(2,1)",
    "sum_at(1,23) :- int(23), use_bit(1,1), sum_at(0,21), int(2)",
    "sum_at(4,20) :- sum_at(3,4), int(20), use_bit(4,1), int(16)",
    "sum_at(1,19) :- int(19), sum_at(0,17), use_bit(1,1), int(2)",
    "sum_at(1,6) :- sum_at(0,4), int(6), use_bit(1,1), int(2)",
    "sum_at(3,10) :- sum_at(2,2), int(10), int(8), use_bit(3,1)",
    "sum_at(1,4) :- int(4), sum_at(0,2), use_bit(1,1), int(2)",
    "sum_at(2,31) :- int(4), int(31), sum_at(1,27), use_bit(2,1)",
    "sum_at(1,16) :- sum_at(0,14), use_bit(1,1), int(16), int(2)",
    "sum_at(1,8) :- int(8), use_bit(1,1), sum_at(0,6), int(2)",
    "sum_at(1,20) :- int(20), sum_at(0,18), use_bit(1,1), int(2)",
    "sum_at(3,21) :- int(8), sum_at(2,13), int(21), use_bit(3,1)",
    "sum_at(1,18) :- int(18), sum_at(0,16), use_bit(1,1), int(2)",
    "sum_at(1,2) :- sum_at(0,0), use_bit(1,1), int(2)",
    "sum_at(2,24) :- int(4), int(24), use_bit(2,1), sum_at(1,20)",
    "sum_at(4,19) :- int(19), use_bit(4,1), sum_at(3,3), int(16)",
    "sum_at(3,18) :- int(18), int(8), use_bit(3,1), sum_at(2,10)",
    "sum_at(1,21) :- sum_at(0,19), int(21), use_bit(1,1), int(2)",
    "sum_at(2,15) :- int(4), int(15), use_bit(2,1), sum_at(1,11)",
    "sum_at(1,32) :- int(32), use_bit(1,1), sum_at(0,30), int(2)",
    "sum_at(2,13) :- int(4), sum_at(1,9), int(13), use_bit(2,1)",
    "sum_at(3,19) :- int(19), int(8), use_bit(3,1), sum_at(2,11)",
    "sum_at(2,32) :- sum_at(1,28), int(4), int(32), use_bit(2,1)",
    "sum_at(4,31) :- int(31), sum_at(3,15), use_bit(4,1), int(16)",
    "sum_at(2,7) :- int(4), sum_at(1,3), use_bit(2,1), int(7)",
    "sum_at(3,11) :- int(8), int(11), sum_at(2,3), use_bit(3,1)",
    "sum_at(2,29) :- int(29), int(4), use_bit(2,1), sum_at(1,25)",
    "sum_at(3,14) :- sum_at(2,6), int(8), use_bit(3,1), int(14)",
    "sum_at(1,28) :- int(28), use_bit(1,1), sum_at(0,26), int(2)",
    "sum_at(2,30) :- int(4), sum_at(1,26), int(30), use_bit(2,1)",
    "sum_at(1,30) :- int(30), use_bit(1,1), sum_at(0,28), int(2)",
    "sum_at(1,13) :- sum_at(0,13), use_bit(1,0), int(13)",
    "sum_at(1,21) :- sum_at(0,21), use_bit(1,0), int(21)",
    "sum_at(3,2) :- sum_at(2,2), use_bit(3,0), int(2)",
    "sum_at(1,14) :- sum_at(0,14), use_bit(1,0), int(14)",
    "sum_at(3,25) :- sum_at(2,25), use_bit(3,0), int(25)",
    "sum_at(1,31) :- sum_at(0,31), use_bit(1,0), int(31)",
    "sum_at(3,0) :- sum_at(2,0), use_bit(3,0), int(0)",
    "sum_at(4,29) :- sum_at(3,29), use_bit(4,0), int(29)",
    "sum_at(3,18) :- sum_at(2,18), use_bit(3,0), int(18)",
    "sum_at(2,19) :- sum_at(1,19), use_bit(2,0), int(19)",
    "sum_at(4,28) :- sum_at(3,28), use_bit(4,0), int(28)",
    "sum_at(4,17) :- sum_at(3,17), use_bit(4,0), int(17)",
    "sum_at(2,7) :- sum_at(1,7), use_bit(2,0), int(7)",
    "sum_at(4,31) :- sum_at(3,31), use_bit(4,0), int(31)",
    "sum_at(4,0) :- sum_at(3,0), use_bit(4,0), int(0)",
    "sum_at(3,11) :- sum_at(2,11), use_bit(3,0), int(11)",
    "sum_at(1,30) :- sum_at(0,30), use_bit(1,0), int(30)",
    "sum_at(3,23) :- sum_at(2,23), use_bit(3,0), int(23)",
    "sum_at(2,21) :- sum_at(1,21), use_bit(2,0), int(21)",
    "sum_at(4,5) :- sum_at(3,5), use_bit(4,0), int(5)",
    "sum_at(4,24) :- sum_at(3,24), use_bit(4,0), int(24)",
    "sum_at(1,2) :- sum_at(0,2), use_bit(1,0), int(2)",
    "sum_at(3,8) :- sum_at(2,8), use_bit(3,0), int(8)",
    "sum_at(2,27) :- sum_at(1,27), use_bit(2,0), int(27)",
    "sum_at(4,13) :- sum_at(3,13), use_bit(4,0), int(13)",
    "sum_at(4,4) :- sum_at(3,4), use_bit(4,0), int(4)",
    "sum_at(1,32) :- sum_at(0,32), use_bit(1,0), int(32)",
    "sum_at(3,16) :- sum_at(2,16), use_bit(3,0), int(16)",
    "sum_at(2,18) :- sum_at(1,18), use_bit(2,0), int(18)",
    "sum_at(2,15) :- sum_at(1,15), use_bit(2,0), int(15)",
    "sum_at(4,21) :- sum_at(3,21), use_bit(4,0), int(21)",
    "sum_at(4,3) :- sum_at(3,3), use_bit(4,0), int(3)",
    "sum_at(2,10) :- sum_at(1,10), use_bit(2,0), int(10)",
    "sum_at(1,27) :- sum_at(0,27), use_bit(1,0), int(27)",
    "sum_at(1,19) :- sum_at(0,19), use_bit(1,0), int(19)",
    "sum_at(2,26) :- sum_at(1,26), use_bit(2,0), int(26)",
    "sum_at(1,15) :- sum_at(0,15), use_bit(1,0), int(15)",
    "sum_at(4,22) :- sum_at(3,22), use_bit(4,0), int(22)",
    "sum_at(2,12) :- sum_at(1,12), use_bit(2,0), int(12)",
    "sum_at(2,28) :- sum_at(1,28), use_bit(2,0), int(28)",
    "sum_at(3,13) :- sum_at(2,13), use_bit(3,0), int(13)",
    "sum_at(3,22) :- sum_at(2,22), use_bit(3,0), int(22)",
    "sum_at(3,29) :- sum_at(2,29), use_bit(3,0), int(29)",
    "sum_at(2,23) :- sum_at(1,23), use_bit(2,0), int(23)",
    "sum_at(2,9) :- sum_at(1,9), use_bit(2,0), int(9)",
    "sum_at(4,27) :- sum_at(3,27), use_bit(4,0), int(27)",
    "sum_at(1,1) :- sum_at(0,1), use_bit(1,0), int(1)",
    "sum_at(4,12) :- sum_at(3,12), use_bit(4,0), int(12)",
    "sum_at(3,4) :- sum_at(2,4), use_bit(3,0), int(4)",
    "sum_at(1,22) :- sum_at(0,22), use_bit(1,0), int(22)",
    "sum_at(2,25) :- sum_at(1,25), use_bit(2,0), int(25)",
    "sum_at(2,13) :- sum_at(1,13), use_bit(2,0), int(13)",
    "sum_at(1,5) :- sum_at(0,5), use_bit(1,0), int(5)",
    "sum_at(2,31) :- sum_at(1,31), use_bit(2,0), int(31)",
    "sum_at(4,18) :- sum_at(3,18), use_bit(4,0), int(18)",
    "sum_at(3,24) :- sum_at(2,24), use_bit(3,0), int(24)",
    "sum_at(3,3) :- sum_at(2,3), use_bit(3,0), int(3)",
    "sum_at(4,25) :- sum_at(3,25), use_bit(4,0), int(25)",
    "sum_at(3,9) :- sum_at(2,9), use_bit(3,0), int(9)",
    "sum_at(3,28) :- sum_at(2,28), use_bit(3,0), int(28)",
    "sum_at(1,28) :- sum_at(0,28), use_bit(1,0), int(28)",
    "sum_at(1,17) :- sum_at(0,17), use_bit(1,0), int(17)",
    "sum_at(4,23) :- sum_at(3,23), use_bit(4,0), int(23)",
    "sum_at(1,20) :- sum_at(0,20), use_bit(1,0), int(20)",
    "sum_at(4,30) :- sum_at(3,30), use_bit(4,0), int(30)",
    "sum_at(2,29) :- sum_at(1,29), use_bit(2,0), int(29)",
    "sum_at(2,32) :- sum_at(1,32), use_bit(2,0), int(32)",
    "sum_at(4,20) :- sum_at(3,20), use_bit(4,0), int(20)",
    "sum_at(1,23) :- sum_at(0,23), use_bit(1,0), int(23)",
    "sum_at(3,6) :- sum_at(2,6), use_bit(3,0), int(6)",
    "sum_at(4,9) :- sum_at(3,9), use_bit(4,0), int(9)",
    "sum_at(1,11) :- sum_at(0,11), use_bit(1,0), int(11)",
    "sum_at(3,31) :- sum_at(2,31), use_bit(3,0), int(31)",
    "sum_at(4,7) :- sum_at(3,7), use_bit(4,0), int(7)",
    "sum_at(4,14) :- sum_at(3,14), use_bit(4,0), int(14)",
    "sum_at(4,32) :- sum_at(3,32), use_bit(4,0), int(32)",
    "sum_at(3,20) :- sum_at(2,20), use_bit(3,0), int(20)",
    "sum_at(1,12) :- sum_at(0,12), use_bit(1,0), int(12)",
    "sum_at(2,17) :- sum_at(1,17), use_bit(2,0), int(17)",
    "sum_at(2,5) :- sum_at(1,5), use_bit(2,0), int(5)",
    "sum_at(3,15) :- sum_at(2,15), use_bit(3,0), int(15)",
    "sum_at(2,14) :- sum_at(1,14), use_bit(2,0), int(14)",
    "sum_at(2,8) :- sum_at(1,8), use_bit(2,0), int(8)",
    "sum_at(2,20) :- sum_at(1,20), use_bit(2,0), int(20)",
    "sum_at(3,10) :- sum_at(2,10), use_bit(3,0), int(10)",
    "sum_at(1,4) :- sum_at(0,4), use_bit(1,0), int(4)",
    "sum_at(1,29) :- sum_at(0,29), use_bit(1,0), int(29)",
    "sum_at(1,16) :- sum_at(0,16), use_bit(1,0), int(16)",
    "sum_at(3,19) :- sum_at(2,19), use_bit(3,0), int(19)",
    "sum_at(3,32) :- sum_at(2,32), use_bit(3,0), int(32)",
    "sum_at(2,11) :- sum_at(1,11), use_bit(2,0), int(11)",
    "sum_at(4,1) :- sum_at(3,1), use_bit(4,0), int(1)",
    "sum_at(1,26) :- sum_at(0,26), use_bit(1,0), int(26)",
    "sum_at(2,16) :- sum_at(1,16), use_bit(2,0), int(16)",
    "sum_at(2,3) :- sum_at(1,3), use_bit(2,0), int(3)",
    "sum_at(4,10) :- sum_at(3,10), use_bit(4,0), int(10)",
    "sum_at(4,11) :- sum_at(3,11), use_bit(4,0), int(11)",
    "sum_at(1,7) :- sum_at(0,7), use_bit(1,0), int(7)",
    "sum_at(1,24) :- sum_at(0,24), use_bit(1,0), int(24)",
    "sum_at(4,26) :- sum_at(3,26), use_bit(4,0), int(26)",
    "sum_at(3,5) :- sum_at(2,5), use_bit(3,0), int(5)",
    "sum_at(2,1) :- sum_at(1,1), use_bit(2,0), int(1)",
    "sum_at(3,27) :- sum_at(2,27), use_bit(3,0), int(27)",
    "sum_at(2,2) :- sum_at(1,2), use_bit(2,0), int(2)",
    "sum_at(1,10) :- sum_at(0,10), use_bit(1,0), int(10)",
    "sum_at(4,16) :- sum_at(3,16), use_bit(4,0), int(16)",
    "sum_at(2,30) :- sum_at(1,30), use_bit(2,0), int(30)",
    "sum_at(1,8) :- sum_at(0,8), use_bit(1,0), int(8)",
    "sum_at(2,0) :- sum_at(1,0), use_bit(2,0), int(0)",
    "sum_at(1,18) :- sum_at(0,18), use_bit(1,0), int(18)",
    "sum_at(2,6) :- sum_at(1,6), use_bit(2,0), int(6)",
    "sum_at(2,4) :- sum_at(1,4), use_bit(2,0), int(4)",
    "sum_at(1,0) :- sum_at(0,0), use_bit(1,0), int(0)",
    "sum_at(3,30) :- sum_at(2,30), use_bit(3,0), int(30)",
    "sum_at(4,19) :- sum_at(3,19), use_bit(4,0), int(19)",
    "sum_at(3,21) :- sum_at(2,21), use_bit(3,0), int(21)",
    "sum_at(3,17) :- sum_at(2,17), use_bit(3,0), int(17)",
    "sum_at(4,15) :- sum_at(3,15), use_bit(4,0), int(15)",
    "sum_at(3,1) :- sum_at(2,1), use_bit(3,0), int(1)",
    "sum_at(1,9) :- sum_at(0,9), use_bit(1,0), int(9)",
    "sum_at(3,12) :- sum_at(2,12), use_bit(3,0), int(12)",
    "sum_at(3,26) :- sum_at(2,26), use_bit(3,0), int(26)",
    "sum_at(3,7) :- sum_at(2,7), use_bit(3,0), int(7)",
    "sum_at(1,3) :- sum_at(0,3), use_bit(1,0), int(3)",
    "sum_at(2,24) :- sum_at(1,24), use_bit(2,0), int(24)",
    "sum_at(4,8) :- sum_at(3,8), use_bit(4,0), int(8)",
    "sum_at(4,6) :- sum_at(3,6), use_bit(4,0), int(6)",
    "sum_at(2,22) :- sum_at(1,22), use_bit(2,0), int(22)",
    "sum_at(1,25) :- sum_at(0,25), use_bit(1,0), int(25)",
    "sum_at(4,2) :- sum_at(3,2), use_bit(4,0), int(2)",
    "sum_at(3,14) :- sum_at(2,14), use_bit(3,0), int(14)",
    "sum_at(1,6) :- sum_at(0,6), use_bit(1,0), int(6)",
    "id(24) :- max_level(4), sum_at(4,24)",
    "id(30) :- max_level(4), sum_at(4,30)",
    "id(2) :- max_level(4), sum_at(4,2)",
    "id(26) :- max_level(4), sum_at(4,26)",
    "id(7) :- max_level(4), sum_at(4,7)",
    "id(6) :- max_level(4), sum_at(4,6)",
    "id(15) :- max_level(4), sum_at(4,15)",
    "id(17) :- max_level(4), sum_at(4,17)",
    "id(23) :- max_level(4), sum_at(4,23)",
    "id(18) :- max_level(4), sum_at(4,18)",
    "id(29) :- max_level(4), sum_at(4,29)",
    "id(10) :- max_level(4), sum_at(4,10)",
    "id(22) :- max_level(4), sum_at(4,22)",
    "id(25) :- max_level(4), sum_at(4,25)",
    "id(5) :- max_level(4), sum_at(4,5)",
    "id(31) :- max_level(4), sum_at(4,31)",
    "id(13) :- max_level(4), sum_at(4,13)",
    "id(16) :- max_level(4), sum_at(4,16)",
    "id(4) :- max_level(4), sum_at(4,4)",
    "id(3) :- max_level(4), sum_at(4,3)",
    "id(12) :- max_level(4), sum_at(4,12)",
    "id(28) :- max_level(4), sum_at(4,28)",
    "id(14) :- max_level(4), sum_at(4,14)",
    "id(1) :- max_level(4), sum_at(4,1)",
    "id(27) :- max_level(4), sum_at(4,27)",
    "id(21) :- max_level(4), sum_at(4,21)",
    "id(9) :- max_level(4), sum_at(4,9)",
    "id(8) :- max_level(4), sum_at(4,8)",
    "id(32) :- max_level(4), sum_at(4,32)",
    "id(19) :- max_level(4), sum_at(4,19)",
    "id(0) :- max_level(4), sum_at(4,0)",
    "id(20) :- max_level(4), sum_at(4,20)",
    "id(11) :- max_level(4), sum_at(4,11)",
    "xx1 :- id(19), int(9), not xx1",
    "xx1 :- id(29), int(9), not xx1",
    "xx1 :- id(28), int(8), not xx1",
    "xx1 :- id(9), int(9), not xx1",
    "xx1 :- id(18), int(8), not xx1",
    "xx1 :- id(8), int(8), not xx1",
    "level(0)",
    "level(1)",
    "level(2)",
    "level(3)",
    "level(4)",
    "int(0)",
    "int(1)",
    "int(2)",
    "int(3)",
    "int(4)",
    "int(5)",
    "int(6)",
    "int(7)",
    "int(8)",
    "int(9)",
    "int(10)",
    "int(11)",
    "int(12)",
    "int(13)",
    "int(14)",
    "int(15)",
    "int(16)",
    "int(17)",
    "int(18)",
    "int(19)",
    "int(20)",
    "int(21)",
    "int(22)",
    "int(23)",
    "int(24)",
    "int(25)",
    "int(26)",
    "int(27)",
    "int(28)",
    "int(29)",
    "int(30)",
    "int(31)",
    "int(32)",
    "max_level(4)"
  ) // end bitEncodingExample1GroundRules


  test("bit 2") {

    val useGrounding = true //false = save time

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
        rule("xx1 :- id(C), mod(C,10,K), geq(K,8), int(K), not xx1")
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
      val start = System.currentTimeMillis()
      val grounder = Grounder(inputProgram)
      val end = System.currentTimeMillis()

      println("grounding time: " + (1.0 * (end - start) / 1000.0) + " sec.")

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
      val groundLarsProgram = LarsProgram(bitEncodingExample2GroundRules20PercentAcceptance map rule)
      asAspProgram(groundLarsProgram)
    }

    //    println("contradiction atoms:")
    //    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println

    val tms = new JtmsLearn()
    tms.shuffle = false
    val start1 = System.currentTimeMillis()
    asp.rules foreach tms.add
    val end1 = System.currentTimeMillis()
    println("time to add all ground rules: "+(1.0*(end1-start1)/1000.0)+" sec")

    //asp.rules foreach println

    var failures = if (tms.getModel == None) 1 else 0
    var modelFoundInAttempt:Int = 0

    val start2 = System.currentTimeMillis()
    var end2 = -1L

    tms.add(asAspRule(rule("bit(0,0)")))
    tms.add(asAspRule(rule("bit(1,0)")))
    tms.add(asAspRule(rule("bit(2,0)")))
    tms.add(asAspRule(rule("bit(3,1)")))

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
    println("size of avoidance current map: "+currentRulesTabu.avoidanceMap.size)
    //println(currentRulesTabu.avoidanceMap)
    //println(tms.status)


  }

  val bitEncodingExample2GroundRules20PercentAcceptance  = Seq[String](
    "bit(4,1) :- level(4), not bit(4,0)",
    "bit(1,1) :- level(1), not bit(1,0)",
    "bit(2,1) :- level(2), not bit(2,0)",
    "bit(3,1) :- level(3), not bit(3,0)",
    "bit(0,1) :- level(0), not bit(0,0)",
    "bit(1,0) :- level(1), not bit(1,1)",
    "bit(0,0) :- level(0), not bit(0,1)",
    "bit(3,0) :- level(3), not bit(3,1)",
    "bit(2,0) :- level(2), not bit(2,1)",
    "bit(4,0) :- level(4), not bit(4,1)",
    "sum_at(0,0) :- bit(0,0)",
    "sum_at(0,1) :- bit(0,1)",
    "sum_at(3,23) :- int(23), int(8), sum_at(2,15), bit(3,1)",
    "sum_at(4,19) :- int(19), bit(4,1), sum_at(3,3), int(16)",
    "sum_at(1,7) :- bit(1,1), sum_at(0,5), int(7), int(2)",
    "sum_at(1,27) :- bit(1,1), sum_at(0,25), int(27), int(2)",
    "sum_at(2,20) :- int(4), bit(2,1), int(20), sum_at(1,16)",
    "sum_at(2,12) :- int(4), bit(2,1), sum_at(1,8), int(12)",
    "sum_at(3,21) :- int(8), sum_at(2,13), int(21), bit(3,1)",
    "sum_at(3,32) :- sum_at(2,24), int(8), int(32), bit(3,1)",
    "sum_at(4,17) :- int(17), sum_at(3,1), bit(4,1), int(16)",
    "sum_at(3,11) :- int(8), int(11), sum_at(2,3), bit(3,1)",
    "sum_at(1,5) :- bit(1,1), sum_at(0,3), int(5), int(2)",
    "sum_at(1,2) :- bit(1,1), sum_at(0,0), int(2)",
    "sum_at(2,32) :- sum_at(1,28), int(4), bit(2,1), int(32)",
    "sum_at(1,3) :- bit(1,1), sum_at(0,1), int(3), int(2)",
    "sum_at(1,26) :- bit(1,1), int(26), sum_at(0,24), int(2)",
    "sum_at(2,31) :- int(4), bit(2,1), int(31), sum_at(1,27)",
    "sum_at(1,21) :- bit(1,1), sum_at(0,19), int(21), int(2)",
    "sum_at(1,20) :- bit(1,1), int(20), sum_at(0,18), int(2)",
    "sum_at(2,25) :- int(4), bit(2,1), int(25), sum_at(1,21)",
    "sum_at(3,13) :- int(8), int(13), bit(3,1), sum_at(2,5)",
    "sum_at(1,4) :- int(4), bit(1,1), sum_at(0,2), int(2)",
    "sum_at(4,32) :- bit(4,1), int(32), int(16), sum_at(3,16)",
    "sum_at(1,18) :- bit(1,1), int(18), sum_at(0,16), int(2)",
    "sum_at(2,18) :- int(4), int(18), bit(2,1), sum_at(1,14)",
    "sum_at(4,31) :- int(31), sum_at(3,15), bit(4,1), int(16)",
    "sum_at(1,10) :- bit(1,1), sum_at(0,8), int(10), int(2)",
    "sum_at(2,6) :- int(4), bit(2,1), int(6), sum_at(1,2)",
    "sum_at(3,10) :- sum_at(2,2), int(10), int(8), bit(3,1)",
    "sum_at(3,17) :- sum_at(2,9), int(17), int(8), bit(3,1)",
    "sum_at(4,23) :- int(23), bit(4,1), sum_at(3,7), int(16)",
    "sum_at(1,31) :- bit(1,1), int(31), sum_at(0,29), int(2)",
    "sum_at(1,11) :- bit(1,1), sum_at(0,9), int(11), int(2)",
    "sum_at(1,9) :- bit(1,1), int(9), sum_at(0,7), int(2)",
    "sum_at(4,24) :- bit(4,1), int(24), int(16), sum_at(3,8)",
    "sum_at(2,23) :- int(4), bit(2,1), int(23), sum_at(1,19)",
    "sum_at(1,32) :- bit(1,1), int(32), sum_at(0,30), int(2)",
    "sum_at(2,10) :- int(4), bit(2,1), int(10), sum_at(1,6)",
    "sum_at(3,15) :- sum_at(2,7), int(8), int(15), bit(3,1)",
    "sum_at(2,27) :- int(4), bit(2,1), int(27), sum_at(1,23)",
    "sum_at(1,12) :- bit(1,1), sum_at(0,10), int(12), int(2)",
    "sum_at(4,21) :- bit(4,1), sum_at(3,5), int(21), int(16)",
    "sum_at(1,28) :- bit(1,1), int(28), sum_at(0,26), int(2)",
    "sum_at(4,25) :- sum_at(3,9), int(25), bit(4,1), int(16)",
    "sum_at(1,17) :- bit(1,1), int(17), int(2), sum_at(0,15)",
    "sum_at(1,19) :- int(19), bit(1,1), sum_at(0,17), int(2)",
    "sum_at(3,25) :- int(25), int(8), bit(3,1), sum_at(2,17)",
    "sum_at(3,19) :- int(19), int(8), bit(3,1), sum_at(2,11)",
    "sum_at(1,23) :- bit(1,1), int(23), sum_at(0,21), int(2)",
    "sum_at(3,9) :- int(8), int(9), sum_at(2,1), bit(3,1)",
    "sum_at(3,27) :- int(8), int(27), sum_at(2,19), bit(3,1)",
    "sum_at(2,9) :- int(4), bit(2,1), sum_at(1,5), int(9)",
    "sum_at(1,29) :- int(29), sum_at(0,27), bit(1,1), int(2)",
    "sum_at(2,22) :- int(4), bit(2,1), sum_at(1,18), int(22)",
    "sum_at(2,19) :- int(19), int(4), sum_at(1,15), bit(2,1)",
    "sum_at(3,29) :- int(29), sum_at(2,21), int(8), bit(3,1)",
    "sum_at(1,6) :- bit(1,1), sum_at(0,4), int(6), int(2)",
    "sum_at(4,30) :- bit(4,1), int(30), int(16), sum_at(3,14)",
    "sum_at(3,8) :- int(8), sum_at(2,0), bit(3,1)",
    "sum_at(1,15) :- bit(1,1), int(15), sum_at(0,13), int(2)",
    "sum_at(4,29) :- int(29), bit(4,1), sum_at(3,13), int(16)",
    "sum_at(2,15) :- int(4), bit(2,1), int(15), sum_at(1,11)",
    "sum_at(3,16) :- int(8), sum_at(2,8), int(16), bit(3,1)",
    "sum_at(2,7) :- int(4), bit(2,1), sum_at(1,3), int(7)",
    "sum_at(2,14) :- int(4), bit(2,1), sum_at(1,10), int(14)",
    "sum_at(1,16) :- bit(1,1), sum_at(0,14), int(16), int(2)",
    "sum_at(1,25) :- sum_at(0,23), bit(1,1), int(25), int(2)",
    "sum_at(2,29) :- int(29), int(4), bit(2,1), sum_at(1,25)",
    "sum_at(2,21) :- int(4), bit(2,1), sum_at(1,17), int(21)",
    "sum_at(3,12) :- int(8), sum_at(2,4), bit(3,1), int(12)",
    "sum_at(4,28) :- int(28), sum_at(3,12), bit(4,1), int(16)",
    "sum_at(4,22) :- bit(4,1), int(16), sum_at(3,6), int(22)",
    "sum_at(3,24) :- sum_at(2,16), int(8), int(24), bit(3,1)",
    "sum_at(1,24) :- bit(1,1), sum_at(0,22), int(24), int(2)",
    "sum_at(2,30) :- int(4), bit(2,1), sum_at(1,26), int(30)",
    "sum_at(2,5) :- int(4), bit(2,1), int(5), sum_at(1,1)",
    "sum_at(1,14) :- sum_at(0,12), bit(1,1), int(14), int(2)",
    "sum_at(4,20) :- sum_at(3,4), int(20), bit(4,1), int(16)",
    "sum_at(3,14) :- sum_at(2,6), int(8), int(14), bit(3,1)",
    "sum_at(2,26) :- int(4), bit(2,1), int(26), sum_at(1,22)",
    "sum_at(3,30) :- int(8), int(30), sum_at(2,22), bit(3,1)",
    "sum_at(2,11) :- sum_at(1,7), int(4), bit(2,1), int(11)",
    "sum_at(4,18) :- int(18), sum_at(3,2), bit(4,1), int(16)",
    "sum_at(1,30) :- bit(1,1), int(30), sum_at(0,28), int(2)",
    "sum_at(2,16) :- int(4), bit(2,1), sum_at(1,12), int(16)",
    "sum_at(3,31) :- sum_at(2,23), int(31), int(8), bit(3,1)",
    "sum_at(4,27) :- bit(4,1), int(27), sum_at(3,11), int(16)",
    "sum_at(2,13) :- int(4), bit(2,1), sum_at(1,9), int(13)",
    "sum_at(3,18) :- int(18), int(8), bit(3,1), sum_at(2,10)",
    "sum_at(3,28) :- sum_at(2,20), int(28), int(8), bit(3,1)",
    "sum_at(4,26) :- bit(4,1), int(26), int(16), sum_at(3,10)",
    "sum_at(2,17) :- int(4), bit(2,1), int(17), sum_at(1,13)",
    "sum_at(3,26) :- int(8), int(26), sum_at(2,18), bit(3,1)",
    "sum_at(4,16) :- bit(4,1), sum_at(3,0), int(16)",
    "sum_at(2,24) :- int(4), bit(2,1), int(24), sum_at(1,20)",
    "sum_at(2,8) :- int(4), bit(2,1), int(8), sum_at(1,4)",
    "sum_at(1,8) :- bit(1,1), int(8), sum_at(0,6), int(2)",
    "sum_at(2,28) :- int(4), bit(2,1), int(28), sum_at(1,24)",
    "sum_at(3,20) :- int(20), int(8), sum_at(2,12), bit(3,1)",
    "sum_at(3,22) :- int(8), sum_at(2,14), int(22), bit(3,1)",
    "sum_at(2,4) :- int(4), bit(2,1), sum_at(1,0)",
    "sum_at(1,13) :- bit(1,1), int(13), sum_at(0,11), int(2)",
    "sum_at(1,22) :- bit(1,1), sum_at(0,20), int(22), int(2)",
    "sum_at(3,0) :- sum_at(2,0), bit(3,0), int(0)",
    "sum_at(2,8) :- sum_at(1,8), bit(2,0), int(8)",
    "sum_at(2,4) :- sum_at(1,4), bit(2,0), int(4)",
    "sum_at(3,14) :- sum_at(2,14), bit(3,0), int(14)",
    "sum_at(3,11) :- sum_at(2,11), bit(3,0), int(11)",
    "sum_at(3,22) :- sum_at(2,22), bit(3,0), int(22)",
    "sum_at(4,23) :- sum_at(3,23), bit(4,0), int(23)",
    "sum_at(4,8) :- sum_at(3,8), bit(4,0), int(8)",
    "sum_at(3,2) :- sum_at(2,2), bit(3,0), int(2)",
    "sum_at(3,8) :- sum_at(2,8), bit(3,0), int(8)",
    "sum_at(4,27) :- sum_at(3,27), bit(4,0), int(27)",
    "sum_at(2,2) :- sum_at(1,2), bit(2,0), int(2)",
    "sum_at(4,2) :- sum_at(3,2), bit(4,0), int(2)",
    "sum_at(1,9) :- sum_at(0,9), bit(1,0), int(9)",
    "sum_at(3,13) :- sum_at(2,13), bit(3,0), int(13)",
    "sum_at(3,24) :- sum_at(2,24), bit(3,0), int(24)",
    "sum_at(4,28) :- sum_at(3,28), bit(4,0), int(28)",
    "sum_at(2,17) :- sum_at(1,17), bit(2,0), int(17)",
    "sum_at(1,15) :- sum_at(0,15), bit(1,0), int(15)",
    "sum_at(1,30) :- sum_at(0,30), bit(1,0), int(30)",
    "sum_at(1,3) :- sum_at(0,3), bit(1,0), int(3)",
    "sum_at(4,20) :- sum_at(3,20), bit(4,0), int(20)",
    "sum_at(3,5) :- sum_at(2,5), bit(3,0), int(5)",
    "sum_at(4,17) :- sum_at(3,17), bit(4,0), int(17)",
    "sum_at(4,0) :- sum_at(3,0), bit(4,0), int(0)",
    "sum_at(4,15) :- sum_at(3,15), bit(4,0), int(15)",
    "sum_at(1,32) :- sum_at(0,32), bit(1,0), int(32)",
    "sum_at(1,25) :- sum_at(0,25), bit(1,0), int(25)",
    "sum_at(3,17) :- sum_at(2,17), bit(3,0), int(17)",
    "sum_at(1,26) :- sum_at(0,26), bit(1,0), int(26)",
    "sum_at(2,32) :- sum_at(1,32), bit(2,0), int(32)",
    "sum_at(2,30) :- sum_at(1,30), bit(2,0), int(30)",
    "sum_at(3,26) :- sum_at(2,26), bit(3,0), int(26)",
    "sum_at(4,32) :- sum_at(3,32), bit(4,0), int(32)",
    "sum_at(3,31) :- sum_at(2,31), bit(3,0), int(31)",
    "sum_at(2,9) :- sum_at(1,9), bit(2,0), int(9)",
    "sum_at(2,7) :- sum_at(1,7), bit(2,0), int(7)",
    "sum_at(4,11) :- sum_at(3,11), bit(4,0), int(11)",
    "sum_at(1,19) :- sum_at(0,19), bit(1,0), int(19)",
    "sum_at(2,25) :- sum_at(1,25), bit(2,0), int(25)",
    "sum_at(1,1) :- sum_at(0,1), bit(1,0), int(1)",
    "sum_at(2,22) :- sum_at(1,22), bit(2,0), int(22)",
    "sum_at(1,4) :- sum_at(0,4), bit(1,0), int(4)",
    "sum_at(3,30) :- sum_at(2,30), bit(3,0), int(30)",
    "sum_at(1,2) :- sum_at(0,2), bit(1,0), int(2)",
    "sum_at(2,11) :- sum_at(1,11), bit(2,0), int(11)",
    "sum_at(4,5) :- sum_at(3,5), bit(4,0), int(5)",
    "sum_at(4,6) :- sum_at(3,6), bit(4,0), int(6)",
    "sum_at(3,27) :- sum_at(2,27), bit(3,0), int(27)",
    "sum_at(1,27) :- sum_at(0,27), bit(1,0), int(27)",
    "sum_at(2,18) :- sum_at(1,18), bit(2,0), int(18)",
    "sum_at(4,12) :- sum_at(3,12), bit(4,0), int(12)",
    "sum_at(1,14) :- sum_at(0,14), bit(1,0), int(14)",
    "sum_at(3,32) :- sum_at(2,32), bit(3,0), int(32)",
    "sum_at(1,0) :- sum_at(0,0), bit(1,0), int(0)",
    "sum_at(4,30) :- sum_at(3,30), bit(4,0), int(30)",
    "sum_at(3,10) :- sum_at(2,10), bit(3,0), int(10)",
    "sum_at(4,10) :- sum_at(3,10), bit(4,0), int(10)",
    "sum_at(4,31) :- sum_at(3,31), bit(4,0), int(31)",
    "sum_at(1,5) :- sum_at(0,5), bit(1,0), int(5)",
    "sum_at(3,7) :- sum_at(2,7), bit(3,0), int(7)",
    "sum_at(2,29) :- sum_at(1,29), bit(2,0), int(29)",
    "sum_at(3,9) :- sum_at(2,9), bit(3,0), int(9)",
    "sum_at(2,21) :- sum_at(1,21), bit(2,0), int(21)",
    "sum_at(1,18) :- sum_at(0,18), bit(1,0), int(18)",
    "sum_at(3,3) :- sum_at(2,3), bit(3,0), int(3)",
    "sum_at(1,23) :- sum_at(0,23), bit(1,0), int(23)",
    "sum_at(1,22) :- sum_at(0,22), bit(1,0), int(22)",
    "sum_at(1,16) :- sum_at(0,16), bit(1,0), int(16)",
    "sum_at(2,28) :- sum_at(1,28), bit(2,0), int(28)",
    "sum_at(4,7) :- sum_at(3,7), bit(4,0), int(7)",
    "sum_at(1,31) :- sum_at(0,31), bit(1,0), int(31)",
    "sum_at(3,6) :- sum_at(2,6), bit(3,0), int(6)",
    "sum_at(1,7) :- sum_at(0,7), bit(1,0), int(7)",
    "sum_at(3,21) :- sum_at(2,21), bit(3,0), int(21)",
    "sum_at(4,9) :- sum_at(3,9), bit(4,0), int(9)",
    "sum_at(3,28) :- sum_at(2,28), bit(3,0), int(28)",
    "sum_at(4,25) :- sum_at(3,25), bit(4,0), int(25)",
    "sum_at(4,4) :- sum_at(3,4), bit(4,0), int(4)",
    "sum_at(3,12) :- sum_at(2,12), bit(3,0), int(12)",
    "sum_at(4,19) :- sum_at(3,19), bit(4,0), int(19)",
    "sum_at(3,15) :- sum_at(2,15), bit(3,0), int(15)",
    "sum_at(3,29) :- sum_at(2,29), bit(3,0), int(29)",
    "sum_at(2,20) :- sum_at(1,20), bit(2,0), int(20)",
    "sum_at(2,3) :- sum_at(1,3), bit(2,0), int(3)",
    "sum_at(1,8) :- sum_at(0,8), bit(1,0), int(8)",
    "sum_at(2,12) :- sum_at(1,12), bit(2,0), int(12)",
    "sum_at(4,21) :- sum_at(3,21), bit(4,0), int(21)",
    "sum_at(4,1) :- sum_at(3,1), bit(4,0), int(1)",
    "sum_at(1,6) :- sum_at(0,6), bit(1,0), int(6)",
    "sum_at(2,10) :- sum_at(1,10), bit(2,0), int(10)",
    "sum_at(4,22) :- sum_at(3,22), bit(4,0), int(22)",
    "sum_at(4,16) :- sum_at(3,16), bit(4,0), int(16)",
    "sum_at(2,6) :- sum_at(1,6), bit(2,0), int(6)",
    "sum_at(4,3) :- sum_at(3,3), bit(4,0), int(3)",
    "sum_at(2,23) :- sum_at(1,23), bit(2,0), int(23)",
    "sum_at(4,14) :- sum_at(3,14), bit(4,0), int(14)",
    "sum_at(2,1) :- sum_at(1,1), bit(2,0), int(1)",
    "sum_at(2,5) :- sum_at(1,5), bit(2,0), int(5)",
    "sum_at(2,13) :- sum_at(1,13), bit(2,0), int(13)",
    "sum_at(1,17) :- sum_at(0,17), bit(1,0), int(17)",
    "sum_at(1,13) :- sum_at(0,13), bit(1,0), int(13)",
    "sum_at(2,27) :- sum_at(1,27), bit(2,0), int(27)",
    "sum_at(3,20) :- sum_at(2,20), bit(3,0), int(20)",
    "sum_at(4,24) :- sum_at(3,24), bit(4,0), int(24)",
    "sum_at(2,31) :- sum_at(1,31), bit(2,0), int(31)",
    "sum_at(4,13) :- sum_at(3,13), bit(4,0), int(13)",
    "sum_at(2,14) :- sum_at(1,14), bit(2,0), int(14)",
    "sum_at(2,16) :- sum_at(1,16), bit(2,0), int(16)",
    "sum_at(1,12) :- sum_at(0,12), bit(1,0), int(12)",
    "sum_at(3,1) :- sum_at(2,1), bit(3,0), int(1)",
    "sum_at(1,24) :- sum_at(0,24), bit(1,0), int(24)",
    "sum_at(1,29) :- sum_at(0,29), bit(1,0), int(29)",
    "sum_at(1,21) :- sum_at(0,21), bit(1,0), int(21)",
    "sum_at(4,29) :- sum_at(3,29), bit(4,0), int(29)",
    "sum_at(2,24) :- sum_at(1,24), bit(2,0), int(24)",
    "sum_at(1,20) :- sum_at(0,20), bit(1,0), int(20)",
    "sum_at(1,10) :- sum_at(0,10), bit(1,0), int(10)",
    "sum_at(2,0) :- sum_at(1,0), bit(2,0), int(0)",
    "sum_at(3,19) :- sum_at(2,19), bit(3,0), int(19)",
    "sum_at(2,19) :- sum_at(1,19), bit(2,0), int(19)",
    "sum_at(4,26) :- sum_at(3,26), bit(4,0), int(26)",
    "sum_at(2,26) :- sum_at(1,26), bit(2,0), int(26)",
    "sum_at(1,11) :- sum_at(0,11), bit(1,0), int(11)",
    "sum_at(3,25) :- sum_at(2,25), bit(3,0), int(25)",
    "sum_at(2,15) :- sum_at(1,15), bit(2,0), int(15)",
    "sum_at(4,18) :- sum_at(3,18), bit(4,0), int(18)",
    "sum_at(1,28) :- sum_at(0,28), bit(1,0), int(28)",
    "sum_at(3,16) :- sum_at(2,16), bit(3,0), int(16)",
    "sum_at(3,23) :- sum_at(2,23), bit(3,0), int(23)",
    "sum_at(3,18) :- sum_at(2,18), bit(3,0), int(18)",
    "sum_at(3,4) :- sum_at(2,4), bit(3,0), int(4)",
    "id(24) :- max_level(4), sum_at(4,24)",
    "id(30) :- max_level(4), sum_at(4,30)",
    "id(2) :- max_level(4), sum_at(4,2)",
    "id(26) :- max_level(4), sum_at(4,26)",
    "id(7) :- max_level(4), sum_at(4,7)",
    "id(6) :- max_level(4), sum_at(4,6)",
    "id(15) :- max_level(4), sum_at(4,15)",
    "id(17) :- max_level(4), sum_at(4,17)",
    "id(23) :- max_level(4), sum_at(4,23)",
    "id(18) :- max_level(4), sum_at(4,18)",
    "id(29) :- max_level(4), sum_at(4,29)",
    "id(10) :- max_level(4), sum_at(4,10)",
    "id(22) :- max_level(4), sum_at(4,22)",
    "id(25) :- max_level(4), sum_at(4,25)",
    "id(5) :- max_level(4), sum_at(4,5)",
    "id(31) :- max_level(4), sum_at(4,31)",
    "id(13) :- max_level(4), sum_at(4,13)",
    "id(16) :- max_level(4), sum_at(4,16)",
    "id(4) :- max_level(4), sum_at(4,4)",
    "id(3) :- max_level(4), sum_at(4,3)",
    "id(12) :- max_level(4), sum_at(4,12)",
    "id(28) :- max_level(4), sum_at(4,28)",
    "id(14) :- max_level(4), sum_at(4,14)",
    "id(1) :- max_level(4), sum_at(4,1)",
    "id(27) :- max_level(4), sum_at(4,27)",
    "id(21) :- max_level(4), sum_at(4,21)",
    "id(9) :- max_level(4), sum_at(4,9)",
    "id(8) :- max_level(4), sum_at(4,8)",
    "id(32) :- max_level(4), sum_at(4,32)",
    "id(19) :- max_level(4), sum_at(4,19)",
    "id(0) :- max_level(4), sum_at(4,0)",
    "id(20) :- max_level(4), sum_at(4,20)",
    "id(11) :- max_level(4), sum_at(4,11)",
    "xx1 :- id(19), int(9), not xx1",
    "xx1 :- id(29), int(9), not xx1",
    "xx1 :- id(28), int(8), not xx1",
    "xx1 :- id(9), int(9), not xx1",
    "xx1 :- id(18), int(8), not xx1",
    "xx1 :- id(8), int(8), not xx1",
    "level(0)",
    "level(1)",
    "level(2)",
    "level(3)",
    "level(4)",
    "int(0)",
    "int(1)",
    "int(2)",
    "int(3)",
    "int(4)",
    "int(5)",
    "int(6)",
    "int(7)",
    "int(8)",
    "int(9)",
    "int(10)",
    "int(11)",
    "int(12)",
    "int(13)",
    "int(14)",
    "int(15)",
    "int(16)",
    "int(17)",
    "int(18)",
    "int(19)",
    "int(20)",
    "int(21)",
    "int(22)",
    "int(23)",
    "int(24)",
    "int(25)",
    "int(26)",
    "int(27)",
    "int(28)",
    "int(29)",
    "int(30)",
    "int(31)",
    "int(32)",
    "max_level(4)"
  )



  //
  //
  //

  //    val output = "x :- leq(1,1), assign(m1,t3,1), lt(1,5), sum(1,4,5), duration(t3,4), timepoint(5), neq(t3,t1), assign(m1,t1,1), not x"
  //
  //    def parseFirstOf(pred: String): String = {
  //      val idx0 = output.indexOf(pred)
  //      val idx1 = output.indexOf("(",idx0) + 1
  //      val idx2 = output.indexOf(",",idx1)
  //      output.substring(idx1,idx2)
  //    }
  //
  //    def parseSecondOf(pred: String): String = {
  //      val idx0 = output.indexOf(pred)
  //      val idx1 = output.indexOf("(",idx0)+1
  //      val idx2 = output.indexOf(",",idx1)+1
  //      var idx3 = output.indexOf(")",idx2)
  //      if (idx3 == -1) idx3 = output.indexOf(",",idx2)
  //      output.substring(idx2,idx3)
  //    }
  //
  //    def i(s: String):Int =  Integer.parseInt(s)
  //
  //    val m = parseFirstOf("assign")
  //    val t1 = parseFirstOf("neq")
  //    val t2 = parseSecondOf("neq")
  //    val d = i(parseSecondOf("duration"))
  //    val p1 = i(parseFirstOf("leq"))
  //    val p2 = i(parseSecondOf("leq"))
  //    val z = i(parseFirstOf("timepoint"))

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
