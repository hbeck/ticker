package core.lars

import core._
import org.scalatest.FunSuite

/**
  * Created by hb on 8/23/16.
  */
class GrounderTests extends FunSuite {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def atom(s:String): Atom = {
    if (!s.contains("(")) return PredicateAtom(p(s))
    val commasOnly = s.substring(0,s.size-1).replace("(",",")
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
  def strVal(ss:String*): Set[Value] = ss map (StringValue(_)) toSet
  def v(s:String) = Variable(s)
  def arg(s:String): Argument = if (s.charAt(0).isUpper) Variable(s) else StringValue(s)

  def fact(s:String):LarsRule = LarsFact(atom(s))
  def rule(head:String,pos:String):LarsRule = LarsRule(atom(head),Set(atom(pos)),Set())
  def program(rules:LarsRule*):LarsProgram = LarsProgram(rules)
  def ground(p:LarsProgram) = Grounder(p).groundProgram

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
    val r2 = rule("b(V)","a(V)")
    val p = program(r1,r2)

    val gr1 = r1
    val gr2 = rule("b(x)","a(x)")
    val gp = program(gr1,gr2)
    val grounder = Grounder(p)

    //grounder.inspect.rules foreach println

    assert(grounder.groundProgram==gp)
  }

  test("gt4") {

    val r1 = fact("a(x)")
    val r2 = rule("b(V)","c(V)")
    val r3 = rule("c(V)","a(V)")
    val p = program(r1,r2,r3)

    val gr1 = r1
    val gr2 = rule("b(x)","c(x)")
    val gr3 = rule("c(x)","a(x)")
    val gp = program(gr1,gr2,gr3)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == Set(strVal("x")))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == Set(strVal("x")))

    assert(grounder.groundProgram==gp)
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
    val r3 = rule("b(V)","a(V)")
    val p = program(ax,ay,r3)

    val gax = ax
    val gay = ay
    val gr3x = rule("b(x)","a(x)")
    val gr3y = rule("b(y)","a(y)")
    val gp = program(gax,gay,gr3x,gr3y)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVal("x","y"))
    assert(grounder.groundProgram == gp)
  }

  test("gt6") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val r2 = rule("b(V)","c(V)")
    val r3 = rule("c(V)","a(V)")
    val p = program(ax,ay,r2,r3)

    val gax = ax
    val gay = ay
    val gr2x = rule("b(x)","c(x)")
    val gr2y = rule("b(y)","c(y)")
    val gr3x = rule("c(x)","a(x)")
    val gr3y = rule("c(y)","a(y)")
    val gp = program(gax,gay,gr2x,gr2y,gr3x,gr3y)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == strVal("x","y"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVal("x","y"))

    assert(grounder.groundProgram == gp)
  }

  test("gt7") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val az = fact("a(z)")
    val r3 = rule("c(V)","a(V)")
    val p = program(ax,ay,az,r3)

    val gax = ax
    val gay = ay
    val gaz = az
    val gr3x = rule("c(x)","a(x)")
    val gr3y = rule("c(y)","a(y)")
    val gr3z = rule("c(z)","a(z)")
    val gp = program(gax,gay,gaz,gr3x,gr3y,gr3z)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVal("x","y","z"))

    assert(grounder.groundProgram == gp)
  }

  test("gt8") {

    val ax = fact("a(x)")
    val ay = fact("a(y)")
    val az = fact("a(z)")
    val r2 = rule("b(V)","c(V)")
    val r3 = rule("c(V)","a(V)")
    val p = program(ax,ay,az,r2,r3)

    val gax = ax
    val gay = ay
    val gaz = az
    val gr2x = rule("b(x)","c(x)")
    val gr2y = rule("b(y)","c(y)")
    val gr2z = rule("b(z)","c(z)")
    val gr3x = rule("c(x)","a(x)")
    val gr3y = rule("c(y)","a(y)")
    val gr3z = rule("c(z)","a(z)")
    val gp = program(gax,gay,gaz,gr2x,gr2y,gr2z,gr3x,gr3y,gr3z)
    val grounder = Grounder(p)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == strVal("x","y","z"))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == strVal("x","y","z"))

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
  }

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
