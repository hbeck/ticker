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
  def strVal(s:String) = StringValue(s)
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
    //println(gp)
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

    printInspect(grounder)

    assert(grounder.inspect.possibleValuesForVariable(r2,v("V")) == Set(strVal("x")))
    assert(grounder.inspect.possibleValuesForVariable(r3,v("V")) == Set(strVal("x")))

    assert(grounder.groundProgram==gp)
  }

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
