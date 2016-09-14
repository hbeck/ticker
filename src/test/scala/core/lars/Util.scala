package core.lars

import core._
import core.asp.{AspProgram, NormalProgram, NormalRule, UserDefinedAspRule}
import engine.asp.LarsToPinnedProgram
import engine.asp.tms.{Pin, PinnedAspToIncrementalAsp}

import scala.io.Source

/**
  * Created by hb on 8/28/16.
  */
object Util {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def xatom(s:String): ExtendedAtom = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("(")) return noArgsAtom(str)
    val commasOnly = str.substring(0,str.size-1).replace("(",",")
    val seq: Seq[String] = commasOnly.split(",").toSeq
    xatom(seq)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def xatom(ss:Seq[String]): ExtendedAtom = {
    assert(ss.size>0)
    if (ss.size == 1)
      noArgsAtom(ss(0))
    else {
      val s = ss(0)
      if (s.startsWith("w_")) { //convention for name of window atom: w_d_7_a(foo,X) says Time Window with size 7 Diamond a
      val parts = s.split("_")
        val temporalModality = parts(1) match {
          case "d" => Diamond
          case "b" => Box
          case x => throw new RuntimeException("window "+s+" atom cannot be parsed. unknown temporal modality "+x)
        }
        val size = Integer.parseInt(parts(2))
        val p = Predicate(parts(3))
        val args = ss.tail map arg
        WindowAtom(SlidingTimeWindow(size),temporalModality,Atom(p,args)) //doesn't matter which one
      } else if (s.startsWith("#")){
        val p = Predicate(s)
        val args = ss.tail take ss.tail.size-1 map arg
        val atom = Atom(p,args)
        val time = Integer.parseInt(ss.tail.last.toString)
        PinnedAtom(atom,time)
      } else {
        val p = Predicate(s)
        val args = ss.tail map arg
        Atom(p,args)
      }
    }
  }


  def noArgsAtom(s:String):GroundAtom = {
    val pred = Predicate(s)
    //if (s.startsWith("xx"))
    //  ContradictionAtom(pred)
    //else

    PredicateAtom(pred)
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

  def fact(s:String):LarsRule = LarsFact(xatom(s).asInstanceOf[Atom])
  def rule(all:String): LarsRule = {
    val s = all.replaceAll("\\.","")
    if (!s.contains(" :- ")) return fact(s)
    val hbStr = s.split(" :- ")
    val head:HeadAtom = xatom(hbStr(0)).asInstanceOf[Atom] //not *A*tom! using atom to parse predicate symbol
    val bodyParts = hbStr(1).split(", ")
    val posBodyParts = bodyParts filterNot (_.trim.startsWith("not"))
    val negBodyParts = bodyParts filter (_.trim.startsWith("not"))
    val pos:Set[ExtendedAtom] = posBodyParts map (xatom(_)) toSet
    val neg:Set[ExtendedAtom] = negBodyParts map (xatom(_)) toSet

    LarsRule(head,pos,neg)
  }

  def groundSignal(s:String):LarsRule = {
    val aa: AtomWithArgument = xatom(s).asInstanceOf[AtomWithArgument]
    val timeArg = Integer.parseInt(aa.arguments.last.toString)
    val otherArgs = aa.arguments.take(aa.arguments.size-1)
    LarsFact(PinnedAtom(AtomWithArgument(aa.predicate,otherArgs),timeArg))
  }

  def program(rules:LarsRule*):LarsProgram = LarsProgram(rules)
  def ground(p:LarsProgram) = Grounder(p).groundProgram

  //a(x,y) b(y,z) ==> use , only within atoms and use white space only to split atoms
  def parseSpaceSeparatedAtoms(s: String): Set[ExtendedAtom] = {
    s.split(" ") map (xatom(_)) toSet
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
    s.split(" ") map (xatom(_).asInstanceOf[Atom]) toSet
  }

  def asInt(v:Value):Int = v match {
    case StringValue(s) => Integer.parseInt(s)
    case IntValue(i) => i
    case _ => throw new RuntimeException("argument %s cannot be casted to int".format(v))
  }

  def inspectGrounder(grounder: Grounder): Unit = {
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

  def readProgramFromFile(filename: String): LarsProgram = {
    val source = Source.fromURL(getClass.getResource(filename))
    val rules = source.getLines().toSeq map rule
    LarsProgram(rules)
  }

  def aspProgramAt(groundLarsProgram: LarsProgram, time: Int): NormalProgram = {

    val aspProgramWithVariables = LarsToPinnedProgram(groundLarsProgram)

    val incrementalProgram = PinnedAspToIncrementalAsp(aspProgramWithVariables)

    val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

    val pin = Pin(time)
    val pinnedRules: Seq[NormalRule] = nonGroundRules map pin.ground

    AspProgram((groundRules ++ pinnedRules).toList)
  }


}
