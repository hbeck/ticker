package experimental

import common.Util.printTime
import core._
import core.asp._
import core.grounding.LarsGrounding
import core.lars.{LarsProgram, LarsRule}
import iclp.evaluation.Util._
import org.scalatest.FunSuite
import runner.Load
import runner.Load._
import jtms.algorithms.JtmsLearn
import jtms.networks.OptimizedNetworkForLearn

import scala.collection.immutable.HashMap

/**
  * Created by hb on 8/23/16.
  */
class GrounderTestsBeyondDoyle extends FunSuite {

  def ground(p: LarsProgram) = LarsGrounding(p).groundProgram
  def program(rules: LarsRule*): LarsProgram = LarsProgram(rules)

  val load = Load()
  import load._

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

    pending //JtmsLearn (contradictions)

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
    val grounder = LarsGrounding(inputProgram)


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

    pending //JtmsLearn (contradictions)

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
      val larsGrounding = printTime("grounding time") {
        LarsGrounding(inputProgram)
      }

      //larsGrounding.groundRules foreach (r => println(LarsProgram(Seq(r))))
      println(LarsProgram.from(larsGrounding.groundRules))

      //printInspect(larsGrounding)

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
            if (larsGrounding.inspect.possibleValuesForVariable(r, variable) != possibleValues) {
              println("rule: " + r)
              println("variable: " + variable.name)
              println("expected values: " + possibleValues)
              println("actual values:   " + larsGrounding.inspect.possibleValuesForVariable(r, variable))
              assert(false)
            }
          }
        }
      }

      println("#rules in ground program: " + larsGrounding.groundProgram.rules.size)

      // printInspect(larsGrounding)

      asAspProgram(larsGrounding.groundProgram)

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

    pending

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
      val larsGrounding = printTime("grounding time") {
        LarsGrounding(inputProgram)
      }

      //larsGrounding.groundRules foreach (r => println(LarsProgram(Seq(r))))
      println(LarsProgram.from(larsGrounding.groundRules))

      //printInspect(larsGrounding)

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
            if (larsGrounding.inspect.possibleValuesForVariable(r, variable) != possibleValues) {
              println("rule: " + r)
              println("variable: " + variable.name)
              println("expected values: " + possibleValues)
              println("actual values:   " + larsGrounding.inspect.possibleValuesForVariable(r, variable))
              assert(false)
            }
          }
        }
      }

      println("#rules in ground program: " + larsGrounding.groundProgram.rules.size)

      // printInspect(larsGrounding)

      asAspProgram(larsGrounding.groundProgram)

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

    pending //involves contradictions

    //object eval extends BitProgram
    //val program = printTime("grounding time"){ eval.groundLarsProgram() }
    //println("#rules: "+program.rules.size)

    val timePoints = 250
    //val tmsNames = Seq("greedy","learn")
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
        LarsGrounding(inputProgram)
      }
      println("#rules: " + grounding.groundRules.size)
      println(grounding.groundProgram)

      asAspProgram(grounding.groundProgram)

    }

    //

    for (windowSize <- windowSizes) {
      for (insertProbability <- insertProbabilities) {
        for (iteration <- 1 to iterationsEach) {
          val tms = new JtmsLearn(new OptimizedNetworkForLearn())

          println(f"\n${iteration}, windowSize: ${windowSize}, insertProbability: ${insertProbability}")

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
