package core.lars

import common.Util.printTime
import core._
import core.asp.{AspFact, AspRule, NormalRule}
import core.lars.Util._
import jtms.JtmsLearn
import org.scalatest.FunSuite


/**
  * Created by hb on 8/28/16.
  */
class StreamingTests extends FunSuite {

  test("streaming bits 1 manual mapping") {

    val useGrounding = true

    val highestExponent = 5 //2^X
    val maxLevel = highestExponent - 1

    val groundLarsProgram = if (useGrounding) {

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
        rule("xx1 :- id(C), mod(C,20,K), geq(K,1), int(K), not xx1")
        //rule("bit(L,1) :- level(L), w_d_20_signal(L)") //new rule
      ))

      //val highestExponent = 5 //2^X
      //val maxLevel = highestExponent - 1 moved above

      val levels:Seq[Int] = for (l <- 0 to maxLevel) yield l
      val ints:Seq[Int] = for (i <- 0 to Math.pow(2,highestExponent).toInt) yield i

      val facts: Seq[LarsRule] =
        (levels map (l => fact("level("+l+")"))) ++
          (ints map (i => fact("int("+i+")"))) :+
          fact("max_level("+maxLevel+")")

      val inputProgram = LarsProgram(bitEncodingProgram.rules ++ facts)

      /*
      val signalL:Atom = atom("signal(L)").asInstanceOf[Atom]
      //println(signalL)
      val wat = WindowAtom(SlidingTimeWindow(20),Diamond,signalL)
      //inputProgram.atoms filter (_.isInstanceOf[WindowAtom]) foreach println
      assert(inputProgram.atoms contains wat)
      */

      //println(inputProgram)
      val grounder = printTime("grounding time") {
        Grounder(inputProgram)
      }

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
      grounder.groundProgram

    } else { //no grounding, use predefined program
    val filename = "/ground-programs/bit2.rules"
      readProgramFromFile(filename)
    } //end assignment of groundLarsProgram

    val asp = asAspProgram(groundLarsProgram)

    //    println("contradiction atoms:")
    //    (asp.atoms filter (_.isInstanceOf[ContradictionAtom]) toSet) foreach println

    val tms = new JtmsLearn()
    tms.shuffle = false
    printTime("time to add all ground rules") {
      asp.rules foreach tms.add
    }

    // manual window stuff:

    //rule("bit(L,1) :- level(L), w_d_20_signal(L)")
    val staticRuleTemplate = "bit(L,1) :- level(L), wd20signal(L)"
    for (level <- 0 to maxLevel) {
      val ruleString = staticRuleTemplate.replaceAll("L",""+level)
      tms add asAspRule(rule(ruleString))
    }

    println("#rules in asp program: " + asp.rules.size)

    //
    //

    var failures = if (tms.getModel == None) 1 else 0

    def projected(model: Model) = model filter (a => Set("bit","id","signal") contains (a.predicate.toString))

    var factsWithinWindowSize = Map[Int,Set[NormalRule]]()

    var idCount = Map[Int,Int]() //id 2 nr of models which had it

    def makePinnedAtom(level:Int, timepoint: Int) = {
      val p = Predicate("signal")
      val a = GroundAtomWithArguments(p,Seq[Value](Value(""+level)))
      PinnedAtom(a, TimePoint(timepoint))
    }

    def makeSignalFact(level: Int, timepoint: Int): NormalRule = {
      AspFact(makePinnedAtom(level,timepoint))
    }

    def makeDynamicSignalRule(level:Int, timepoint: Int): NormalRule = {
      val levelVal = IntValue(level)
      val headPredicate = Predicate("wd20signal")
      val head = GroundAtomWithArguments(headPredicate,Seq[Value](levelVal))
      val signalPredicate = Predicate("signal")
      val signalGroundAtom = GroundAtomWithArguments(signalPredicate,Seq[Value](levelVal))
      val pa = PinnedAtom(signalGroundAtom, TimePoint(timepoint))
      AspRule(head,pa)
    }

    /*
    tms add makeSignalFact(0,1)
    tms add makeSignalFact(1,1)
    tms add makeSignalFact(2,1)
    tms add makeSignalFact(3,1)
    tms add makeSignalFact(4,1)
    */
    //=> 31; mod 10 == 1

    var failuresLastPart = 0

    val windowSize = 20
    val insertProbability = 0.05
    val lengthOfTimeline = 2000
    val startLastPart = 1800
    val reportEvery = 200

    var lastFailed = tms.getModel == None

    for (timepoint <- 1 to lengthOfTimeline) {

      //1 add streaming facts
      var addedNewFact = false
      //if (tms.getModel.isDefined && !addedNewFact) {
        if (tms.random.nextDouble() < insertProbability) {
          val level = tms.random.nextInt(maxLevel)
          //challenge with next signal
          val signal: NormalRule = makeSignalFact(level, timepoint)
          //if (insertProbability <= 0.05)
          // println("+"+signal)
          tms add signal
          addedNewFact = true
          val set = factsWithinWindowSize.getOrElse(timepoint, Set()) + signal
          factsWithinWindowSize = factsWithinWindowSize.updated(timepoint, set)
        }
      //}

      //2 add new rules
      for (level <- 0 to maxLevel) {
        val signalRule: NormalRule = makeDynamicSignalRule(level,timepoint)
        if (timepoint == 1) {
          println(signalRule)
        }
        tms add makeDynamicSignalRule(level,timepoint)
      }

      //3 remove old rules
      val deletionTimepoint = timepoint - windowSize - 1
      for (level <- 0 to maxLevel) {
        tms remove makeDynamicSignalRule(level,deletionTimepoint)
      }

      //5 remove old facts
      factsWithinWindowSize.getOrElse(deletionTimepoint,Set()) foreach { signal =>
        tms remove signal
      }
      if (factsWithinWindowSize.contains(deletionTimepoint)) {
        factsWithinWindowSize = factsWithinWindowSize - deletionTimepoint
      }

//      if (timepoint == 1) {
//        tms.rules foreach { r => val s = r.toString; if (s.contains("signal")) println(s) }
//      }

      if (lastFailed && tms.getModel.isDefined) {
        println(timepoint+" -> "+projected(tms.getModel.get))
      }

      if (!addedNewFact && tms.inconsistent()) tms.recompute()

      if (tms.getModel.isEmpty) {
        lastFailed = true
      } else {
        lastFailed = false
      }

      //

      tms.getModel match {
        case Some(model) => {
          if (idCount.isEmpty) { //print first model
            println("\t"+timepoint+" -> "+projected(model))
          }
          val id: Int = Integer.parseInt( projected(model).head.asInstanceOf[AtomWithArgument].arguments(0).toString )
          val count = idCount.getOrElse(id,0)
          idCount = idCount + (id -> (count + 1))
          //end2 = System.currentTimeMillis()
          //println(timepoint+" -> "+projected(model))
        }
        case None => {
          //println("fail")
          failures = failures + 1
          if (timepoint >= startLastPart) {
            failuresLastPart = failuresLastPart + 1
          }
          //println(timepoint+" -> \n"+factsWithinWindowSize)
          //println(timepoint+" -> ["+failures+"]")
        }
      }

      if (timepoint < 100 || timepoint % reportEvery == 0) {
        tms.getModel() match {
          case Some(m) => println("\t"+timepoint+" -> "+projected(m))
          case None => println("\t"+timepoint+" -> ---")
        }
      }
    }

    println("\nfacts within window size:")
    println(factsWithinWindowSize)

    println("\nid count:")
    println(idCount)

    println("failures: "+failures+" ("+(1.0*failures)/(1.0*lengthOfTimeline)+")")
    println("failures after iteration "+startLastPart+": "+failuresLastPart+" ("+(1.0*failuresLastPart)/(1.0*(lengthOfTimeline-startLastPart))+")")

    if (tms.isInstanceOf[JtmsLearn]) {
      val jtms = tms.asInstanceOf[JtmsLearn]
      val tabu = jtms.tabu
      val currentRulesTabu = tabu.currentRulesTabu
      println("size of avoidance current map: "+currentRulesTabu.avoidanceMap.size)
      jtms.printAvoidanceMap()
      //println(jtms.status)
    }

  }



}
