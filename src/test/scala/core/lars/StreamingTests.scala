package core.lars

import common.Util.printTime
import core.lars.Util._
import core.{Atom, IntValue, Value, Variable}
import jtms.JtmsLearn
import org.scalatest.FunSuite


/**
  * Created by hb on 8/28/16.
  */
class StreamingTests extends FunSuite {

  test("bit 2 lars 1") {

    val useGrounding = true

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
        rule("xx1 :- id(C), mod(C,10,K), geq(K,8), int(K), not xx1"),
        rule("bit(L,1) :- level(L), w_d_20_signal(L)") //new rule
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
      val signalL:Atom = atom("signal(L)").asInstanceOf[Atom]
      //println(signalL)
      val wat = WindowAtom(SlidingTimeWindow(20),Diamond,signalL)
      //inputProgram.atoms filter (_.isInstanceOf[WindowAtom]) foreach println
      assert(inputProgram.atoms contains wat)

      //println(inputProgram)
      val grounder = printTime("grounding time") {
        Grounder(inputProgram)
      }

      //println(LarsProgram(grounder.groundRules))
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
    val filename = "/ground-programs/bit2.rules" //TODO
      readProgramFromFile(filename)
    } //end assignment of groundLarsProgram

    println("#rules in ground program: " + groundLarsProgram)

    val asp = aspProgram(groundLarsProgram)
    asp.rules foreach (r => if (r.toString.contains("w_")) println(r))

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
    println("size of avoidance current map: "+currentRulesTabu.avoidanceMap.size)
    //println(currentRulesTabu.avoidanceMap)
    //println(tms.status)


  }

}
