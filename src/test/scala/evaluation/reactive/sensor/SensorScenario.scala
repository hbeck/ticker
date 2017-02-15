package evaluation.reactive.sensor

import core._
import core.lars._

import scala.util.Random

/**
  * Created by fm on 14/02/2017.
  */
trait SensorScenario {

  val V: Variable = Variable("V")

  val U: TimeVariableWithOffset = TimeVariableWithOffset("U")

  val s = Atom("s")

  val low = Atom("low")
  val high = Atom("high")
  val med_1 = Atom("med_1")
  val med_2 = Atom("med_2")
  val med = Atom("med")

  val green: Atom = Atom("green")
  val yellow_1 = Atom("yellow_1")
  val yellow_2 = Atom("yellow_2")

  val y = Atom("y")

  val warn = Atom("warn")
  val chaos = Atom("chaos")

  val lowThreshold = IntValue(5)
  val highThreshold = IntValue(80)

  sealed trait MedRules

  object UseBothMeds extends MedRules

  object UseMed1 extends MedRules

  object UseMed2 extends MedRules

  sealed trait YellowRules

  object UseBothYellows extends YellowRules

  object UseYellow1 extends YellowRules

  object UseYellow2 extends YellowRules


  def buildProgram(n: TimeWindowSize)(window: (TemporalModality, Atom) => WindowAtom, useMed: MedRules = UseBothMeds, useYellow: YellowRules = UseBothYellows): LarsProgram = {

    def slidingTime(temp: TemporalModality, atom: Atom) = WindowAtom(SlidingTimeWindow(n), temp, atom)

    val at_low: LarsRule = AtAtom(U, low) <= window(At(U), s(V)) and Lt(V, lowThreshold)
    val at_high: LarsRule = AtAtom(U, high) <= window(At(U), s(V)) and Gt(V, highThreshold)

    val at_med_1: LarsRule = AtAtom(U, med_1) <= window(At(U), s(V)) and Geq(V, lowThreshold) and Leq(V, highThreshold)
    val at_med_2: LarsRule = AtAtom(U, med_2) <= window(At(U), s(V)) not AtAtom(U, low) not AtAtom(U, high)

    val at_med__1: LarsRule = AtAtom(U, med) <= slidingTime(At(U), med_1)
    val at_med__2: LarsRule = AtAtom(U, med) <= slidingTime(At(U), med_2)

    val greenRule: LarsRule = green <= slidingTime(Box, high)

    val at_y_1: LarsRule = AtAtom(U, y) <= slidingTime(At(U), high)
    val at_y_2: LarsRule = AtAtom(U, y) <= slidingTime(At(U), med)

    val yellowRule_1: LarsRule = yellow_1 <= slidingTime(Box, y)
    val yellowRule_2: LarsRule = yellow_2 <= not[ExtendedAtom](warn)

    val warnRule: LarsRule = warn <= window(Diamond, s(V)) and Lt(V, lowThreshold)
    val chaosRule: LarsRule = chaos <= slidingTime(Diamond, low) and slidingTime(Diamond, med) and slidingTime(Diamond, high)

    val baseRules = Seq(
      at_low,
      at_high,

      greenRule,

      warnRule,
      chaosRule
    )

    val medRules = useMed match {
      case UseBothMeds => Seq(
        at_med_1,
        at_med_2,
        at_med__1,
        at_med__2
      )
      case UseMed1 => Seq(
        at_med_1,
        at_med__1
      )
      case UseMed2 => Seq(
        at_med_2,
        at_med__2
      )
    }

    val yellowRules = useYellow match {
      case UseBothYellows => Seq(
        at_y_1,
        at_y_2,

        yellowRule_1,
        yellowRule_2
      )
      case UseYellow1 => Seq(
        at_y_1,
        at_y_2,

        yellowRule_1
      )
      case UseYellow2 => Seq(
        yellowRule_2
      )
    }

    LarsProgram(baseRules ++ medRules ++ yellowRules)
  }

  def timeWindowProgram(windowSize: TimeWindowSize, useMed: MedRules = UseBothMeds, useYellow: YellowRules = UseBothYellows): LarsProgram = buildProgram(windowSize)((temp, atom) => WindowAtom(SlidingTimeWindow(windowSize), temp, atom), useMed, useYellow)

  def tupleWindowProgram(windowSize: TupleCount, useMed: MedRules = UseBothMeds, useYellow: YellowRules = UseBothYellows): LarsProgram = buildProgram(TimeWindowSize(windowSize))((temp, atom) => WindowAtom(SlidingTupleWindow(windowSize), temp, atom), useMed, useYellow)


  def continuousSignalStream(random: Random)(length: Int): Seq[(TimePoint, Atom)] = (0 to length).
    map((_, IntValue(random.nextInt(100)))).
    map {
      case (point, value) => (TimePoint(point), s(value))
    }

  //    toMap


}
