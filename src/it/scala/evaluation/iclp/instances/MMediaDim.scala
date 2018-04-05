package evaluation.iclp.instances

import core._
import core.asp.NormalRule
import core.lars._
import evaluation.iclp.StreamingTmsEvalInst

import scala.util.Random

/**
  * Created by hb on 04.04.17.
  */
abstract class MMediaDim(windowSize: Int, timePoints: Int, maxDimensions: Int, random: Random) extends StreamingTmsEvalInst {

  val done = Predicate("done")
  val lfu = Predicate("lfu")
  val lru = Predicate("lru")
  val fifo = Predicate("fifo")
  val randomAtom = Predicate("random")

  val _alpha_at = Predicate("alpha_at")
  val _high_at = Predicate("high_at")
  val _mid_at = Predicate("mid_at")
  val _low_at = Predicate("low_at")
  val _rtm_at = Predicate("rtm50_at")
  val _value = Predicate("value")


  val spoil_high = Predicate("spoil_high")
  val spoil_mid = Predicate("spoil_mid")
  val spoil_low = Predicate("spoil_low")

  val dim = Predicate("dim")

  val wrtm = Predicate("wrtm")

  def alpha_at(arg1: Int, arg2: Int) = AtomWithArguments(_alpha_at, Seq(IntValue(arg1), IntValue(arg2)))

  def high_at(arg1: Int) = AtomWithArguments(_high_at, Seq(IntValue(arg1)))

  def mid_at(arg1: Int) = AtomWithArguments(_mid_at, Seq(IntValue(arg1)))

  def low_at(arg1: Int) = AtomWithArguments(_low_at, Seq(IntValue(arg1)))

  def rtm_at(arg1: Int) = AtomWithArguments(_rtm_at, Seq(IntValue(arg1)))

  def lt(arg1: Int, arg2: Int) = Lt(IntValue(arg1), IntValue(arg2))

  def leq(arg1: Int, arg2: Int) = Leq(IntValue(arg1), IntValue(arg2))

  def value(arg1: Int) = AtomWithArguments(_value, Seq(IntValue(arg1)))

  // LARS:
  val _alpha = Predicate("alpha")
  val high = Predicate("high")
  val mid = Predicate("mid")
  val low = Predicate("low")
  val rtm50 = Predicate("rtm50")

  def alpha(arg: Argument, dim: Argument) = AtomWithArguments(_alpha, Seq(arg, dim))

  def value(arg: Argument) = AtomWithArguments(_value, Seq(arg))


  /*
done :- lfu.
done :- lru.
done :- fifo.
random :- not done.
   */
  val E = Set[Atom]()

  val maxAlphaValue = 30
  val dimensions = (0 to maxDimensions) map (IntValue(_))

  override def staticRules(): Seq[NormalRule] = ???

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = ???

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = ???

  override def larsProgram(windowSize: Int): LarsProgram = {

    val T: Variable = StringVariable("T")
    val V: Variable = StringVariable("V")
    val X: Variable = StringVariable("X")

    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    val rules = Seq[LarsRule](
      AtAtom(T, high(X)) <= dim(X) and value(V) and wAt(n, T, _alpha(V, X)) and Leq(IntValue(18), V),
      AtAtom(T, mid(X)) <= dim(X) and value(V) and wAt(n, T, _alpha(V, X)) and Leq(IntValue(12), V) and Lt(V, IntValue(18)),
      AtAtom(T, low(X)) <= dim(X) and value(V) and wAt(n, T, _alpha(V, X)) and Lt(V, IntValue(12)),
      lfu(X) <= wB(n, high(X)),
      lru(X) <= wB(n, mid(X)),
      fifo(X) <= dim(X) and wB(n, low(X)) and wD(n, rtm50(X)),
      done(X) <= lfu(X),
      done(X) <= lru(X),
      done(X) <= fifo(X),
      UserDefinedLarsRule(randomAtom(X), s(dim(X)), s(done(X)))
    ) ++ (factAtoms map larsFact)

    LarsProgram(rules)

  }

  def factAtoms(): Seq[Atom] = {
    val valueAtoms = (0 to maxAlphaValue) map (value(_))
    val dimAtoms = dimensions map (dim(_))
    valueAtoms ++ dimAtoms
  }
}

case class MMediaDimDetInstance(windowSize: Int, timePoints: Int, maxDimensions: Int, random: Random) extends MMediaDim(windowSize, timePoints, maxDimensions, random) {

  override def verifyModel(optModel: Option[Model], t: Int) = {
    if (optModel.isEmpty) assert(false)
    val model = optModel.get

    def has(atom: Atom) = contains(model, t, atom)

    //def hasNot(atom: Atom) = notContains(model,t,atom)
    //def hasSomeOf(ats: Atom*) = containsSomeOf(model,t,ats.toSeq)
    val q = t % 180
    if (q >= 0 && q < windowSize) {
      dimensions forall (d => has(randomAtom(d)))
    } else if (q >= windowSize && q < 60) {
      dimensions forall (d => has(randomAtom(d))) //low, if also rtm holds
    } else if (q >= 60 && q < 60 + windowSize) {
      dimensions forall (d => has(randomAtom(d)))
    } else if (q >= (60 + windowSize) && q < 120) {
      dimensions forall (d => has(lru(d))) //mid
    } else if (q >= 120 && q < (120 + windowSize)) {
      dimensions forall (d => has(randomAtom(d)))
    } else if (q >= (120 + windowSize) && q < 180) {
      dimensions forall (d => has(lfu(d))) //high
    }
  }

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    dimensions map (d => alpha(alphaValueFor(t), d))
  }

  def alphaValueFor(t: Int): IntValue = {
    val q = t % 180
    if (0 <= q && q < 60) IntValue(5)
    else if (q >= 60 && q < 120) IntValue(15)
    else IntValue(25)
  }

}


case class MMediaDimNonDeterministicEvalInst(windowSize: Int, timePoints: Int, maxDimensions: Int, random: Random) extends MMediaDim(windowSize, timePoints, maxDimensions, random) {

  abstract class Mode

  case object High extends Mode

  case object Mid extends Mode

  case object Low extends Mode

  case object RandomMode extends Mode

  var mode: Mode = RandomMode
  var modeUntil = 1
  var lastRtm = -10000

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (modeUntil == t) {
      mode = random.nextInt(4) match {
        case 0 => High
        case 1 => Mid
        case 2 => Low
        case 3 => RandomMode
      }
      modeUntil = t + 2 * windowSize
    }
    val v = mode match {
      case High => 18 + random.nextInt(13)
      case Mid => 12 + random.nextInt(6)
      case _ => random.nextInt(12)
    }
    val skip = random.nextInt(maxDimensions)
    val take = random.nextInt(maxDimensions - skip)

    val usedDimensions = dimensions //.slice(skip, take)

    var signals: Seq[Atom] = usedDimensions map (d => alpha(IntValue(v), d))

    if (mode == Low) {
      if ((t - lastRtm > windowSize) || (random.nextDouble() < (1.0 / (1.0 * windowSize / 2.0)))) {
        signals = signals ++ (usedDimensions map (rtm50(_)))
        lastRtm = t
      }
    }
    //addedFacts = addedFacts + (t -> (signals map (fact(_))))
    //println(f"$t -> $signals")
    signals
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {
    if (optModel.isEmpty) assert(false)
    val model = optModel.get

    def has(atom: Atom) = contains(model, t, atom)

    if (t >= (modeUntil - windowSize)) {
      mode match {
        case High => dimensions forall (d => has(lfu(d)))
        case Mid => dimensions forall (d => has(lru(d)))
        case Low => dimensions forall (d => has(fifo(d)))
        case _ => dimensions forall (d => has(randomAtom(d)))
      }
    }
  }

}