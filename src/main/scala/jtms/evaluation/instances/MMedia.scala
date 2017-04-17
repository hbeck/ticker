package jtms.evaluation.instances

import core._
import core.asp.NormalRule
import core.lars._
import jtms.evaluation.{StreamingTmsEvalInst}

import scala.util.Random

/**
  * Created by hb on 04.04.17.
  */
abstract class MMedia(windowSize: Int, timePoints: Int, random: Random) extends StreamingTmsEvalInst {

  val done = Atom("done")
  val lfu = Atom("lfu")
  val lru = Atom("lru")
  val fifo = Atom("fifo")
  val randomAtom = Atom("random")

  val _alpha_at = Predicate("alpha_at")
  val _high_at = Predicate("high_at")
  val _mid_at = Predicate("mid_at")
  val _low_at = Predicate("low_at")
  val _rtm_at = Predicate("rtm_at")
  val _lt = Predicate("lt")
  val _leq = Predicate("leq")

  val spoil_high = Atom("spoil_high")
  val spoil_mid = Atom("spoil_mid")
  val spoil_low = Atom("spoil_low")

  val wrtm = Atom("wrtm")

  def alpha_at(arg1: Int, arg2: Int) = AtomWithArguments(_alpha_at,Seq(IntValue(arg1),IntValue(arg2)))
  def high_at(arg1: Int) = AtomWithArguments(_high_at,Seq(IntValue(arg1)))
  def mid_at(arg1: Int) = AtomWithArguments(_mid_at,Seq(IntValue(arg1)))
  def low_at(arg1: Int) = AtomWithArguments(_low_at,Seq(IntValue(arg1)))
  def rtm_at(arg1: Int) = AtomWithArguments(_rtm_at,Seq(IntValue(arg1)))
  def lt(arg1: Int, arg2: Int) = AtomWithArguments(_lt,Seq(IntValue(arg1),IntValue(arg2)))
  def leq(arg1: Int, arg2: Int) = AtomWithArguments(_leq,Seq(IntValue(arg1),IntValue(arg2)))

  // LARS:
  val _alpha = Predicate("alpha")
  val high = Atom("high")
  val mid = Atom("mid")
  val low = Atom("low")
  val rtm50 = Atom("rtm50")

  def alpha_at(arg1: Argument, arg2: Argument) = AtomWithArguments(_alpha_at,Seq(arg1,arg2))
  def high_at(arg1: Argument) = AtomWithArguments(_high_at,Seq(arg1))
  def mid_at(arg1: Argument) = AtomWithArguments(_mid_at,Seq(arg1))
  def low_at(arg1: Argument) = AtomWithArguments(_low_at,Seq(arg1))
  def rtm_at(arg1: Argument) = AtomWithArguments(_rtm_at,Seq(arg1))
//  def lt(arg1: Int, arg2: Argument) = AtomWithArguments(_lt,Seq(IntValue(arg1),arg2))
//  def lt(arg1: Argument, arg2: Int) = AtomWithArguments(_lt,Seq(arg1,IntValue(arg2)))
//  def lt(arg1: Argument, arg2: Argument) = AtomWithArguments(_lt,Seq(arg1,arg2))
//  def leq(arg1: Int, arg2: Argument) = AtomWithArguments(_leq,Seq(IntValue(arg1),arg2))
//  def leq(arg1: Argument, arg2: Int) = AtomWithArguments(_leq,Seq(arg1,IntValue(arg2)))
//  def leq(arg1: Argument, arg2: Argument) = AtomWithArguments(_leq,Seq(arg1,arg2))


  /*
done :- lfu.
done :- lru.
done :- fifo.
random :- not done.
   */
  val E = Set[Atom]()

  val maxAlphaValue = 30

  override val staticRules: Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+ rule(done,lfu) :+ rule(done,lru) :+ rule(done,fifo) :+ rule(randomAtom,E,Set(done))
    for (i <- 0 to maxAlphaValue) {
      for (j <- 0 to maxAlphaValue) {
        if (i < j) {
          rules = rules :+ fact(lt(i,j))
        }
        if (i <= j) {
          rules = rules :+ fact(leq(i,j))
        }
      }
    }
    rules
  }

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+
      rule(lfu,Set[Atom](high_at(t)),Set[Atom](spoil_high)) :+ //lfu :- high_at(N), not spoil_high.
      rule(lru,mid_at(t),spoil_mid) :+ //lru :- mid_at(N), not spoil_mid.
      rule(fifo, Set[Atom](low_at(t),wrtm), Set[Atom](spoil_low)) //fifo :- low_at(N), not spoil_low, wrtm.

    rules
  }

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    for (v <- 0 to maxAlphaValue) {
      rules = rules :+ rule(high_at(t),Set[Atom](alpha_at(v,t),leq(18,v)),E) //high_at(T) :- alpha_at(V,T), leq(18,V).
      rules = rules :+ rule(mid_at(t),Set[Atom](alpha_at(v,t),leq(12,v), lt(v,18)),E) //mid_at(T) :- alpha_at(V,T), leq(12,V), lt(V,18).
      rules = rules :+ rule(low_at(t),Set[Atom](alpha_at(v,t),lt(v,12)),E) //low_at(T) :- alpha_at(V,T), lt(V,12).

    }
    rules = rules :+
      rule(spoil_high,mid_at(t-1)) :+ //spoil_high :- mid_at(N-1).
      rule(spoil_high,low_at(t-1)) :+ //spoil_high :- low_at(N-1).
      rule(spoil_mid,high_at(t-1)) :+ //spoil_mid :- high_at(N-1).
      rule(spoil_mid,low_at(t-1)) :+ //spoil_mid :- low_at(N-1).
      rule(spoil_low,high_at(t-1)) :+ //spoil_low :- high_at(N-1).
      rule(spoil_low,mid_at(t-1)) :+ //spoil_low :- mid_at(N-1).
      rule(wrtm,rtm_at(t)) //wrtm :- rtm_at(N)

    rules
  }

  val program = larsProgram(30)

  def larsProgram(windowSize: Int): LarsProgram = {

    val T:Variable = StringVariable("T")
    val V:Variable = StringVariable("V")

    def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), At(time), atom)
    def wD(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Diamond, atom)
    def wB(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Box, atom)
    
    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    LarsProgram.from(
      AtAtom(T,high) <= wAt(n,T,_alpha(V)) and Leq(IntValue(18),V),
      AtAtom(T,mid) <= wAt(n,T,_alpha(V)) and Leq(IntValue(12),V) and Lt(V,IntValue(18)),
      AtAtom(T,low) <= wAt(n,T,_alpha(V)) and Lt(V,IntValue(12)),
      lfu <= wB(n,high),
      lru <= wB(n,mid),
      fifo <= wB(n,low) and wD(n,rtm50),
      done <= lfu,
      done <= lru,
      done <= fifo,
      UserDefinedLarsRule(randomAtom,s(),s(done))
    )

  }

}

case class MMediaDeterministicEvalInst(windowSize: Int, timePoints: Int, random: Random) extends MMedia(windowSize,timePoints,random) {

  override def verifyModel(optModel: Option[Model], t: Int) = {
    if (optModel.isEmpty) assert(false)
    val model = optModel.get
    val q = t % 180
    if (q >= 0 && q < windowSize) {
      assert(model.contains(randomAtom))
    } else if (q >= windowSize && q < 60) {
      assert(model.contains(randomAtom)) //low, if also rtm holds
    } else if (q >= 60 && q < 60 + windowSize) {
      assert(model.contains(randomAtom))
    } else if (q >= (60 + windowSize) && q < 120) {
      assert(model.contains(lru)) //mid
    } else if (q >= 120 && q < (120 + windowSize)) {
      assert(model.contains(randomAtom))
    } else if (q >= (120 + windowSize) && q < 180) {
      assert(model.contains(lfu)) //high
    }
  }

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    Seq[Atom]() :+ alpha_at(alphaValueFor(t),t)
  }

  def alphaValueFor(t: Int): Int = {
    val q = t%180
    if (0 <= q && q < 60) 5
    else if (q >= 60 && q < 120) 15
    else 25
  }

}

case class MMediaNonDeterministicEvalInst(windowSize: Int, timePoints: Int, random: Random) extends MMedia(windowSize,timePoints, random) {

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
      modeUntil = t+2*windowSize
    }
    val v = mode match {
      case High => 18 + random.nextInt(13)
      case Mid => 12 + random.nextInt(6)
      case _ => random.nextInt(12)
    }
    var factAtoms = Seq[Atom]() :+ alpha_at(v,t)
    if (mode == Low) {
      if ((t-lastRtm > windowSize) || (random.nextDouble() < (1.0/(1.0*windowSize/2.0)))) {
        factAtoms = factAtoms :+ rtm_at(t)
        lastRtm = t
      }
    }
    addedFacts = addedFacts + (t -> (factAtoms map (fact(_))))
    //println(f"$t -> $factAtoms")
    factAtoms
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {
    if (optModel.isEmpty) assert(false)
    val model = optModel.get
    if (t >= (modeUntil-windowSize)) {
      mode match {
        case High => assert(model.contains(lfu))
        case Mid => assert(model.contains(lru))
        case Low => assert(model.contains(fifo))
        case _ => assert(model.contains(randomAtom))
      }
    }
  }

}