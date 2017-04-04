package jtms.evaluation.instances

import core.asp.NormalRule
import core.{Atom, Predicate}
import jtms.JtmsUpdateAlgorithm
import jtms.evaluation.StreamingTmsEvalInstance

/**
  * Created by hb on 04.04.17.
  */
case class MMediaInstance(windowSize: Int, timePoints: Int) extends StreamingTmsEvalInstance {

  val done = Atom("done")
  val lfu = Atom("lfu")
  val lru = Atom("lru")
  val fifo = Atom("fifo")
  val random = Atom("random")

  val alpha_at = Predicate("alpha_at")
  val high_at = Predicate("high_at")
  val mid_at = Predicate("mid_at")
  val low_at = Predicate("low_at")
  val rtm_at = Predicate("rtm_at")
  val lt = Predicate("lt")
  val leq = Predicate("leq")

  val spoil_high = Atom("spoil_high")
  val spoil_mid = Atom("spoil_mid")
  val spoil_low = Atom("spoil_low")

  val wrtm = Atom("wrtm")

  /*
done :- lfu.
done :- lru.
done :- fifo.
random :- not done.
   */
  val E = Set[Atom]()

  override val staticRules: Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+ rule(done,lfu) :+ rule(done,lru) :+ rule(done,fifo) :+ rule(random,E,Set(done))
    for (i <- 0 to 30) {
      for (j <- 0 to 30) {
        if (i < j) {
          rules = rules :+ fact(f"lt($i,$j)")
        }
        if (i <= j) {
          rules = rules :+ fact(f"leq($i,$j)")
        }
      }
    }
    rules
  }


  //  val rulesToAdd: Map[Int,Seq[NormalRule]] = {
  //    (0 to timePoints) map (t => (t,rulesToAddAt(t))) toMap
  //  }
  //  val rulesToRemove: Map[Int,Seq[NormalRule]] = {
  //    (0 to timePoints) map (t => (t,rulesToRemoveAt(t))) toMap
  //  }

  override def factsToAddAt(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+ fact(alpha_at(alphaValueFor(t),t))
    rules
  }

  override def factsToRemoveAt(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+ fact(alpha_at(alphaValueFor(t-windowSize-1),t-windowSize-1))
    rules
  }

  def alphaValueFor(t: Int): Int = {
    val q = t%180
    var v = 0
    if (0 <= q && q < 60) {
      v = 5
    } else if (q >= 60 && q < 120) {
      v = 15
    } else {
      v = 25
    }
    v
  }

  override def verifyModel(tms: JtmsUpdateAlgorithm, t: Int) = {
    val model = tms.getModel().get
    val q = t % 180
    if (q >= 0 && q < windowSize) {
      assert(model.contains(random))
    } else if (q >= windowSize && q < 60) {
      assert(model.contains(random)) //low, if also rtm holds
    } else if (q >= 60 && q < 60 + windowSize) {
      assert(model.contains(random))
    } else if (q >= (60 + windowSize) && q < 120) {
      assert(model.contains(lru)) //mid
    } else if (q >= 120 && q < (120 + windowSize)) {
      assert(model.contains(random))
    } else if (q >= (120 + windowSize) && q < 180) {
      assert(model.contains(lfu)) //high
    }
  }

  def rulesToAddAt(t: Int) = immediatelyExpiringRulesFor(t) ++ rulesExpiringAfterWindow(t)
  def rulesToRemoveAt(t: Int) = immediatelyExpiringRulesFor(t-1) ++ rulesExpiringAfterWindow(t - windowSize - 1)

  def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    rules = rules :+
      rule(lfu,Set[Atom](high_at(t)),Set[Atom](spoil_high)) :+ //lfu :- high_at(N), not spoil_high.
      rule(lru,mid_at(t),spoil_mid) :+ //lru :- mid_at(N), not spoil_mid.
      rule(fifo, Set[Atom](low_at(t),wrtm), Set[Atom](spoil_low)) //fifo :- low_at(N), not spoil_low, wrtm.

    rules
  }

  def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = {
    var rules = Seq[NormalRule]()
    for (v <- 0 to 30) {
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



}
