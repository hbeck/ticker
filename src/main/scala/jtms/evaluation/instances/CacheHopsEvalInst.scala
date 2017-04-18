package jtms.evaluation.instances

import core._
import core.asp.{AspProgram, NormalRule}
import core.grounding.{GrounderInstance, StaticProgramInspection}
import core.lars._
import jtms.evaluation.{StreamingTmsEvalInst}

import scala.util.Random

/**
  * Created by hb on 10.04.17.
  */
abstract class CacheHopsEvalInst(random: Random) extends StreamingTmsEvalInst {

  val windowSize: Int
  val timePoints: Int
  val nrOfItems: Int

  def edges: Set[Atom]

  final def nodes(): Set[Value] = edges collect { case a:GroundAtomWithArguments => a.arguments } flatten
  final def nodeAtoms(): Set[Atom] = nodes map (node(_))

  def maxPathLength: Int = nodes.size //at most number of nodes

  val _node = Predicate("node")
  val _sat = Predicate("sat")
  val _hit = Predicate("hit")
  val _get = Predicate("get")
  val _fail = Predicate("fail")
  val _unsat = Predicate("unsat")
  val _item = Predicate("item")
  val _getFrom = Predicate("getFrom")
  val _n_getFrom = Predicate("n_getFrom")
  val _needAt = Predicate("needAt")
  val _conn = Predicate("conn")
  val _edge = Predicate("edge")
  val _minReach = Predicate("minReach")
  val _n_minReach = Predicate("n_minReach")
  val _itemReach = Predicate("itemReach")
  val _reach = Predicate("reach")
  val _length = Predicate("length")

  //windows
  val _w_req = Predicate("w_req")
  val _w_cache = Predicate("w_cache")
  val _w_error = Predicate("w_error")
  //signals
  val _req = Predicate("req")
  val _cache = Predicate("cache")
  val _error = Predicate("error")

  def iVal(i: Int) = IntValue(i)

  def node(arg1: Argument) = AtomWithArguments(_node,Seq(arg1))
  def sat(arg1: Argument, arg2: Argument) = AtomWithArguments(_sat,Seq(arg1,arg2))
  def sat(arg1: Argument, arg2: Int) = AtomWithArguments(_sat,Seq(arg1,iVal(arg2)))
  def hit(arg1: Argument, arg2: Argument) = AtomWithArguments(_sat,Seq(arg1,arg2))
  def hit(arg1: Argument, arg2: Int) = AtomWithArguments(_sat,Seq(arg1,iVal(arg2)))
  def get(arg1: Argument, arg2: Argument) = AtomWithArguments(_sat,Seq(arg1,arg2))
  def get(arg1: Argument, arg2: Int) = AtomWithArguments(_sat,Seq(arg1,iVal(arg2)))
  def fail(arg1: Argument, arg2: Argument) = AtomWithArguments(_sat,Seq(arg1,arg2))
  def fail(arg1: Argument, arg2: Int) = AtomWithArguments(_sat,Seq(arg1,iVal(arg2)))
  def unsat(arg1: Argument, arg2: Argument) = AtomWithArguments(_unsat,Seq(arg1,arg2))
  def unsat(arg1: Argument, arg2: Int) = AtomWithArguments(_unsat,Seq(arg1,iVal(arg2)))
  def getFrom(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_getFrom,Seq(arg1,arg2,arg3))
  def getFrom(arg1: Argument, arg2: Int, arg3: Int) = AtomWithArguments(_getFrom,Seq(arg1,iVal(arg2),iVal(arg3)))
  def item(arg1: Argument) = AtomWithArguments(_item,Seq(arg1))
  def n_getFrom(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_n_getFrom,Seq(arg1,arg2,arg3))
  def needAt(arg1: Argument, arg2: Argument) = AtomWithArguments(_needAt,Seq(arg1,arg2))
  def needAt(arg1: Argument, arg2: Int) = AtomWithArguments(_needAt,Seq(arg1,iVal(arg2)))
  def conn(arg1: Argument, arg2: Argument) = AtomWithArguments(_conn,Seq(arg1,arg2))
  def edge(arg1: Argument, arg2: Argument) = AtomWithArguments(_edge,Seq(arg1,arg2))
  def edge(arg1: Int, arg2: Int) = AtomWithArguments(_edge,Seq(iVal(arg1),iVal(arg2)))
  def minReach(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_minReach,Seq(arg1,arg2,arg3))
  def minReach(arg1: Argument, arg2: Int, arg3: Int) = AtomWithArguments(_minReach,Seq(arg1,iVal(arg2),iVal(arg3)))
  def n_minReach(arg1: Argument, arg2: Argument, arg3: Argument, arg4: Argument) = AtomWithArguments(_n_minReach,Seq(arg1,arg2,arg3,arg4))
  def itemReach(arg1: Argument, arg2: Argument, arg3: Argument, arg4: Argument) = AtomWithArguments(_itemReach,Seq(arg1,arg2,arg3,arg4))
  def itemReach(arg1: Argument, arg2: Int, arg3: Int, arg4: Int) = AtomWithArguments(_itemReach,Seq(arg1,iVal(arg2),iVal(arg3),iVal(arg4)))
  def reach(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_reach,Seq(arg1,arg2,arg3))
  def w_req(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_req,Seq(arg1,arg2))
  def w_cache(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_cache,Seq(arg1,arg2))
  def w_error(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_error,Seq(arg1,arg2))
  //def req(arg1: Argument, arg2: Argument, arg3: Argument):Atom = AtomWithArguments(_req,Seq(arg1,arg2,arg3))
  //def cache(arg1: Argument, arg2: Argument, arg3: Argument):Atom = AtomWithArguments(_cache,Seq(arg1,arg2,arg3))
  //def error(arg1: Argument, arg2: Argument, arg3: Argument):Atom = AtomWithArguments(_error,Seq(arg1,arg2,arg3))
  def length(arg1: Argument) = AtomWithArguments(_length,Seq(arg1))
  def length(arg1: Int) = AtomWithArguments(_length,Seq(iVal(arg1)))

  //LARS:
  def req(arg1: Argument, arg2: Argument):Atom = AtomWithArguments(_req,Seq(arg1,arg2))
  def cache(arg1: Argument, arg2: Argument):Atom = AtomWithArguments(_cache,Seq(arg1,arg2))
  def error(arg1: Argument, arg2: Argument):Atom = AtomWithArguments(_error,Seq(arg1,arg2))
  def reach(arg1: Argument, arg2: Argument, arg3: Int):Atom = AtomWithArguments(_reach,Seq(arg1,arg2,iVal(arg3)))
  def req(arg1: Argument, arg2: Int):Atom = AtomWithArguments(_req,Seq(arg1,iVal(arg2)))
  def cache(arg1: Argument, arg2: Int):Atom = AtomWithArguments(_cache,Seq(arg1,iVal(arg2)))
  def error(arg1: Int, arg2: Int):Atom = AtomWithArguments(_error,Seq(iVal(arg1),iVal(arg2)))


  /*
   % STATIC:
   sat(I,N) :- item(I), node(N), w_req(I,N), w_cache(I,N).
   sat(I,N) :- item(I), node(N), w_req(I,N), getFrom(I,N,M).
   needAt(I,N) :- item(I), node(N), w_req(I,N), not w_cache(I,N).
   conn(N,M) :- edge(N,M), not w_error(N,M).
   getFrom(I,N,M) :- needAt(I,N), minReach(I,N,M), not n_getFrom(I,N,M).
   n_getFrom(I,N,M2) :- getFrom(I,N,M), minReach(I,N,M2), M != M2.
   minReach(I,N,M) :- itemReach(I,N,M,K), not n_minReach(I,N,M,K).
   n_minReach(I,N,M,K) :- itemReach(I,N,M,K), itemReach(I,N,M2,K2), K2 < K.
   itemReach(I,N,M,K) :- needAt(I,N), w_cache(I,M), reach(N,M,K).
   reach(N,M,1) :- conn(N,M).
   reach(N,M,K) :- reach(N,M0,K0), conn(M0,M), N!=M, K=K0+1.

   %DYNAMIC:
   w_req(I,N) :- req(I,N,T)
   w_cache(I,N) :- cache(I,N,T)
   w_error(N,M) :- error(N,M,T)
   */

  override def larsProgram(windowSize: Int): LarsProgram = {

    val I:Variable = StringVariable("I")
    val N:Variable = StringVariable("N")
    val M:Variable = StringVariable("M")
    val M0:Variable = StringVariable("M0")
    val M2:Variable = StringVariable("M2")
    val K:Variable = Variable("K")
    val K0:Variable = Variable("K0")
    val K2:Variable = Variable("K2")

    //def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), At(time), atom)
    def wD(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Diamond, atom)
    //def wB(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Box, atom)

    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    val rules: Seq[LarsRule] = Seq[LarsRule](
      hit(I,N) <= item(I) and node(N) and wD(n,req(I,N)) and wD(n,cache(I,N)),
      get(I,N) <= item(I) and node(N) and wD(n,req(I,N)) and getFrom(I,N,M),
      fail(I,N) <= item(I) and node(N) and wD(n,req(I,N)) not hit(I,N) not get(I,N),
      needAt(I,N) <= item(I) and node(N) and wD(n,req(I,N)) not wD(n,cache(I,N)),
      conn(N,M) <= edge(N,M) not wD(n,error(N,M)),
      getFrom(I,N,M) <= needAt(I,N) and minReach(I,N,M) not n_getFrom(I,N,M),
      n_getFrom(I,N,M2) <= getFrom(I,N,M) and minReach(I,N,M2) and Neq(M,M2), //!
      minReach(I,N,M) <= itemReach(I,N,M,K) not n_minReach(I,N,M,K),
      n_minReach(I,N,M,K) <= itemReach(I,N,M,K) and itemReach(I,N,M2,K2) and Lt(K2,K), //!
      itemReach(I,N,M,K) <= needAt(I,N) and wD(n,cache(I,M)) and reach(N,M,K),
      reach(N,M,1) <= conn(N,M),
      reach(N,M,K) <= reach(N,M0,K0) and conn(M0,M) and Neq(N,M) and Incr(K0,K) and length(K0) and length(K)//!
    ) ++ (factAtoms() map larsFact)

    LarsProgram(rules)

  }

  def larsFact(a: Atom): LarsRule = UserDefinedLarsRule(a,Set(),Set())

  var printRulesOnce = false

  override lazy val staticRules: Seq[NormalRule] = {

    val I:Variable = StringVariable("I")
    val N:Variable = StringVariable("N")
    val M:Variable = StringVariable("M")
    val M0:Variable = StringVariable("M0")
    val M2:Variable = StringVariable("M2")
    val K:Variable = Variable("K")
    val K0:Variable = Variable("K0")
    val K2:Variable = Variable("K2")

    var rules = Seq[NormalRule]()
    def s(ats: Atom*) = ats.toSet[Atom]

    rules = rules :+
    //   sat(I,N) :- item(I), node(N), w_req(I,N), w_cache(I,N).
    rule(sat(I,N), s(item(I), node(N), w_req(I,N), w_cache(I,N)), s()) :+
    //   sat(I,N) :- item(I), node(N), w_req(I,N), getFrom(I,N,M).
    rule(sat(I,N), s(item(I), node(N), w_req(I,N), getFrom(I,N,M)), s()) :+
    //  unsat(I,N) :- item(I), node(N), w_req(I,N), not sat(I,N).
    rule(unsat(I,N), s(item(I), node(N), w_req(I,N)), s(sat(I,N))) :+
    //  needAt(I,N) :- item(I), node(N), w_req(I,N), not w_cache(I,N).
    rule(needAt(I,N), s(item(I), node(N), w_req(I,N)),  s(w_cache(I,N)) ) :+
    //   conn(N,M) :- edge(N,M), not w_error(N,M).
    rule(conn(N,M), s(edge(N,M)),  s(w_error(N,M))) :+
    //   getFrom(I,N,M) :- needAt(I,N), minReach(I,N,M), not n_getFrom(I,N,M).
    rule(getFrom(I,N,M), s(needAt(I,N), minReach(I,N,M)),  s(n_getFrom(I,N,M))) :+
    //   n_getFrom(I,N,M2) :- getFrom(I,N,M), minReach(I,N,M2), M != M2.
    rule(n_getFrom(I,N,M2), s(getFrom(I,N,M), minReach(I,N,M2), Lt(M,M2)), s()) :+
    //   minReach(I,N,M) :- itemReach(I,N,M,K), not n_minReach(I,N,M,K).
    rule(minReach(I,N,M), s(itemReach(I,N,M,K)),  s(n_minReach(I,N,M,K))) :+
    //   n_minReach(I,N,M,K) :- itemReach(I,N,M,K), itemReach(I,N,M2,K2), K2 < K.
    rule(n_minReach(I,N,M,K), s(itemReach(I,N,M,K), itemReach(I,N,M2,K2), Lt(K2,K)), s()) :+
    //   itemReach(I,N,M,K) :- needAt(I,N), w_cache(I,M), reach(N,M,K).
    rule(itemReach(I,N,M,K), s(needAt(I,N), w_cache(I,M), reach(N,M,K)), s()) :+
    //   reach(N,M,1) :- conn(N,M).
    rule(reach(N,M,iVal(1)), s(conn(N,M)), s()) :+
    //   reach(N,M,K) :- reach(N,M0,K0), conn(M0,M), N!=M, incr(K0,K).
    rule(reach(N,M,K), s(reach(N,M0,K0), conn(M0,M), Neq(N,M), Incr(K0,K), length(K0), length(K)), s())

    /* facts */

    rules = rules ++ (factAtoms map fact)

    //

    val program = AspProgram(rules.toList)
    val inspect = StaticProgramInspection.forAsp(program)
    val grounder = GrounderInstance.oneShotAsp(inspect)

    val groundRules = rules flatMap (grounder.ground(_))

    if (printRulesOnce) {
      println()
      groundRules foreach println
      println(f"\n${groundRules.size} ground rules")
      printRulesOnce = false
    }

    groundRules

  } //end staticRules

  def factAtoms(): Seq[Atom] = {
    var atoms = Seq[Atom]() ++ edges ++ nodeAtoms
    (1 to nrOfItems) foreach (i => atoms = atoms :+ item("i"+i))
    (1 to maxPathLength) foreach (k => atoms = atoms :+ length(k))
    atoms
  }

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = Seq()

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = {
    /*
       w_req(I,N) :- req_at(I,N,T)
       w_cache(I,N) :- cache_at(I,N,T)
       w_error(N,M) :- error_at(N,M,T)
    */
    var rules = Seq[NormalRule]()
    def pin(a:Atom,t: Int) = PinnedAtom.asPinnedAtAtom(a, TimePoint(t))
    def it(i:Int) = StringValue("i"+i)
    for (i <- 1 to nrOfItems; n <- nodes()) {
      rules = rules :+
        rule(w_req(it(i),n), pin(req(it(i),n),t)) :+
        rule(w_cache(it(i),n), pin(cache(it(i),n),t))
    }
    for (e <- edges) {
      val args = e.asInstanceOf[AtomWithArguments].arguments
      val n = args(0)
      val m = args(1)
      rules = rules :+ rule(w_error(n,m), pin(error(n,m),t))
    }
    rules
  }

}

case class CacheHopsEvalInst1(timePoints: Int, nrOfItems: Int, printRules: Boolean, random: Random) extends CacheHopsEvalInst(random) {

  override val windowSize = 10

  if (printRules) {
    printRulesOnce = true
  }

  override lazy val edges: Set[Atom] = { //part of initialization
    Set((1,2),(2,3),(3,4),(4,5),(5,6),(6,7),
        (1,8),(8,4),(1,6)) map { case (x,y) => edge(x,y) }
  }

  val i1 = StringValue("i1")

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    t % 30 match {
      case 0 => Seq(req(i1,1),cache(i1,4))
      case 2 => Seq(cache(i1,7))
      case 4 => Seq(error(8,4))
      case 6 => Seq(cache(i1,1))
      case 8 => Seq(req(i1,1))
      case 10 => Seq(cache(i1,7))
      case 16 => Seq(cache(i1,7))
      case 20 => Seq(req(i1,1))
      case _ => Seq()
    }
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {
    if (optModel.isEmpty) {
      print(f"x($t)")
      return
    }

    val model = optModel.get

    def has(atom: Atom) = contains(model,t,atom)
    def hasNot(atom: Atom) = notContains(model,t,atom)
    def hasSomeOf(ats: Atom*) = containsSomeOf(model,t,ats.toSeq)

    val q = t % 30
    if (q >= 0 && q < 2) {
      has(getFrom(i1,1,4))
      has(sat(i1,1))
      has(itemReach(i1,1,4,2))
      has(itemReach(i1,1,4,3))
      has(minReach(i1,1,4))
    } else if (q >= 2 && q < 4) {
      //from before:
      has(getFrom(i1,1,4)) //keep
      has(sat(i1,1))
      has(itemReach(i1,1,4,2))
      has(itemReach(i1,1,4,3))
      has(minReach(i1,1,4))
      //new:
      has(itemReach(i1,1,7,2))
      has(itemReach(i1,1,7,5))
      has(itemReach(i1,1,7,6))
      has(minReach(i1,1,7))
    } else if (q >= 4 && q < 6) {
      has(getFrom(i1,1,7)) //switch
    } else if (q >= 6 && q < 8) {
      hasNot(getFrom(i1,1,4))
      hasNot(getFrom(i1,1,7))
      has(sat(i1,1))
    } else if (q >= 8 && q < 10) {
      has(sat(i1,1))
    } else if (q >= 10 && q <= 16) {
      hasNot(getFrom(i1,1,4))
      hasNot(getFrom(i1,1,7))
      has(sat(i1,1))
    } else if (q >= 17 && q <= 18) {
      has(getFrom(i1,1,7))
    } else if (q == 19) {
      hasNot(getFrom(i1,1,4))
      hasNot(getFrom(i1,1,7))
      hasNot(sat(i1,1))
    } else if (q >= 20 && q <= 26) {
      has(getFrom(i1,1,7))
    } else {
      hasNot(getFrom(i1,1,4))
      hasNot(getFrom(i1,1,7))
      hasNot(sat(i1,1))
      has(unsat(i1,1))
    }
  }
}

case class CacheHopsEvalInst2(timePoints: Int, nrOfItems: Int, printRules: Boolean, random: Random) extends CacheHopsEvalInst(random) {

  override val windowSize = 15

  if (printRules) {
    printRulesOnce = true
  }

  /*
  override lazy val edges: Set[Atom] = {
    val consecutive = for (i <- 1 to 15) yield (i,i+1)
    val pairs = Set((8,1),(16,1)) ++ consecutive
    pairs map { case (x,y) => edge(x,y) }
  }
  */

  override lazy val edges: Set[Atom] = {
    val pairs = Set((1,2),(2,4),(4,7),(7,8),(8,9),(9,10),(10,16),(16,1),(8,1),(16,9))
    //val pairs = Set((1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10),(10,11),(11,12),(12,16),(16,1),(8,1),(16,9))
    pairs map { case (x,y) => edge(x,y) }
  }

  val i1 = StringValue("i1")

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    t % 40 match {
      case 0 => Seq(cache(i1,16),req(i1,1))
      case 2 => Seq(cache(i1,9))
      case 4 => Seq(req(i1,2))
      case 6 => Seq(req(i1,10))
      case 8 => Seq(cache(i1,4))
      case 10 => Seq(req(i1,1),req(i1,7))
      case 12 => Seq(cache(i1,9),cache(i1,1))
      case 14 => Seq(error(8,9))
      //nothing at 16; where cache(i1,16,1) expires
      case 18 => {
        Seq(req(i1, 2), cache(i1, 1), cache(i1, 9))
      }
      case 20 => Seq(req(i1,10))
      case 22 => Seq(req(i1,1),req(i1,7))
      case 32 => Seq(error(16,1))
      case _=> Seq()
    }
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {
    if (optModel.isEmpty) {
      print(f"x($t)")
      return
    }

    val model = optModel.get

    def has(atom: Atom) = contains(model,t,atom)
    def hasNot(atom: Atom) = notContains(model,t,atom)
    def hasSomeOf(ats: Atom*) = containsSomeOf(model,t,ats.toSeq)

    val ensureModelMaintenance = false

    val q = t % 40
    if (q >= 0 && q < 2) {
      has(getFrom(i1,1,16))
    } else if (q >= 2 && q < 4) {
      has(getFrom(i1,1,9))
    } else if (q >= 4 && q < 6) {
      has(getFrom(i1,1,9))
      has(getFrom(i1,2,9))
    } else if (q >= 6 && q < 8) {
      has(getFrom(i1,1,9))
      has(getFrom(i1,2,9))
      has(getFrom(i1,10,16))
    } else if (q >= 8 && q < 10) {
      has(getFrom(i1,1,4))
      has(getFrom(i1,2,4))
      has(getFrom(i1,10,16))
    } else if (q >= 10 && q < 12) {
      has(getFrom(i1,1,4))
      has(getFrom(i1,2,4))
      has(getFrom(i1,7,9))
      has(getFrom(i1,10,16))
    } else if (q >= 12 && q < 14) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,4))
      has(getFrom(i1,10,16))
      if (ensureModelMaintenance) {
        has(getFrom(i1,7,9))
      } else {
        hasSomeOf(getFrom(i1,7,9),getFrom(i1,7,1))
      }
    } else if (q >= 14 && q < 16) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,4))
      has(getFrom(i1,7,1))
      has(getFrom(i1,10,16))
    } else if (q >= 16 && q < 18) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,4))
      has(getFrom(i1,7,1))
      has(getFrom(i1,10,1))
    } else if (q >= 18 && q < 24) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,4))
      has(getFrom(i1,7,1))
      if (ensureModelMaintenance) {
        has(getFrom(i1,10,1))
      } else {
        hasSomeOf(getFrom(i1,10,1),getFrom(i1,10,9))
      }
    } else if (q >= 24 && q < 30) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,1))
      has(getFrom(i1,7,1))
      if (ensureModelMaintenance) {
        has(getFrom(i1,10,1))
      } else {
        hasSomeOf(getFrom(i1,10,1),getFrom(i1,10,9))
      }
    } else if (q >= 30 && q < 32) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      if (ensureModelMaintenance) {
        has(getFrom(i1,2,1))
        has(getFrom(i1,7,1))
        has(getFrom(i1,10,1))
      } else {
        hasSomeOf(getFrom(i1,2,1),getFrom(i1,2,9))
        hasSomeOf(getFrom(i1,7,1),getFrom(i1,7,9))
        hasSomeOf(getFrom(i1,10,1),getFrom(i1,10,9))
      }
    } else if (q >= 32 && q < 34) {
      hasNot(getFrom(i1,1,4))
      hasNot(needAt(i1,1))
      has(sat(i1,1))
      has(getFrom(i1,2,1))
      has(getFrom(i1,10,9))
      if (ensureModelMaintenance) {
        has(getFrom(i1,7,1))
      } else {
        hasSomeOf(getFrom(i1,7,1),getFrom(i1,7,9))
      }
    } else if (q >= 34 && q < 36) {
      has(unsat(i1,1))
      hasNot(sat(i1,2))
      hasNot(unsat(i1,2))
      has(unsat(i1,7))
      has(unsat(i1,10))
    } else if (q >= 36 && q < 38) {
      has(unsat(i1,1))
      hasNot(sat(i1,2))
      hasNot(unsat(i1,2))
      has(unsat(i1,7))
      hasNot(sat(i1,10))
      hasNot(unsat(i1,10))
    } else if (q >= 38 && q < 40) {
      hasNot(sat(i1,1))
      hasNot(unsat(i1,1))
      hasNot(sat(i1,2))
      hasNot(unsat(i1,2))
      hasNot(sat(i1,7))
      hasNot(unsat(i1,7))
      hasNot(sat(i1,10))
      hasNot(unsat(i1,10))
    }
  }
}
