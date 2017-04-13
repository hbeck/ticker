package jtms.evaluation.instances

import core._
import core.asp.{AspProgram, NormalRule}
import core.grounding.{GrounderInstance, StaticProgramInspection}
import core.lars._
import jtms.JtmsUpdateAlgorithm
import jtms.evaluation.StreamingTmsStandardEvalInst

import scala.util.Random

/**
  * Created by hb on 10.04.17.
  */
abstract class CacheHopsEvalInst(random: Random) extends StreamingTmsStandardEvalInst {

  def windowSize: Int
  def timePoints: Int
  def nrOfItems: Int
  def postProcessGrounding: Boolean

  def edges: Set[Atom]

  final def nodes(): Set[Value] = edges collect { case a:GroundAtomWithArguments => a.arguments } flatten
  final def nodeAtoms(): Set[Atom] = nodes map (node(_))

  val _node = Predicate("node")
  val _sat = Predicate("sat")
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

  //do not use predicates used in grounding since we do not want to re-ground here
  val _notEq = Predicate("notEq") //corresponds to neq
  val _lowerThan = Predicate("lowerThan") //corresponds to lt
  val _incr = Predicate("incr") //inr(K0,K) corresponds to plus(K0,1,K)
  //windows
  val _w_req = Predicate("w_req")
  val _w_cache = Predicate("w_cache")
  val _w_error = Predicate("w_error")
  //signals
  val _req = Predicate("req")
  val _cache = Predicate("cache")
  val _error = Predicate("error")

  def node(arg1: Argument) = AtomWithArguments(_node,Seq(arg1))
  def sat(arg1: Argument, arg2: Argument) = AtomWithArguments(_sat,Seq(arg1,arg2))
  def sat(arg1: Argument, arg2: Int) = AtomWithArguments(_sat,Seq(arg1,IntValue(arg2)))
  def unsat(arg1: Argument, arg2: Argument) = AtomWithArguments(_unsat,Seq(arg1,arg2))
  def unsat(arg1: Argument, arg2: Int) = AtomWithArguments(_unsat,Seq(arg1,IntValue(arg2)))
  def getFrom(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_getFrom,Seq(arg1,arg2,arg3))
  def getFrom(arg1: Argument, arg2: Int, arg3: Int) = AtomWithArguments(_getFrom,Seq(arg1,IntValue(arg2),IntValue(arg3)))
  def item(arg1: Argument) = AtomWithArguments(_item,Seq(arg1))
  def n_getFrom(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_n_getFrom,Seq(arg1,arg2,arg3))
  def needAt(arg1: Argument, arg2: Argument) = AtomWithArguments(_needAt,Seq(arg1,arg2))
  def needAt(arg1: Argument, arg2: Int) = AtomWithArguments(_needAt,Seq(arg1,IntValue(arg2)))
  def conn(arg1: Argument, arg2: Argument) = AtomWithArguments(_conn,Seq(arg1,arg2))
  def edge(arg1: Argument, arg2: Argument) = AtomWithArguments(_edge,Seq(arg1,arg2))
  def edge(arg1: Int, arg2: Int) = AtomWithArguments(_edge,Seq(IntValue(arg1),IntValue(arg2)))
  def minReach(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_minReach,Seq(arg1,arg2,arg3))
  def n_minReach(arg1: Argument, arg2: Argument, arg3: Argument, arg4: Argument) = AtomWithArguments(_n_minReach,Seq(arg1,arg2,arg3,arg4))
  def itemReach(arg1: Argument, arg2: Argument, arg3: Argument, arg4: Argument) = AtomWithArguments(_itemReach,Seq(arg1,arg2,arg3,arg4))
  def reach(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_reach,Seq(arg1,arg2,arg3))
  def notEq(arg1: Argument, arg2: Argument) = AtomWithArguments(_notEq,Seq(arg1,arg2))
  def lowerThan(arg1: Argument, arg2: Argument) = AtomWithArguments(_lowerThan,Seq(arg1,arg2))
  def incr(arg1: Argument, arg2: Argument) = AtomWithArguments(_incr,Seq(arg1,arg2))
  def w_req(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_req,Seq(arg1,arg2))
  def w_cache(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_cache,Seq(arg1,arg2))
  def w_error(arg1: Argument, arg2: Argument) = AtomWithArguments(_w_error,Seq(arg1,arg2))
  def req(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_req,Seq(arg1,arg2,arg3))
  def cache(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_cache,Seq(arg1,arg2,arg3))
  def error(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_error,Seq(arg1,arg2,arg3))

  //LARS:
  val _neq = Predicate("neq")
  val _lt = Predicate("lt")
  val _plus = Predicate("plus")
  def req(arg1: Argument, arg2: Argument) = AtomWithArguments(_req,Seq(arg1,arg2))
  def cache(arg1: Argument, arg2: Argument) = AtomWithArguments(_cache,Seq(arg1,arg2))
  def error(arg1: Argument, arg2: Argument) = AtomWithArguments(_error,Seq(arg1,arg2))
  def reach(arg1: Argument, arg2: Argument, arg3: Int) = AtomWithArguments(_reach,Seq(arg1,arg2,IntValue(arg3)))
  def neq(arg1: Argument, arg2: Argument) = AtomWithArguments(_neq,Seq(arg1,arg2))
  def lt(arg1: Argument, arg2: Argument) = AtomWithArguments(_lt,Seq(arg1,arg2))
  def plus(arg1: Argument, arg2: Argument, arg3: Argument) = AtomWithArguments(_plus,Seq(arg1,arg2,arg3))
  def plus(arg1: Int, arg2: Argument, arg3: Argument) = AtomWithArguments(_plus,Seq(IntValue(arg1),arg2,arg3))

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

  def larsProgram(windowSize: Int): LarsProgram = {

    val I:Variable = StringVariable("I")
    val N:Variable = StringVariable("N")
    val M:Variable = StringVariable("M")
    val M0:Variable = StringVariable("M0")
    val M2:Variable = StringVariable("M2")
    val K:Variable = Variable("K")
    val K0:Variable = Variable("K0")
    val K2:Variable = Variable("K2")

    def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), At(time), atom)
    def wD(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Diamond, atom)
    def wB(windowSize: Int, atom: Atom) = WindowAtom(SlidingTimeWindow(windowSize), Box, atom)

    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    LarsProgram.from(
      sat(I,N) <= item(I) and node(N) and wD(n,req(I,N)) and wD(n,cache(I,N)),
      sat(I,N) <= item(I) and node(N) and wD(n,req(I,N)) and getFrom(I,N,M),
      unsat(I,N) <= item(I) and node(N) and wD(n,req(I,N)) not sat(I,N),
      needAt(I,N) <= item(I) and node(N) and wD(n,req(I,N)) not wD(n,cache(I,N)),
      conn(N,M) <= edge(N,M) not wD(n,error(N,M)),
      getFrom(I,N,M) <= needAt(I,N) and minReach(I,N,M) not n_getFrom(I,N,M),
      n_getFrom(I,N,M2) <= getFrom(I,N,M) and minReach(I,N,M2) and neq(M,M2), //!
      minReach(I,N,M) <= itemReach(I,N,M,K) not n_minReach(I,N,M,K),
      n_minReach(I,N,M,K) <= itemReach(I,N,M,K) and itemReach(I,N,M2,K2) and lt(K2,K), //!
      itemReach(I,N,M,K) <= needAt(I,N) and wD(n,cache(I,M)) and reach(N,M,K),
      reach(N,M,1) <= conn(N,M),
      reach(N,M,K) <= reach(N,M0,K0) and conn(M0,M) and neq(N,M) and plus(1,K0,K) //!
    )

  }

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
    rule(n_getFrom(I,N,M2), s(getFrom(I,N,M), minReach(I,N,M2), notEq(M,M2)), s()) :+
    //   minReach(I,N,M) :- itemReach(I,N,M,K), not n_minReach(I,N,M,K).
    rule(minReach(I,N,M), s(itemReach(I,N,M,K)),  s(n_minReach(I,N,M,K))) :+
    //   n_minReach(I,N,M,K) :- itemReach(I,N,M,K), itemReach(I,N,M2,K2), K2 < K.
    rule(n_minReach(I,N,M,K), s(itemReach(I,N,M,K), itemReach(I,N,M2,K2), lowerThan(K2,K)), s()) :+
    //   itemReach(I,N,M,K) :- needAt(I,N), w_cache(I,M), reach(N,M,K).
    rule(itemReach(I,N,M,K), s(needAt(I,N), w_cache(I,M), reach(N,M,K)), s()) :+
    //   reach(N,M,1) :- conn(N,M).
    rule(reach(N,M,IntValue(1)), s(conn(N,M)), s()) :+
    //   reach(N,M,K) :- reach(N,M0,K0), conn(M0,M), N!=M, incr(K0,K).
    rule(reach(N,M,K), s(reach(N,M0,K0), conn(M0,M), notEq(N,M), incr(K0,K)), s()) //do not use 'plus(K0,1,K)' instead of incr. grounder cannot resolve

    /* facts */

    rules = rules ++ (edges map (fact(_)))
    rules = rules ++ (nodeAtoms map (fact(_)))
    for (i <- 1 to nrOfItems) {
      rules = rules :+ fact(item("i"+i))
    }

    /* auxiliary relations lowerThan, incr, notEq */

    for (i <- 0 to (nodes.size - 1)) { //maximum reachability is for number of nodes
      for (j <- (i+1) to (nodes.size)) {
        rules = rules :+ fact(lowerThan(IntValue(i),IntValue(j)))
      }
    }
    for (i <- 0 to nodes.size) {
      rules = rules :+ fact(incr(IntValue(i),IntValue(i+1)))
    }
    for (n <- nodes) {
      for (m <- nodes) {
        if (n != m) {
          rules = rules :+ fact(notEq(n,m)) :+ fact(notEq(m,n))
        }
      }
    }

    //

    val program = AspProgram(rules.toList)
    val inspect = StaticProgramInspection.forAsp(program)
    val grounder = GrounderInstance.oneShotAsp(inspect)

    var groundRules = rules flatMap (grounder.ground(_))

    if (postProcessGrounding) {
      groundRules = postProcess(groundRules)
    }

    if (printRulesOnce) {
      println()
      groundRules foreach println
      println(f"\n${groundRules.size} ground rules")
      printRulesOnce = false
    }

    groundRules

  } //end staticRules

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = Seq()

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = {
    /*
       w_req(I,N) :- req(I,N,T)
       w_cache(I,N) :- cache(I,N,T)
       w_error(N,M) :- error(N,M,T)
    */
    var rules = Seq[NormalRule]()
    def it(i:Int) = StringValue("i"+i)
    for (i <- 1 to nrOfItems; n <- nodes()) {
      rules = rules :+
        rule(w_req(it(i),n), req(it(i),n,IntValue(t))) :+
        rule(w_cache(it(i),n), cache(it(i),n,IntValue(t)))
    }
    for (e <- edges) {
      val args = e.asInstanceOf[AtomWithArguments].arguments
      val n = args(0)
      val m = args(1)
      rules = rules :+ rule(w_error(n,m), error(n,m,IntValue(t)))
    }
    rules
  }

  //delete aux relation atoms, as facts and from rules where they hold
  //delete rules where they do not hold
  def postProcess(groundRules: Seq[NormalRule]): Seq[NormalRule] = {
    groundRules collect {
      case r if (!isRelation(r.head) && !unsatisfiedRelation(r)) => removeAuxiliary(r)
    }
  }

  def unsatisfiedRelation(r: NormalRule): Boolean = {
    val relations = r.body filter (isRelation(_))
    relations exists (a => !(satisfied(a)))
  }

  def satisfied(relation: Atom): Boolean = {
    val args = relation.arguments()
    relation.predicate match {
      case `_notEq` => args(0) != args(1)
      case `_lowerThan` => Integer.parseInt(""+args(0)) < Integer.parseInt(""+args(1))
      case `_incr` => Integer.parseInt(""+args(0)) + 1 == Integer.parseInt(""+args(1))
    }
  }

  def isRelation(a: Atom): Boolean = {
    val h = a.predicate
    h == _notEq || h == _lowerThan || h == _incr
  }

  def removeAuxiliary(r: NormalRule): NormalRule = {
    val h = r.head
    //item and node predicates only needed for grounding
    val p = r.pos filter (a => (!isRelation(a) && a.predicate !=_item && a != _node))
    val n = r.neg
    rule(h,p,n)
  }

  //

  def printModel(t:Int, model: Set[Atom]): Unit = {
    println(f"\nt=$t, q=${t%30}")
    model filter { a =>
      a.predicate != _edge && a.predicate != _node && a.predicate != _conn && a.predicate != _reach &&
        a.predicate != _itemReach && a.predicate != _n_minReach && a.predicate != _item && a.predicate != _n_getFrom
    } foreach println
  }

}

case class CacheHopsEvalInst1(timePoints: Int, nrOfItems: Int, postProcessGrounding: Boolean, printRules: Boolean, random: Random) extends CacheHopsEvalInst(random) {

  override val windowSize = 10

  if (printRules) {
    printRulesOnce = true
  }

  override lazy val edges: Set[Atom] = {
    Set(("n1","n2"),("n2","n3"),("n3","n4"),("n4","n5"),("n5","n6"),("n6","n7"),
        ("n1","m"),("m","n4"),("n1","n6")) map { case (x,y) => edge(x,y) }
  }

  val i1 = StringValue("i1")
  val n1 = StringValue("n1")
  val n4 = StringValue("n4")
  val n7 = StringValue("n7")
  val m = StringValue("m")

  override def generateFactsToAddAt(t: Int): Seq[NormalRule] = {
    def req_(item: StringValue, node: StringValue) = fact(req(item,node,IntValue(t)))
    def cache_(item: StringValue, node: StringValue) = fact(cache(item,node,IntValue(t)))
    def err_(fromNode: StringValue, toNode: StringValue) = fact(error(fromNode,toNode,IntValue(t)))
    t % 30 match {
      case 0 => Seq(req_(i1,n1),cache_(i1,n4))
      case 2 => Seq(cache_(i1,n7))
      case 4 => Seq(err_(m,n4))
      case 6 => Seq(cache_(i1,n1))
      case 8 => Seq(req_(i1,n1))
      case 10 => Seq(cache_(i1,n7))
      case 16 => Seq(cache_(i1,n7))
      case 20 => Seq(req_(i1,n1))
      case _ => Seq()
    }
  }

  override def verifyModel(tms: JtmsUpdateAlgorithm, t: Int): Unit = {
    if (tms.getModel.isEmpty) {
      print(f"x($t)")
      return
    }
    val model = tms.getModel.get
    val q = t % 30
    if (q >= 0 && q < 2) {
      assert(model.contains(getFrom(i1,n1,n4)))
      assert(model.contains(sat(i1,n1)))
      assert(model.contains(itemReach(i1,n1,n4,IntValue(2))))
      assert(model.contains(itemReach(i1,n1,n4,IntValue(3))))
      assert(model.contains(minReach(i1,n1,n4)))
    } else if (q >= 2 && q < 4) {
      //from before:
      assert(model.contains(getFrom(i1,n1,n4))) //keep
      assert(model.contains(sat(i1,n1)))
      assert(model.contains(itemReach(i1,n1,n4,IntValue(2))))
      assert(model.contains(itemReach(i1,n1,n4,IntValue(3))))
      assert(model.contains(minReach(i1,n1,n4)))
      //new:
      assert(model.contains(itemReach(i1,n1,n7,IntValue(2))))
      assert(model.contains(itemReach(i1,n1,n7,IntValue(5))))
      assert(model.contains(itemReach(i1,n1,n7,IntValue(6))))
      assert(model.contains(minReach(i1,n1,n7)))
    } else if (q >= 4 && q < 6) {
      assert(model.contains(getFrom(i1,n1,n7))) //switch
    } else if (q >= 6 && q < 8) {
      assert(!model.contains(getFrom(i1,n1,n4)))
      assert(!model.contains(getFrom(i1,n1,n7)))
      assert(model.contains(sat(i1,n1)))
    } else if (q >= 8 && q < 10) {
      assert(model.contains(sat(i1,n1)))
    } else if (q >= 10 && q <= 16) {
      assert(!model.contains(getFrom(i1,n1,n4)))
      assert(!model.contains(getFrom(i1,n1,n7)))
      assert(model.contains(sat(i1,n1)))
    } else if (q >= 17 && q <= 18) {
      assert(model.contains(getFrom(i1,n1,n7)))
    } else if (q == 19) {
      assert(!model.contains(getFrom(i1,n1,n4)))
      assert(!model.contains(getFrom(i1,n1,n7)))
      assert(!model.contains(sat(i1,n1)))
    } else if (q >= 20 && q <= 26) {
      assert(model.contains(getFrom(i1,n1,n7)))
    } else {
      assert(!model.contains(getFrom(i1,n1,n4)))
      assert(!model.contains(getFrom(i1,n1,n7)))
      assert(!model.contains(sat(i1,n1)))
      assert(model.contains(unsat(i1,n1)))
    }
  }
}

case class CacheHopsEvalInst2(timePoints: Int, nrOfItems: Int, postProcessGrounding: Boolean, printRules: Boolean, random: Random) extends CacheHopsEvalInst(random) {

  override val windowSize = 15

  if (printRules) {
    printRulesOnce = true
  }

  override lazy val edges: Set[Atom] = {
    val consecutive = for (i <- 1 to 15) yield (i,i+1)
    val pairs = Set((8,1),(16,1)) ++ consecutive
    pairs map { case (x,y) => edge(x,y) }
  }

  val i1 = StringValue("i1")

  override def generateFactsToAddAt(t: Int): Seq[NormalRule] = {
    def cache_(i: StringValue, n: Int) = fact(cache(i,IntValue(n),IntValue(t)))
    def req_(i: StringValue, n: Int) = fact(req(i,IntValue(n),IntValue(t)))
    def err_(n: Int, m: Int) = fact(error(IntValue(n),IntValue(m),IntValue(t)))
    t % 40 match {
      case 0 => Seq(cache_(i1,16),req_(i1,1))
      case 2 => Seq(cache_(i1,9))
      case 4 => Seq(req_(i1,2))
      case 6 => Seq(req_(i1,10))
      case 8 => Seq(cache_(i1,4))
      case 10 => Seq(req_(i1,1),req_(i1,7))
      case 12 => Seq(cache_(i1,9),cache_(i1,1))
      case 14 => Seq(err_(8,9))
      //nothing at 16; where cache(i1,16,1) expires
      case 18 => Seq(req_(i1,2),cache_(i1,1),cache_(i1,9))
      case 20 => Seq(req_(i1,10))
      case 22 => Seq(req_(i1,1),req_(i1,7))
      case _=> Seq()
    }
  }

  override def verifyModel(tms: JtmsUpdateAlgorithm, t: Int): Unit = {
    
    if (tms.getModel.isEmpty) {
      print(f"x($t)")
      return
    }
    val model = tms.getModel.get

    def contains(a: Atom) = {
      if (!model.contains(a)) {
        printModel(t,model)
        println(f"does not contain $a")
      }
      assert(false)
    }
    def notContains(a: Atom) = {
      if (model.contains(a)) {
        printModel(t,model)
        println(f"contains $a")
      }
      assert(false)
    }

    val q = t % 40
    if (q >= 0 && q < 2) {
      contains(getFrom(i1,1,16))
    } else if (q <= 2 && q < 4) {
      contains(getFrom(i1,1,9))
    } else if (q <= 4 && q < 6) {
      contains(getFrom(i1,1,9))
      contains(getFrom(i1,2,9))
    } else if (q <= 6 && q < 8) {
      contains(getFrom(i1,1,9))
      contains(getFrom(i1,2,9))
      contains(getFrom(i1,10,16))
    } else if (q <= 8 && q < 10) {
      contains(getFrom(i1,1,4))
      contains(getFrom(i1,2,4))
      contains(getFrom(i1,10,16))
    } else if (q <= 10 && q < 12) {
      contains(getFrom(i1,1,4))
      contains(getFrom(i1,2,4))
      contains(getFrom(i1,7,9))
      contains(getFrom(i1,10,16))
    } else if (q <= 12 && q < 14) {
      notContains(getFrom(i1,1,4))
      notContains(needAt(i1,1))
      contains(sat(i1,1))
      contains(getFrom(i1,2,4))
      contains(getFrom(i1,7,9))
      contains(getFrom(i1,10,16))
    } else if (q <= 14 && q < 16) {
      notContains(getFrom(i1,1,4))
      notContains(needAt(i1,1))
      contains(sat(i1,1))
      contains(getFrom(i1,2,4))
      contains(getFrom(i1,7,1))
      contains(getFrom(i1,10,16))
    } else if (q <= 16 && q < 24) {
      notContains(getFrom(i1,1,4))
      notContains(needAt(i1,1))
      contains(sat(i1,1))
      contains(getFrom(i1,2,4))
      contains(getFrom(i1,7,1))
      contains(getFrom(i1,10,1))
    } else if (q <= 24 && q < 32) {
      notContains(getFrom(i1,1,4))
      notContains(needAt(i1,1))
      contains(sat(i1,1))
      contains(getFrom(i1,2,1))
      contains(getFrom(i1,7,1))
      contains(getFrom(i1,10,1))
    } else if (q <= 32 && q < 34) {
      notContains(getFrom(i1,1,4))
      notContains(needAt(i1,1))
      contains(sat(i1,1))
      contains(getFrom(i1,2,1))
      contains(getFrom(i1,7,9))
      contains(getFrom(i1,10,9))
    } else if (q <= 34 && q < 36) {
      contains(unsat(i1,1))
      notContains(sat(i1,2))
      notContains(unsat(i1,2))
      contains(unsat(i1,7))
      contains(unsat(i1,10))
    } else if (q <= 36 && q < 38) {
      contains(unsat(i1,1))
      notContains(sat(i1,2))
      notContains(unsat(i1,2))
      contains(unsat(i1,7))
      notContains(sat(i1,10))
      notContains(unsat(i1,10))
    } else if (q <= 38 && q < 40) {
      notContains(sat(i1,1))
      notContains(unsat(i1,1))
      notContains(sat(i1,2))
      notContains(unsat(i1,2))
      notContains(sat(i1,7))
      notContains(unsat(i1,7))
      notContains(sat(i1,10))
      notContains(unsat(i1,10))
    }
  }
}
