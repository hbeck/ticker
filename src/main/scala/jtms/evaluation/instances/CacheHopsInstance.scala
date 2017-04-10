package jtms.evaluation.instances

import core._
import core.asp.{AspProgram, NormalRule}
import core.grounding.{GrounderInstance, StaticProgramInspection}
import jtms.JtmsUpdateAlgorithm
import jtms.evaluation.StreamingTmsEvalInstance

import scala.util.Random

/**
  * Created by hb on 10.04.17.
  */
abstract class CacheHopsInstance extends StreamingTmsEvalInstance {

  def nrOfItems: Int
  def windowSize: Int
  def edges: Set[Atom]

  final def nodes(): Set[Value] = edges collect { case a:GroundAtomWithArguments => a.arguments } flatten
  final def nodeAtoms(): Set[Atom] = nodes map (node(_))

  val _node = Predicate("node")
  def node(arg1: Argument) = AtomWithArguments(_node,Seq(arg1))
  val _sat = Predicate("sat")
  def sat(arg1: Variable, arg2: Variable) = AtomWithArguments(_sat,Seq(arg1,arg2))
  val _item = Predicate("item")
  def item(arg1: Argument) = AtomWithArguments(_item,Seq(arg1))
  val _getFrom = Predicate("getFrom")
  def getFrom(arg1: Variable, arg2: Variable, arg3: Variable) = AtomWithArguments(_getFrom,Seq(arg1,arg2,arg3))
  val _n_getFrom = Predicate("n_getFrom")
  val _needAt = Predicate("needAt")
  val _conn = Predicate("conn")
  val _edge = Predicate("edge")
  val _minReach = Predicate("minReach")
  val _n_minReach = Predicate("n_minReach")
  val _itemReach = Predicate("itemReach")
  val _reach = Predicate("reach")

  val _neq = Predicate("neq")
  val _lt = Predicate("lt")
  val _plus = Predicate("plus")

  //windows
  val _w_req = Predicate("w_req")
  def w_req(arg1: Variable, arg2: Variable) = AtomWithArguments(_w_req,Seq(arg1,arg2))
  val _w_cache = Predicate("w_cache")
  def w_cache(arg1: Variable, arg2: Variable) = AtomWithArguments(_w_cache,Seq(arg1,arg2))
  val _w_error = Predicate("w_error")
  def w_error(arg1: Variable, arg2: Variable) = AtomWithArguments(_w_error,Seq(arg1,arg2))

  //signals
  val _req = Predicate("req")
  val _cache = Predicate("cache")
  val _error = Predicate("error")

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


  override def staticRules(): Seq[NormalRule] = {

    val I:Variable = StringVariable("I")
    val N:Variable = StringVariable("N")
    val M:Variable = StringVariable("M")
    val K:Variable = StringVariable("K")

    val _0=Set[Atom]()

    var rules = Seq[NormalRule]()
    rules = rules :+ rule(sat(I,N),Set[Atom](item(I),w_req(I,N),w_cache(I,N)),_0)

    for (i <- 1 to nrOfItems) {
      rules = rules :+ fact(item("i"+i))
    }
    rules = rules ++ (nodeAtoms() map (fact(_)))

    //

    val program = AspProgram(rules.toList)
    val inspect = StaticProgramInspection.forAsp(program)
    val grounder = GrounderInstance.oneShotAsp(inspect)

    rules flatMap (grounder.ground(_))

  }

}

case class SimpleCacheHopInstance(random: Random, timePoints: Int, windowSize: Int, nrOfItems: Int, edges: Set[Atom]) extends CacheHopsInstance {

  override def factsToAddAt(t: Int): Seq[NormalRule] = Seq()
  override def rulesToAddAt(t: Int): Seq[NormalRule] = Seq()
  override def rulesToRemoveAt(t: Int): Seq[NormalRule] = Seq()
  override def factsToRemoveAt(t: Int): Seq[NormalRule] = Seq()
  override def verifyModel(tms: JtmsUpdateAlgorithm, t: Int) = {}

}