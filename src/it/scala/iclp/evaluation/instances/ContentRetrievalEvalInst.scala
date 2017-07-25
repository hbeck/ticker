package iclp.evaluation.instances

import core._
import core.asp.NormalRule
import core.lars._
import iclp.evaluation.StreamingTmsEvalInst

import scala.util.Random

/**
  * Created by hb on 10.04.17.
  */
abstract class ContentRetrievalEvalInst(nrOfItems: Int, random: Random) extends StreamingTmsEvalInst {

  val node = Predicate("node")
  val item = Predicate("item")
  val need = Predicate("need")
  val req = Predicate("req")
  val avail = Predicate("avail")
  val get = Predicate("get")
  val cache = Predicate("cache")
  val source = Predicate("source")
  val nGet = Predicate("nGet")
  val qual = Predicate("qual")
  val reach = Predicate("reach")
  val conn = Predicate("conn")
  val edge = Predicate("edge")
  val down = Predicate("down")
  val lev = Predicate("lev")
  val qLev = Predicate("qLev")


  val nrOfQualityLevels = 2
  val windowSize: Int
  val timePoints: Int

  val items = (1 to nrOfItems) map (IntValue(_))
  val qualityLevels = (0 to nrOfQualityLevels) map (IntValue(_))

  val edgeAtoms: Set[Atom] = { //part of initialization
    val pairs0To9 = (0 to 9) map (i => (i, i + 1)) toSet
    val allPairs = pairs0To9 ++ Set((0, 10), (1, 10), (2, 8), (3, 7))

    val valuePairs = allPairs map (p => (IntValue(p._1), IntValue(p._2)))

    valuePairs flatMap (p => Set(edge(p._1, p._2), edge(p._2, p._1)))
  }

  val nodes: Set[Value] = edgeAtoms collect { case a: GroundAtomWithArguments => a.arguments } flatten
  val orderedNodes = nodes.toVector
  val nodeAtoms: Set[Atom] = nodes map (node(_))

  val maxPathLength: Int = nodes.size //at most number of nodes

  def pickRandomNodeValue(): Value = orderedNodes(random.nextInt(orderedNodes.length))

  def pickRandomItemValue(): Value = items(random.nextInt(items.length))

  def pickQualityLevelValue(): Value = qualityLevels(random.nextInt(qualityLevels.length))

  override def larsProgram(windowSize: Int): LarsProgram = {

    val I: Variable = StringVariable("I")
    val N: Variable = StringVariable("N")

    val M: Variable = StringVariable("M")
    val M0: Variable = StringVariable("M0")
    val M2: Variable = StringVariable("M2")

    val K: Variable = Variable("K")
    val K0: Variable = Variable("K0")
    val K2: Variable = Variable("K2")

    val L: Variable = StringVariable("L")
    val L0: Variable = StringVariable("L0")

    def s(ats: Atom*): Set[ExtendedAtom] = ats.toSet

    val n = windowSize

    val rules: Seq[LarsRule] = Seq[LarsRule](
      need(I, N) <= item(I) and node(N) and wD(n, req(I, N)),
      avail(I, N) <= item(I) and node(N) and wD(n, cache(I, N)),
      get(I, N, M) <= source(I, N, M) not nGet(I, N, M),
      nGet(I, N, M) <= node(M) and get(I, N, M0) and Neq(M, M0),
      nGet(I, N, M) <= source(I, N, M) and source(I, N, M0) and Neq(M, M0) and qual(M, L) and qual(M0, L0) and Lt(L, L0),
      source(I, N, M) <= need(I, N) not avail(I, N) and avail(I, M) and reach(N, M),
      reach(N, M) <= conn(N, M),
      reach(N, M) <= reach(N, M0) and conn(M0, M) and Neq(M0, M) and Neq(N, M),
      //conn(N, M) <= edge(N, M) not tup_wB(n, down(M)),
      conn(N, M) <= edge(N, M) not wB(n, down(M)),
      qual(N, L) <= node(N) and lev(L) and lev(L0) and Lt(L0, L) and wD(n, qLev(N, L)) not wD(n, qLev(N, L0))
    ) ++ (factAtoms() map larsFact)

    LarsProgram(rules)

  }

  var printRulesOnce = false

  override lazy val staticRules: Seq[NormalRule] = ???


  def factAtoms(): Seq[Atom] = {
    val networkAtoms = edgeAtoms ++ nodeAtoms
    val itemAtoms = items map (item(_))
    val levels = qualityLevels map (lev(_))

    (networkAtoms ++ itemAtoms ++ levels) toSeq
  }

  override def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule] = Seq()

  override def rulesExpiringAfterWindow(t: Int): Seq[NormalRule] = ???

}

case class ContentRetrieval1EvalInst(windowSize: Int, timePoints: Int, nrOfItems: Int, random: Random) extends ContentRetrievalEvalInst(nrOfItems, random) {

  val i = StringValue("i1")

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    val requestItems = items collect {
      case item if random.nextDouble() <= 0.1 => req(item, pickRandomNodeValue())
    }

    val cachedItem: Option[Atom] = if (random.nextDouble() <= 0.1) {
      Some(cache(pickRandomItemValue(), pickRandomNodeValue()))
    } else {
      None
    }

    val qLevelsPerNodes = nodes collect {
      case n if random.nextDouble() <= 3.0 / windowSize => qLev(n, pickQualityLevelValue())
      case n if random.nextDouble() <= 1.0 / (windowSize / 2.0) => qLev(n, pickQualityLevelValue())
    }

    val downNodes = if (random.nextDouble() <= 0.1) {
      Some(down(pickRandomNodeValue()))
    } else {
      None
    }

    requestItems ++ cachedItem ++ qLevelsPerNodes ++ downNodes
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {

  }
}

case class ContentRetrieval2EvalInst(windowSize: Int, timePoints: Int, nrOfItems: Int, random: Random) extends ContentRetrievalEvalInst(nrOfItems, random) {

  var previousQualityLevels: Map[Value, Value] = Map()

  var downUntil: Map[Value, Int] = Map()

  override def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    val requestItems = items collect {
      case i if random.nextDouble() <= 0.5 => Set(
        req(i, pickRandomNodeValue()),
        req(i, pickRandomNodeValue()),
        req(i, pickRandomNodeValue())
      )
    }

    val cachedItems: Set[Atom] = Set(
      cache(pickRandomItemValue(), pickRandomNodeValue()),
      cache(pickRandomItemValue(), pickRandomNodeValue()),
      cache(pickRandomItemValue(), pickRandomNodeValue())
    )

    val qLevelsPerNodes = nodes collect {
      case n if random.nextDouble() <= 0.25 => if (random.nextDouble() <= 0.9) {
        (n, previousQualityLevels.getOrElse(n, pickQualityLevelValue()))
      } else {
        (n, pickQualityLevelValue())
      }
    }

    previousQualityLevels = previousQualityLevels ++ qLevelsPerNodes

    val qLevels = qLevelsPerNodes map (l => qLev(l._1, l._2))

    if (random.nextDouble() <= 1.0 / windowSize) {
      downUntil = downUntil updated(pickRandomNodeValue(), (1.5 * windowSize + t).toInt)
    }


    val downNodes = downUntil collect {
      case (n, time) if time <= t => down(n)
    }

    requestItems.flatten ++ cachedItems ++ qLevels ++ downNodes
  }

  override def verifyModel(optModel: Option[Model], t: Int): Unit = {

  }
}
