package reasoner.parser

import core.lars._
import core._
import iclp.evaluation.instances.MMediaDeterministicEvalInst
import iclp2.evaluation2.instances.{CacheHopsEvalInst1, MMediaDeterministicEvalInst}
import org.scalatest.FunSuite

import scala.util.Random

/**
  * Created by et on 11.04.17.
  */
class LarsParserTest extends FunSuite {

  private val a = Atom("a")
  private val b = Atom("b")
  private val c = Atom("c")

  test("simple one-rule program") {
    val result: LarsProgram = LarsParser.fromString("a :- b.")
    assert(result == LarsProgram(Seq(LarsRule(a,Set(b),Set()))))
  }

  test("two-rule program with two body atoms") {
    val result: LarsProgram = LarsParser.fromString("a :- b,c. b:-a, c.")
    assert(result == LarsProgram(Seq(LarsRule(a,Set(b,c),Set()),LarsRule(b,Set(a,c),Set()))))
  }

  test("Left-hand operations with a variable") {
    val result: LarsProgram = LarsParser.fromString("a :- 3+5=T.")
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","T")),Set())))
    assert(result == check)
  }

  test("Left-hand operations") {
    val result: LarsProgram = LarsParser.fromString("a :- 3+5=8.")
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","8")),Set())))
    assert(result == check)
  }

  test("Right-hand operations") {
    val result: LarsProgram = LarsParser.fromString("a :- 8=3+5.")
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","8")),Set())))
    assert(result == check)
  }

  test("Two sided operations") {
    intercept[InvalidSyntaxException] {
      LarsParser.fromString("a :- 10-2=3+5.")
    }
  }

  test("Left-hand operations with variables") {
    val result: LarsProgram = LarsParser.fromString("a :- 3+5=A.")
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","A")),Set())))
    assert(result == check)
  }

  test("Right-hand operations with variables") {
    val result: LarsProgram = LarsParser.fromString("a :- A=3+5.")
    val check = LarsProgram(Seq(LarsRule(a,Set(Plus("3","5","A")),Set())))
    assert(result == check)
  }

  test("Program reasoner.incremental.jtms.experimental.evaluation.instances.CacheHopsEvalInst") {
    val program: LarsProgram = LarsParser("/parser-programs/cacheHopsEvalInst.lars")
    val cacheHops = CacheHopsEvalInst1(10000,10,0,Random)
    val cacheHopsProgram = cacheHops.larsProgram(300)
    assert(cacheHopsProgram.toString == program.toString)
  }

  test("Program reasoner.incremental.jtms.experimental.evaluation.instances.MMedia") {
    val program: LarsProgram = LarsParser("/parser-programs/MMedia.lars")
    val mmedia = MMediaDeterministicEvalInst(300,10000,Random)
    val mmediaProgram = mmedia.larsProgram(300)
    assert(mmediaProgram.toString == program.toString)
  }

  //

  def wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TimeWindow(windowSize), At(time), atom)
  def wD(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Diamond, atom)
  def wB(windowSize: Int, atom: Atom) = WindowAtom(TimeWindow(windowSize), Box, atom)
  def tup_wAt(windowSize: Int, time: Time, atom: Atom) = WindowAtom(TupleWindow(windowSize), At(time), atom)
  def tup_wD(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Diamond, atom)
  def tup_wB(windowSize: Int, atom: Atom) = WindowAtom(TupleWindow(windowSize), Box, atom)

  val I = StringVariable("I")
  val N = StringVariable("N")
  val M = StringVariable("M")
  val T = StringVariable("T")

  val hit = AtomWithArguments(Predicate("hit"),Seq(StringVariable("I"),StringVariable("N")))
  val item = AtomWithArguments(Predicate("item"),Seq(StringVariable("I")))
  val nodeN = AtomWithArguments(Predicate("node"),Seq(StringVariable("N")))
  val nodeM = AtomWithArguments(Predicate("node"),Seq(StringVariable("M")))
  val req = AtomWithArguments(Predicate("req"),Seq(StringVariable("I"),StringVariable("N")))
  val time = AtomWithArguments(Predicate("time"),Seq(StringVariable("T")))

  // time window

  test("w-time diamond") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,wD(300,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) [300 sec].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,wD(300,req)),Set())))
    assert(result == check)
  }

  test("w-time box") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) always in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,wB(300,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) always [300 sec].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,wB(300,req)),Set())))
    assert(result == check)
  }

  test("w-time at") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), req(I,N) at T in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,wAt(300,T,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), req(I,N) at T [300 sec].")
    check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,wAt(300,T,req)),Set())))
    assert(result == check)
  }

  test("at (no window)") {
    val result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), req(I,N) at T.")
    val check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,AtAtom(T,req)),Set())))
    assert(result == check)
  }

  // time window negated

  test("neg: w-time diamond") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(wD(300,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) [300 sec].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(wD(300,req)))))
    assert(result == check)
  }

  test("neg: w-time box") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) always in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(wB(300,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) always [300 sec].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(wB(300,req)))))
    assert(result == check)
  }

  test("neg: w-time at") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), time(T), not req(I,N) at T in [300 sec].")
    var check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,time),Set(wAt(300,T,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), time(T), not req(I,N) at T [300 sec].")
    check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,time),Set(wAt(300,T,req)))))
    assert(result == check)
  }

  test("neg: at (no window)") {
    val result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), time(T), not req(I,N) at T.")
    val check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,time),Set(AtAtom(T,req)))))
    assert(result == check)
  }

  // tuple window

  test("w-tuple diamond") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) in [300 #].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,tup_wD(300,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) [300 #].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,tup_wD(300,req)),Set())))
    assert(result == check)
  }

  test("w-tuple box") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) always in [300 #].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,tup_wB(300,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), req(I,N) always [300 #].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,tup_wB(300,req)),Set())))
    assert(result == check)
  }

  test("w-tuple at") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), req(I,N) at T in [300 #].")
    var check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,tup_wAt(300,T,req)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), req(I,N) at T [300 #].")
    check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,tup_wAt(300,T,req)),Set())))
    assert(result == check)
  }

  // time window negated

  test("neg: w-tuple diamond") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) in [300 #].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(tup_wD(300,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) [300 #].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(tup_wD(300,req)))))
    assert(result == check)
  }

  test("neg: w-tuple box") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) always in [300 #].")
    var check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(tup_wB(300,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), not req(I,N) always [300 #].")
    check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN),Set(tup_wB(300,req)))))
    assert(result == check)
  }

  test("neg: w-tuple at") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), time(T), not req(I,N) at T in [300 #].")
    var check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,time),Set(tup_wAt(300,T,req)))))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) at T :- item(I), node(N), time(T), not req(I,N) at T [300 #].")
    check = LarsProgram(Seq(LarsRule(AtAtom(T,hit),Set(item,nodeN,time),Set(tup_wAt(300,T,req)))))
    assert(result == check)
  }

  // relation atoms

  test("Rel: eq") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I = N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Eq(I,N)),Set())))
    assert(result == check)

    LarsParser.fromString("hit(I,N) :- item(I), node(N), I=N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), eq(I,N).")
    assert(result == check)
  }

  test("Rel: neq") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I != N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Neq(I,N)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), I!=N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), neq(I,N).")
    assert(result == check)
  }

  test("Rel: leq") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I <= N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Leq(I,N)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), I<=N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), leq(I,N).")
    assert(result == check)
  }

  test("Rel: geq") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I >= N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Geq(I,N)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), I>=N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), geq(I,N).")
    assert(result == check)
  }

  test("Rel: lt") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I < N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Lt(I,N)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), I<N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), lt(I,N).")
    assert(result == check)
  }

  test("Rel: gt") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), I > N.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,Gt(I,N)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), I>N.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), gt(I,N).")
    assert(result == check)
  }

  test("Rel: plus") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N + M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Plus(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N+M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N + M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), plus(N,M,I).")
    assert(result == check)
  }

  test("Rel: minus") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N - M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Minus(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N-M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N - M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), minus(N,M,I).")
    assert(result == check)
  }

  test("Rel: times") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N * M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Times(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N*M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N * M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), times(N,M,I).")
    assert(result == check)
  }

  test("Rel: divide") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N / M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Divide(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N/M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N / M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), divide(N,M,I).")
    assert(result == check)
  }

  test("Rel: modulo") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N % M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Modulo(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N%M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N % M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), modulo(N,M,I).")
    assert(result == check)
  }

  test("Rel: power") {
    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I = N ^ M.")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Power(N,M,I)),Set())))
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), I=N^M.")
    assert(result == check)

    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N ^ M = I.")
    assert(result == check)

    //low-level variant (direct)
    result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), power(N,M,I).")
    assert(result == check)
  }

  test("Rel: leqleq") {
//    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- node(N), node(M), I = N .. M")
//    val check = LarsProgram(Seq(LarsRule(hit,Set(nodeN,nodeM,LeqLeq(N,I,M)),Set())))
//    assert(result == check)
//
//    result = LarsParser.fromString("hit(I,N) :- node(N), node(M), I=N..M")
//    assert(result == check)

    //only low-level variant available
    val result = LarsParser.fromString("hit(I,N) :- node(N), node(M), leqleq(N,I,M).")
    val check = LarsProgram(Seq(LarsRule(hit,Set(nodeN,nodeM,LeqLeq(N,I,M)),Set())))
    assert(result == check)
  }

  test("Rel: ltlt") {
    //only low-level variant available
    val result: LarsProgram = LarsParser.fromString("hit(I,N) :- node(N), node(M), ltlt(N,I,M).")
    val check = LarsProgram(Seq(LarsRule(hit,Set(nodeN,nodeM,LtLt(N,I,M)),Set())))
    assert(result == check)
  }

  test("Rel: ltleq") {
    //only low-level variant available
    val result: LarsProgram = LarsParser.fromString("hit(I,N) :- node(N), node(M), ltleq(N,I,M).")
    val check = LarsProgram(Seq(LarsRule(hit,Set(nodeN,nodeM,LtLeq(N,I,M)),Set())))
    assert(result == check)
  }

  test("Rel: leqlt") {
    //only low-level variant available
    val result: LarsProgram = LarsParser.fromString("hit(I,N) :- node(N), node(M), leqlt(N,I,M).")
    val check = LarsProgram(Seq(LarsRule(hit,Set(nodeN,nodeM,LeqLt(N,I,M)),Set())))
    assert(result == check)
  }

  test("Rel: incr") {
//    var result: LarsProgram = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), N = M + 1.")
//    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Incr(N,M)),Set())))
//    assert(result == check)

    //only low-level variant available
    val result = LarsParser.fromString("hit(I,N) :- item(I), node(N), node(M), incr(N,M).")
    val check = LarsProgram(Seq(LarsRule(hit,Set(item,nodeN,nodeM,Incr(N,M)),Set())))
    assert(result == check)
  }

}
