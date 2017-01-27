package clingo.reactive

import java.io._
import java.nio.charset.StandardCharsets

import clingo._
import core._
import core.lars.Assignment
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * Created by fm on 22/01/2017.
  */
class ReactiveClingoWrapperSpecs extends FlatSpec {

  val wrapper = ClingoWrapper("/Users/fm/Documents/diplom/iclingo/clingo-5.1.0-macos-10.9/clingo")

  val program =
    """#program signals_b_0(t,c).

      |#external at_b(t).
      |#external cnt_b(c).


      |#program volatile(t,c).

      |#external now(t).

      |a(t) :- wd_b(t).

      |wd_b(t) :- at_b(t), now(t).
      |wd_b(t) :- at_b(t-1), now(t).

    """.stripMargin


  "Building the interactive explicitly" should "work by specifing the relative path to the file" in {
    info("Needs a Running server")
    pending

    val inputStream = new ByteArrayInputStream(program.getBytes(StandardCharsets.UTF_8))
    val client = this.getClass.getResourceAsStream("/python/clingo/client-py.lp")

    val p = wrapper.clingoProcess.
      #<(new SequenceInputStream(inputStream, client))


    val r = p.lineStream_!

    val s = r.mkString

    assert(!s.isEmpty)
  }

  "Using the wrapper on an running server" should "work with reactive" in {
    info("Needs a Running server")
    pending
    val result = wrapper.runReactive(program)
  }

  def tick_now(count: Int = 1) = Tick(TickParameter("t"), count)

  def tick_cnt(count: Int = 1) = Tick(TickParameter("c"), count)

  "With explicitly connecting client" should "solve an asp program" in {
    info("Needs a connecting client")
    pending

    val server = ReactiveClingoClient.connect()

    server.sendSignal(Seq(ReactiveClingoSignal("b", Seq(), Seq(tick_now(1)))))
    val result = server.evaluate(Seq(tick_now(1)))

    assert(result.isDefined)
    assert(result.get.contains(Set("a(1)")))
  }

  "Starting client and server" should "lead to an asp model" in {
    val clingo = wrapper.runReactive(program)
    try {
      val server = ReactiveClingoClient.connect()

      server.sendSignal(Seq(ReactiveClingoSignal("b", Seq(), Seq(tick_now(1), tick_cnt(1)))))
      val result = server.evaluate(Seq(tick_now(1), tick_cnt(1)))

      assert(result.isDefined)
      assert(result.get.flatten.contains("a(1)"))

      server.terminate()

    } finally {

      clingo.destroy()
    }

  }

  "Explicitly starting reactive clingo" should "lead to an asp model" in {
    val reactiveClingo = new ReactiveClingo(wrapper)

    val X = Variable("X")
    val b = Atom(Predicate("b"), Seq(X))

    val p = Set(
      "a(t) :- wd_b(X,t).",
      "wd_b(X,t) :- at_b(X,t),now(t).",
      "wd_b(X,t) :- at_b(X,t-1),now(t)."
    )
    val reactiveProgram = ReactiveClingoProgram(p, Set(ClingoSignalAtom.fromAtom(b)))
    val runner = reactiveClingo.executeProgram(reactiveProgram)

    try {

      runner.evaluate(Seq(tick_now(1), tick_cnt(0)))
      val p = Seq(tick_now(2), tick_cnt(1))

      val groundSignal = b.assign(Assignment(Map(X -> Value("y"))))
      assert(groundSignal.isGround())
      
      runner.signal(Seq((groundSignal.asInstanceOf[GroundAtom], p)))


      val model = runner.evaluate(p)

      assert(model.isDefined)
      assert(!model.get.isEmpty)
      assert(model.get.flatten.contains("a(2)"))

    } finally {
      runner.terminate
    }
  }
}
