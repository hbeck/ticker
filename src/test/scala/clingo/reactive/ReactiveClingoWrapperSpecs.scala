package clingo.reactive

import java.io._
import java.nio.charset.StandardCharsets

import clingo._
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * Created by fm on 22/01/2017.
  */
class ReactiveClingoWrapperSpecs extends FlatSpec {

  val wrapper = ClingoWrapper("/Users/fm/Documents/diplom/iclingo/clingo-5.1.0-macos-10.9/clingo")

  val program =
    """#program signals(t).

      |#external b(t).


      |#program volatile(t).

      |#external now(t).

      |a(t) :- wd_b(t).

      |wd_b(t) :- b(t), now(t).
      |wd_b(t) :- b(t-1), now(t).

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

  val tick_now_1 = TickValue(TickAtom("now"), 1)

  "With explicitly connecting client" should "solve an asp program" in {
    info("Needs a connecting client")
    pending

    val server = new ReactiveClingoClient()

    server.connect()

    server.sendTick(Seq(tick_now_1))
    server.sendSignal(Seq("b"))
    val result = server.evaluate()

    assert(result.isDefined)
    assert(result.get.contains(Set("b(1)")))
  }

  "Starting client and server" should "lead to an asp model" in {
    val clingo = wrapper.runReactive(program)
    try {
      val server = new ReactiveClingoClient()

      server.connect()

      server.sendTick(Seq(tick_now_1))
      server.sendSignal(Seq("b"))
      val result = server.evaluate()

      assert(result.isDefined)
      assert(result.get.flatten.contains("b(1)"))

    } finally {

      clingo.destroy()
    }

  }

  "Explicitly starting reactive clingo" should "lead to an asp model" in {
    val reactiveClingo = new ReactiveClingo(wrapper)

    val p = Set(
      "a(t) :- wd_b(X,t).",
      "wd_b(X,t) :- b(X,t),now(t).",
      "wd_b(X,t) :- b(X,t-1),now(t)."
    )
    val reactiveProgram = ReactiveClingoProgram(p, Set())
    val runner = reactiveClingo.executeProgram(reactiveProgram)

    try {

      runner.ticks(Seq(TickValue(TickAtom("now"), 2), TickValue(TickAtom("cnt"), 1)))
      runner.signal(Seq("b(y,2)"))
      runner.ticks(Seq(TickValue(TickAtom("now"), 3), TickValue(TickAtom("cnt"), 1)))

      val model = runner.evaluate

      assert(model.isDefined)
      assert(!model.get.isEmpty)
      assert(model.get.flatten.contains("a(1)"))

    } finally {
      runner.terminate
    }
  }
}
