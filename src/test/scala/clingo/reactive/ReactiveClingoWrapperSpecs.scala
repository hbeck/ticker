package clingo.reactive

import java.io._
import java.nio.charset.StandardCharsets

import clingo._
import core._
import core.lars._
import engine.asp.PlainLarsToAspMapper
import engine.asp.incremental.ReactiveEvaluationEngine
import org.scalatest.FlatSpec

/**
  * Created by fm on 22/01/2017.
  */
class ReactiveClingoWrapperSpecs extends FlatSpec {

  val wrapper = ClingoWrapper()

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

  def tick_now(value: Int = 1) = Tick(TickParameter("t"), value)

  def tick_cnt(value: Int = 1) = Tick(TickParameter("c"), value)

  def tickAt(now: Int, cnt: Int) = Seq(tick_now(now), tick_cnt(cnt))

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

  "Explicitly starting reactive clingo and evaluating 'a :- w^1 d b(X)' with {2 -> b(y) }" should "lead to 'a' at t=2,t=3 and not to a at t=4 " in {
    val reactiveClingo = new ReactiveClingo(wrapper)

    val X = Variable("X")
    val b = Atom(Predicate("b"), Seq(X))

    val p = Set(
      "a(t) :- wd_b(X,t).",
      "wd_b(X,t) :- at_b(X,t),now(t).",
      "wd_b(X,t) :- at_b(X,t-1),now(t)."
    )
    val reactiveProgram = ReactiveClingoProgram(p, Set(ClingoSignal.fromAtom(b)))
    val runner = reactiveClingo.executeProgram(reactiveProgram)

    try {

      runner.evaluate(tickAt(1, 0))

      val tickAt_2_1 = tickAt(2, 1)

      val groundSignal = b.assign(Assignment(Map(X -> Value("y"))))
      assert(groundSignal.isGround())

      val signal_2_1 = Seq((groundSignal.asInstanceOf[GroundAtom], tickAt_2_1))
      runner.signal(signal_2_1)

      def assertModel(model: Option[Set[ClingoModel]], time: Long) = {
        assert(model.isDefined)
        assert(!model.get.isEmpty)
        assert(model.get.flatten.contains(f"a($time)"))
      }

      assertModel(runner.evaluate(tickAt_2_1), 2)
      assertModel(runner.evaluate(tickAt(3, 1)), 3)

      runner.expire(signal_2_1)

      val model = runner.evaluate(tickAt(4, 1))

      assert(model.isDefined)
      assert(!model.get.isEmpty)
      assert(!model.get.flatten.contains("a(4)"))

    } finally {
      runner.terminate
    }
  }

  "With engine" should "work" in {
    val p = LarsProgram.from(
      Atom("a") <= WindowAtom(SlidingTimeWindow(2), Diamond, Atom("b"))
    )
    val mapper = PlainLarsToAspMapper()

    val mappedProgram = mapper(p)

    val engine = ReactiveEvaluationEngine(mappedProgram)

    try {

      engine.append(TimePoint(1))(Atom("b"))

      val model = engine.evaluate(TimePoint(3))

      /*
          a <- \window^2 \Diamond b

          Stream: 1 -> b
          t=3

          1 -> b        <-- answer stream
          3 -> a

          b_at(1)       <-- encoding of answer stream
          a_at(3) a

          a             <-- output stream (model)
       */

      assert(model.get contains (Atom("a")))
      //Some(Set(b_at(1), now(3), cnt(1))) did not contain a
    }
    finally {
      engine.terminate
    }

  }
}
