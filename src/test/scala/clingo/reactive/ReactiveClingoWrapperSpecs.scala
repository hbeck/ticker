package clingo.reactive

import java.io._
import java.nio.charset.StandardCharsets

import clingo.{ClingoWrapper, ReactiveClingoServer}
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * Created by fm on 22/01/2017.
  */
class ReactiveClingoWrapperSpecs extends FlatSpec {

  val wrapper = ClingoWrapper("/Users/fm/Documents/diplom/iclingo/clingo-5.1.0-macos-10.9/clingo")

  val program =
    """#program signals(t).

      #external b(t).


      #program volatile(t).

      #external now(t).

      a(t) :- wd_b(t).

      wd_b(t) :- b(t), now(t).
      wd_b(t) :- b(t-1), now(t).

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

  "With explicitly connecting client" should "solve an asp program" in {
    info("Needs a connecting client")
    pending

    val server = new ReactiveClingoServer()

    server.connect()

    server.sendTick(1)
    server.sendSignal(Seq("b"))
    val result = server.evaluate()

    assert(result.isDefined)
    assert(result.get.contains(Set("b(1)")))
  }

  "Starting client and server" should "lead to an asp model" in {
    val server = new ReactiveClingoServer()

    server.connect()

    wrapper.runReactive(program)

    server.sendTick(1)
    server.sendSignal(Seq("b"))
    val result = server.evaluate()

    assert(result.isDefined)
    assert(result.get.contains(Set("b(1)")))
  }

}
