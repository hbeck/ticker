package engine.examples

import core.{Atom, Program, not}
import engine.implementations.StreamingAspTransformation
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.04.16.
  */
class XWindowBoxASample extends FlatSpec {
  val aspProgram =
    """x(T) :- w1b_a(T).

       w1b_a(T) :- now(T), not spoil_w1b_a(T).
       spoil_w1b_a(T) :- reach_w1b_a(U,T), not a(U).
       reach_w1b_a(U,T) :- now(T), U=T-1..T.
    """

  val x = Atom("x")
  val w1b_a = Atom("w1b_a")
  val spoil_w1b_a = Atom("spoil_w1b_a")
  val a = Atom("a")
  val u = Atom("u")

  val now = StreamingAspTransformation.now

  val program = Program(
    x("T") :- w1b_a("T"),
    w1b_a("T") :- now("T") and not(spoil_w1b_a("T")),
    spoil_w1b_a("T") :- now("T") and u("U") and not(a("U"))
  )


  // add u(T-1) and u(T)
}
