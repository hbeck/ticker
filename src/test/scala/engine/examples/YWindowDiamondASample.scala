package engine.examples

import core.{Atom, Program, not}
import engine.implementations.StreamingAspTransformation

/**
  * Created by FM on 23.04.16.
  */
class YWindowDiamondASample {
  val aspProgram =
    """y(T) :- w1d_a(T).

      w1d_a(T) :- a(U), now(T), U >= T - 1, U <= T.

      #show a/1.
      #show y/1.
    """

  val y = Atom("y")
  val w1d_a = Atom("w1d_a")
  val a = Atom("a")
  val u = Atom("u")

  val now = StreamingAspTransformation.now

  val program = Program(
    y("T") :- w1d_a("T"),
    w1d_a("T") :- a("U") and now("T") and u("U")
  )

}
