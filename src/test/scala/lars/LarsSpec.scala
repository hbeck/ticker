package lars

import core.Atom
import core.lars.{At, Program, _}
import engine.Time
import org.scalatest.FlatSpec

/**
  * Created by FM on 01.05.16.
  */
class LarsSpec extends FlatSpec {

  val a = Atom("a")
  val b = Atom("b")
  val c = Atom("c")
  val d = Atom("d")

  def W = WindowAtom

  def STW = SlidingTimeWindow

  val r1 = Rule(c, Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), d), Set(b))
  val r2 = Rule(AtAtom(Time(1), c), Set(W(STW(5), Box, b)), Set(W(STW(3), Diamond, a), W(STW(1), At(Time(3)), a)))

  val rb1 = c <= WindowAtom(SlidingTimeWindow(3), Diamond, a) and d not (b)
  val rb2 = AtAtom(Time(1), c) <= W(STW(5), Box, b) not W(STW(3), Diamond, a) not W(STW(1), At(Time(3)), a)

  def sw(time: Int, top: TemporalModality, a: Atom) = W(STW(time),top,a)

  val rc1 = c <= sw(3, Diamond, a) and d not (b)
  val rc2 = AtAtom(Time(1), c) <= sw(5, Box, b) not sw(3, Diamond, a) not sw(1, At(Time(3)), a)


  "A simple LARS program" should "be formatted as string" in {

    //val r = c :- WindowAtom(SlidingTimeWindow(3), Diamond, a) and not(b)
    //val r2 = c :- WindowAtom(SlidingTimeWindow(5), Box, b) and not(WindowAtom(SlidingTimeWindow(3), Diamond, a)) and not(WindowAtom(SlidingTimeWindow(1), At(Time(3)), a))

    val program = Program(Set(r1, r2))

    val formatted = Format(program)

    assert(formatted.size == 2)
    assert(formatted.head contains "c :-")
    assert(formatted.head contains "⊞")
    assert(formatted.head contains "◇")
    assert(formatted.head contains "not b")

    assert(formatted.last contains "@_1 c :-")
    assert(formatted.last contains "⊞^5 ☐")
    assert(formatted.last contains "not ⊞^1 @_3 a")
  }
}
