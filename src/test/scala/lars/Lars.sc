import core.lars._
import core.{Atom, not}


val a = Atom("a")
val b = Atom("b")
val c = Atom("c")

val op: WindowAtom = WindowAtom(SlidingTimeWindow(3), Diamond, a)

Format(op)

val r = LarsRule(c, Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), a), Set(b))
val r2 = LarsRule(c,
  Set(WindowAtom(SlidingTimeWindow(5), Box, b)),
  Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), WindowAtom(SlidingTimeWindow(1), At(TimePoint(3)), a))
)

val r1b = c <= WindowAtom(SlidingTimeWindow(3), Diamond, a) not (b)
val r2b = c <= WindowAtom(SlidingTimeWindow(5), Box, b) not (WindowAtom(SlidingTimeWindow(3), Diamond, a)) and not(WindowAtom(SlidingTimeWindow(1), At(TimePoint(3)), a))

val program = LarsProgram(Set(r, r2))

Format(program) foreach println

println(Format(r1b))

