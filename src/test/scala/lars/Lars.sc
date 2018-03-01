import core.lars._
import core.{Atom, Not}


val a = Atom("a")
val b = Atom("b")
val c = Atom("c")

val op: WindowAtom = WindowAtom(TimeWindow(3), Diamond, a)

Format(op)

val r = UserDefinedLarsRule(c, Set(WindowAtom(TimeWindow(3), Diamond, a), a), Set(b))
val r2 = UserDefinedLarsRule(c,
  Set(WindowAtom(TimeWindow(5), Box, b)),
  Set(WindowAtom(TimeWindow(3), Diamond, a), WindowAtom(TimeWindow(1), At(TimePoint(3)), a))
)

val r1b = c <= WindowAtom(TimeWindow(3), Diamond, a) not (b)
val r2b = c <= WindowAtom(TimeWindow(5), Box, b) not (WindowAtom(TimeWindow(3), Diamond, a)) and not(WindowAtom(TimeWindow(1), At(TimePoint(3)), a))

val program = LarsProgram(Set(r, r2))

Format(program) foreach println

println(Format(r1b))

