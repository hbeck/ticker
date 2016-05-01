import core.{Atom, AspRule, not}
import core.lars._
import engine.{Stream, Time}

def SlidingTimeWindowFunction(windowSize: WindowSize)(stream: Stream, time: Time): Stream = {
  null
}

case class SlidingTimeWindow(windowSize: WindowSize) extends WindowFunction {
  def apply(stream: Stream, time: Time): Stream = {
    null
  }
}


val a = Atom("a")
val b = Atom("b")
val c = Atom("c")

val op: WindowAtom = WindowAtom(SlidingTimeWindow(3), Diamond, a)

def prettyPrintWindow(operator: WindowAtom) = operator match {
  case WindowAtom(SlidingTimeWindow(windowSize), Diamond, atom) => f"⊞^$windowSize ☐ $atom"
  case WindowAtom(SlidingTimeWindow(windowSize), Box, atom) => f"⊞^$windowSize ◇ $atom"
  case WindowAtom(SlidingTimeWindow(windowSize), At(time), atom) => f"⊞^$windowSize @_$time $atom"
}

prettyPrintWindow(op)

def prettyPrintAtom(atom: Formula): String = atom match {
  case w: WindowAtom => prettyPrintWindow(w)
  case a: Atom => a.toString
}

val r = Rule(c, Set(WindowAtom(SlidingTimeWindow(3), Diamond, a),a), Set(b))
val r2 = Rule(c,
  Set(WindowAtom(SlidingTimeWindow(5), Box, b)),
  Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), WindowAtom(SlidingTimeWindow(1), At(Time(3)), a))
)

//val r = c :- WindowAtom(SlidingTimeWindow(3), Diamond, a) and not(b)
//val r2 = c :- WindowAtom(SlidingTimeWindow(5), Box, b) and not(WindowAtom(SlidingTimeWindow(3), Diamond, a)) and not(WindowAtom(SlidingTimeWindow(1), At(Time(3)), a))

//r.body map prettyPrintAtom foreach println


def prettyPrint(rule: Rule) = {
  f"${rule.head} :- ${rule.pos map prettyPrintAtom mkString ","}${rule.neg map prettyPrintAtom mkString(", not ", ", not ", "")}. "
}

println(prettyPrint(r))

println(prettyPrint(r2))
//class SlidingTimeWindow extends WindowFunction {
//  def apply(windowSize: WindowSize)(stream: Stream, time: Time): Stream = {
//    null
//  }
//
//
//}