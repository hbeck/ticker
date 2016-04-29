import core.{Atom, Rule, not}
import core.lars.{TemporalOperator, WinOp, WindowFunction, WindowSize}
import engine.{Stream, Time}

def SlidingTimeWindowFunction(windowSize: WindowSize)(stream: Stream, time: Time): Stream = {
  null
}

case class SlidingTimeWindow(windowSize: WindowSize) extends WindowFunction {
  def apply(stream: Stream, time: Time): Stream = {
    null
  }
}

//def Diamond(stream: Stream, time: Time, atom: Atom): Boolean = {
//  true
//}

object Diamond extends TemporalOperator {
  def apply(stream: Stream, atom: Atom): Boolean = {
    stream exists (_.atoms.contains(atom))
  }
}

object Box extends TemporalOperator {
  override def apply(stream: Stream, atom: Atom): Boolean = {
    stream forall (_.atoms.contains(atom))
  }
}

case class At(time: Time) extends TemporalOperator {
  override def apply(stream: Stream, atom: Atom): Boolean = {
    stream filter (_.time == time) exists (_.atoms.contains(atom))
  }
}


val a = Atom("a")
val b = Atom("b")
val c = Atom("c")

val op: WinOp = WinOp(SlidingTimeWindow(3), Diamond, a)

def prettyPrintWindow(operator: WinOp) = operator match {
  case WinOp(SlidingTimeWindow(windowSize), Diamond, atom) => f"⊞^$windowSize ☐ $atom"
  case WinOp(SlidingTimeWindow(windowSize), Box, atom) => f"⊞^$windowSize ◇ $atom"
  case WinOp(SlidingTimeWindow(windowSize), At(time), atom) => f"⊞^$windowSize @_$time $atom"
}

prettyPrintWindow(op)

def prettyPrintAtom(atom: Atom): String = atom match {
  case w: WinOp => prettyPrintWindow(w)
  case a: Atom => a.toString
}


val r = c :- WinOp(SlidingTimeWindow(3), Diamond, a) and not(b)
val r2 = c :- WinOp(SlidingTimeWindow(5), Box, b) and not(WinOp(SlidingTimeWindow(3), Diamond, a)) and not(WinOp(SlidingTimeWindow(1),At(Time(3)),a))

r.body map prettyPrintAtom foreach println


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