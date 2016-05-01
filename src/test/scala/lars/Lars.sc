import core.Atom
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

def prettyWindowFunction(windowFunction: WindowFunction) = windowFunction match {
  case SlidingTimeWindow(windowSize) => f"⊞^$windowSize"
}

def prettyTempOp(temporalOperator: TemporalOperator) = temporalOperator match {
  case Diamond => "☐"
  case Box => "◇"
  case At(time) => f"@_$time"
}
def prettyPrintWindow(atom: WindowAtom): String = {
  val parts = Seq(
    prettyWindowFunction(atom.windowFunction),
    prettyTempOp(atom.temporalOperator),
    atom.atom
  )
  parts mkString " "
}

prettyPrintWindow(op)

def prettyPrintAtom(atom: Formula): String = atom match {
  case w: WindowAtom => prettyPrintWindow(w)
  case a: Atom => a.toString
}

val r = Rule(c, Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), a), Set(b))
val r2 = Rule(c,
  Set(WindowAtom(SlidingTimeWindow(5), Box, b)),
  Set(WindowAtom(SlidingTimeWindow(3), Diamond, a), WindowAtom(SlidingTimeWindow(1), At(Time(3)), a))
)

//val r = c :- WindowAtom(SlidingTimeWindow(3), Diamond, a) and not(b)
//val r2 = c :- WindowAtom(SlidingTimeWindow(5), Box, b) and not(WindowAtom(SlidingTimeWindow(3), Diamond, a)) and not(WindowAtom(SlidingTimeWindow(1), At(Time(3)), a))

def prettyPrintRule(rule: Rule): String = {
  f"${rule.head} :- ${rule.pos map prettyPrintAtom mkString ","}${rule.neg map prettyPrintAtom mkString(", not ", ", not ", "")}. "
}

val program = Program(Set(r, r2))

def prettyPrint(program: Program): Set[String] = {
  program.rules map prettyPrintRule
}

prettyPrint(program) foreach println

