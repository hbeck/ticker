package core.lars


import core.Atom

/**
  * Created by FM on 01.05.16.
  */

object Format {
  def symbolic(program: LarsProgram) = SymbolicLars(program)

  def parsed(program: LarsProgram) = ParsedLars(program)

  def parsed(rules: Seq[LarsRule]) = ParsedLars(rules)
}

abstract class Format {

  def apply(windowFunction: WindowFunction): String

  def apply(windowSize: TimeWindowSize): String

  def apply(temporalOperator: TemporalModality): String

  def apply(atom: WindowAtom): String

  def apply(atom: Atom): String = {
    atom.toString
  }

  def apply(atom: ExtendedAtom): String = atom match {
    case w: WindowAtom => apply(w)
    case a: Atom => apply(a)
  }

  def apply(atom: HeadAtom): String = atom match {
    case a: Atom => apply(a)
    case at: AtAtom => apply(At(at.time)) + " " + at.atom.predicate
  }

  def apply(rule: LarsRule): String = {
    val head = apply(rule.head)
    val pos = rule.pos map apply mkString ", "
    val neg = rule.neg map apply mkString ", not "

    if (pos.nonEmpty && neg.nonEmpty)
      f"$head :- $pos, not $neg."
    else if (neg.nonEmpty)
      f"$head :- not $neg."
    else
      f"$head :- $pos."
  }

  def apply(rules: Seq[LarsRule]): Seq[String] = rules map apply

  def apply(program: LarsProgram): Seq[String] = apply(program.rules)
}

object SymbolicLars extends Format {
  def apply(windowFunction: WindowFunction): String = windowFunction match {
    case TupleWindow(windowSize) => f"⊞_#^$windowSize"
    case TimeWindow(windowSize) => f"⊞_t^${apply(windowSize)}"
  }

  def apply(windowSize: TimeWindowSize): String = f"[${windowSize.length} ${windowSize.unit.toString}]"

  def apply(temporalOperator: TemporalModality): String = temporalOperator match {
    case Diamond => "◇"
    case Box => "☐"
    case At(time) => f"@_$time"
  }

  def apply(atom: WindowAtom): String = {
    val parts = Seq(
      apply(atom.windowFunction),
      apply(atom.temporalModality),
      apply(atom.atom)
    )
    parts mkString " "
  }
}

object ParsedLars extends Format {
  def apply(atom: WindowAtom): String = {
    val parts = Seq(
      apply(atom.atom),
      apply(atom.temporalModality),
      apply(atom.windowFunction)
    )
    parts mkString " "
  }

  override def apply(windowFunction: WindowFunction): String = windowFunction match {
    case TimeWindow(windowSize) => f"[${apply(windowSize)}]"
    case TupleWindow(windowSize) => f"[$windowSize #]"
  }

  override def apply(windowSize: TimeWindowSize): String = f"${windowSize.length} ${windowSize.unit.toString}"

  override def apply(temporalOperator: TemporalModality): String = temporalOperator match {
    case Diamond => "in"
    case Box => "always in"
    case At(time) => f"@_$time"
  }
}