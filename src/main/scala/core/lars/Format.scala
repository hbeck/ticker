package core.lars


import core.Atom

/**
  * Created by FM on 01.05.16.
  */
object Format {

  def apply(windowFunction: WindowFunction) = windowFunction match {
    case SlidingTupleWindow(windowSize) => f"⊞_#^$windowSize"
    case SlidingTimeWindow(windowSize) => f"⊞_t^$windowSize"
  }

  def apply(temporalOperator: TemporalModality) = temporalOperator match {
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
