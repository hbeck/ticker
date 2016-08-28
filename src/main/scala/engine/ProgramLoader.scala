package engine

import core._
import core.lars.{ExtendedAtom, HeadAtom, LarsRule, _}

/**
  * Created by FM on 28.08.16.
  */
object ProgramLoader {

  def atom(s: String): ExtendedAtom = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("(")) return noArgsAtom(str)
    val commasOnly = str.substring(0, str.size - 1).replace("(", ",")
    val seq: Seq[String] = commasOnly.split(",").toSeq
    atom(seq)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def atom(ss: Seq[String]): ExtendedAtom = {
    assert(ss.size > 0)
    if (ss.size == 1)
      noArgsAtom(ss(0))
    else {
      val s = ss(0)
      if (s.startsWith("w_")) {
        //convention for name of window atom: w_d_7_a(foo,X) says Time Window with size 7 Diamond a
        val parts = s.split("_")
        val temporalModality = parts(1) match {
          case "d" => Diamond
          case "b" => Box
          case x => throw new RuntimeException("window " + s + " atom cannot be parsed. unknown temporal modality " + x)
        }
        val size = Integer.parseInt(parts(2))
        val a = atom(parts(3)).asInstanceOf[Atom]
        WindowAtom(SlidingTimeWindow(size), temporalModality, a) //doesn't matter which one
      } else {
        val p = Predicate(s)
        val args = ss.tail map arg
        Atom(p, args)
      }
    }
  }

  def arg(s: String): Argument = if (s.charAt(0).isUpper) Variable(s) else Value(s)

  def noArgsAtom(s: String): GroundAtom = {
    val pred = Predicate(s)
    if (s.startsWith("xx"))
      ContradictionAtom(pred)
    else
      PredicateAtom(pred)

  }

  def fact(s: String): LarsRule = LarsFact(atom(s).asInstanceOf[Atom])

  def rule(all: String): LarsRule = {
    if (!all.contains(" :- ")) return fact(all)
    val hbStr = all.split(" :- ")
    val head: HeadAtom = atom(hbStr(0)).asInstanceOf[Atom] //not *A*tom! using atom to parse predicate symbol
    val bodyParts = hbStr(1).split(", ")
    val posBodyParts = bodyParts filterNot (_.trim.startsWith("not"))
    val negBodyParts = bodyParts filter (_.trim.startsWith("not"))
    val pos: Set[ExtendedAtom] = posBodyParts map (atom(_)) toSet
    val neg: Set[ExtendedAtom] = negBodyParts map (atom(_)) toSet

    LarsRule(head, pos, neg)
  }
}
