package runner

import java.util.concurrent.TimeUnit

import core.lars.{Box, Diamond, ExtendedAtom, HeadAtom, LarsFact, LarsProgram, LarsRule, SlidingTimeWindow, TimeWindowSize, WindowAtom, _}
import core.{Argument, Atom, AtomWithArgument, GroundAtom, IntValue, PinnedAtom, Predicate, PredicateAtom, StringValue, Value, Variable, _}
import runner.Load._
import unfiltered.util.Of.Int

import scala.concurrent.duration.Duration
import scala.io.{BufferedSource, Source}

/**
  * Created by FM on 19.11.16.
  */

object Load {
  def apply(): Load = Load(TimeUnit.SECONDS)

  def p(s: String) = Predicate(s)

  def strVal(s: String): Value = StringValue(s)

  def strVals(ss: String*): Set[Value] = ss map (StringValue(_)) toSet

  def strVals(ss: List[String]): Set[Value] = ss map (StringValue(_)) toSet

  def intVals(list: String*): Set[Value] = list map (IntValue(_)) toSet

  def v(s: String) = Variable(s)

  def arg(s: String): Argument = if (s.charAt(0).isUpper) Variable(s) else Value(s)

  def asInt(v: Value): Int = v match {
    case StringValue(s) => Integer.parseInt(s)
    case IntValue(i) => i
    case _ => throw new RuntimeException("argument %s cannot be casted to int".format(v))
  }

  def tokens(s: String): Seq[String] = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("("))
      return Seq(str)

    val commasOnly = str.substring(0, str.size - 1).replace("(", ",")

    commasOnly split (",") map (_.trim) toSeq
  }

  def signal(s: String): GroundAtom = {
    val token = tokens(s)

    assert(token.nonEmpty)

    val predicate = Predicate(token.head)
    val arguments = token.tail map arg

    assert(arguments forall (_.isInstanceOf[Value]))
    GroundAtom(predicate, arguments.map(_.asInstanceOf[Value]))
  }


}

case class Load(timeUnit: TimeUnit) {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def xatom(s: String): ExtendedAtom = {
    val token = tokens(s)
    if (token.tail.isEmpty)
      noArgsAtom(token.head)
    else
      xatom(token)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def xatom(ss: Seq[String]): ExtendedAtom = {
    assert(ss.size > 0)

    val atomName = ss.head
    val atomArguments = ss.tail

    if (atomArguments.isEmpty)
      noArgsAtom(atomName)
    else {
      if (atomName.startsWith("w_")) {
        windowAtom(atomName, atomArguments)
      } else if (atomName.startsWith("#")) {
        val p = Predicate(atomName)
        val args = atomArguments take atomArguments.size - 1 map arg
        val atom = Atom(p, args)
        val time = Integer.parseInt(atomArguments.last.toString)
        PinnedAtom(atom, time)
      } else {
        val p = Predicate(atomName)
        val args = atomArguments map arg
        Atom(p, args)
      }
    }
  }

  def windowAtom(atomName: String, arguments: Seq[String]) = {
    val parts = atomName.split("_")

    val windowParameter = parts(1).partition(_.isDigit)
    val window = windowParameter match {
      case (Int(timeInDefaultUnit), "") => SlidingTimeWindow(TimeWindowSize(timeInDefaultUnit, TimeUnit.SECONDS))
      case (Int(tupleCount), "t") => SlidingTupleWindow(tupleCount)
      case (Int(time), unit) => {
        val duration = Duration(time, unit)
        SlidingTimeWindow(TimeWindowSize(duration.length, duration.unit))
      }
      case _ => throw new RuntimeException("Could not parse " + windowParameter + " into a valid window")
    }

    val temporalModality = parts(2) match {
      case "d" => Diamond
      case "b" => Box
      case x => throw new RuntimeException("window " + atomName + " atom cannot be parsed. unknown temporal modality " + x)
    }
    val p = Predicate(parts(3))
    //    val args = arguments.tail filterNot (_ == p.caption) map arg
    WindowAtom(window, temporalModality, Atom(p, arguments map arg)) //doesn't matter which one
  }

  def noArgsAtom(s: String): ExtendedAtom = {
    val pred = Predicate(s)
    //if (s.startsWith("xx"))
    //  ContradictionAtom(pred)
    //else
    if (s.startsWith("w_"))
      windowAtom(s, Seq())
    else
      PredicateAtom(pred)
  }

  //
  //
  //

  def fact(s: String): LarsRule = LarsFact(xatom(s).asInstanceOf[Atom])

  def rule(all: String): LarsRule = {
    val s = all.replaceAll("\\.", "")
    if (!s.contains(" :- ")) return fact(s)
    val hbStr = s.split(" :- ")
    val head: HeadAtom = xatom(hbStr(0)).asInstanceOf[Atom] //not *A*tom! using atom to parse predicate symbol
    val bodyParts = hbStr(1).split(", ")
    val posBodyParts = bodyParts filterNot (_.trim.startsWith("not"))
    val negBodyParts = bodyParts filter (_.trim.startsWith("not"))
    val pos: Set[ExtendedAtom] = posBodyParts map (xatom(_)) toSet
    val neg: Set[ExtendedAtom] = negBodyParts map (xatom(_)) toSet

    LarsRule(head, pos, neg)
  }

  def groundSignal(s: String): LarsRule = {
    val aa: AtomWithArgument = xatom(s).asInstanceOf[AtomWithArgument]
    val timeArg = Integer.parseInt(aa.arguments.last.toString)
    val otherArgs = aa.arguments.take(aa.arguments.size - 1)
    LarsFact(PinnedAtom(AtomWithArgument(aa.predicate, otherArgs), timeArg))
  }


  //a(x,y) b(y,z) ==> use , only within atoms and use white space only to split atoms
  def parseSpaceSeparatedAtoms(s: String): Set[ExtendedAtom] = {
    s.split(" ") map (xatom(_)) toSet
  }

  def modelFromClingo(s: String): Model = {
    s.split(" ") map (xatom(_).asInstanceOf[Atom]) toSet
  }

  def readProgramFromFile(filename: String): LarsProgram = {
    readProgram(Source.fromURL(getClass.getResource(filename)))
  }

  def readProgram(source: BufferedSource): LarsProgram = {
    readProgram(source.getLines().toArray)
  }

  def readProgram(lines: Seq[String]): LarsProgram = {
    val validLines = lines map (_.trim) filter (_.nonEmpty)
    val rules = validLines map rule
    LarsProgram(rules.toList)
  }
}
