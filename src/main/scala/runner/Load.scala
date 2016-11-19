package runner

import java.util.concurrent.TimeUnit

import core.{Argument, Atom, AtomWithArgument, GroundAtom, IntValue, PinnedAtom, Predicate, PredicateAtom, StringValue, Value, Variable, _}
import core.asp.{AspProgram, UserDefinedAspRule, _}
import core.lars.{Box, Diamond, ExtendedAtom, Grounder, HeadAtom, LarsFact, LarsProgram, LarsRule, SlidingTimeWindow, TimeWindowSize, WindowAtom, _}
import jtms.evaluation.Util._

import scala.concurrent.duration.Duration
import scala.io.{BufferedSource, Source}

/**
  * Created by FM on 19.11.16.
  */
object Load {

  //"a(x,Y)" ==> Seq("a","x","Y")
  def xatom(s: String): ExtendedAtom = {
    val str = if (s.startsWith("not ")) s.substring(4) else s
    if (!str.contains("(")) return noArgsAtom(str)
    val commasOnly = str.substring(0, str.size - 1).replace("(", ",")
    val seq: Seq[String] = commasOnly.split(",").toSeq
    xatom(seq)
  }

  //Seq("a","x","Y") ==> NonGroundAtom(a,{x,Y})
  def xatom(ss: Seq[String]): ExtendedAtom = {
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
        val windowSize = parts(2).forall(_.isDigit) match {
          case true => Duration(Integer.parseInt(parts(2)), TimeUnit.SECONDS)
          case false => {
            val sizeParts = parts(2).partition(_.isDigit)
            Duration(Integer.parseInt(sizeParts._1).toLong, sizeParts._2)
          }
        }
        val p = Predicate(parts(3))
        val args = ss.tail filterNot (_ == p.caption) map arg
        WindowAtom(SlidingTimeWindow(TimeWindowSize(windowSize.length, windowSize.unit)), temporalModality, Atom(p, args)) //doesn't matter which one
      } else if (s.startsWith("#")) {
        val p = Predicate(s)
        val args = ss.tail take ss.tail.size - 1 map arg
        val atom = Atom(p, args)
        val time = Integer.parseInt(ss.tail.last.toString)
        PinnedAtom(atom, time)
      } else {
        val p = Predicate(s)
        val args = ss.tail map arg
        Atom(p, args)
      }
    }
  }


  def noArgsAtom(s: String): GroundAtom = {
    val pred = Predicate(s)
    //if (s.startsWith("xx"))
    //  ContradictionAtom(pred)
    //else

    PredicateAtom(pred)
  }

  //
  //
  //

  def p(s: String) = Predicate(s)

  def strVal(s: String): Value = StringValue(s)

  def strVals(ss: String*): Set[Value] = ss map (StringValue(_)) toSet

  def strVals(ss: List[String]): Set[Value] = ss map (StringValue(_)) toSet

  def intVals(list: String*): Set[Value] = list map (IntValue(_)) toSet

  def v(s: String) = Variable(s)

  def arg(s: String): Argument = if (s.charAt(0).isUpper) Variable(s) else Value(s)

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

  def program(rules: LarsRule*): LarsProgram = LarsProgram(rules)

  def ground(p: LarsProgram) = Grounder(p).groundProgram

  //a(x,y) b(y,z) ==> use , only within atoms and use white space only to split atoms
  def parseSpaceSeparatedAtoms(s: String): Set[ExtendedAtom] = {
    s.split(" ") map (xatom(_)) toSet
  }

  //use only for asp fragment!
  def asAspProgram(larsProgram: LarsProgram): NormalProgram = {
    val aspRules: Seq[NormalRule] = larsProgram.rules map (asAspRule(_))
    AspProgram(aspRules.toList)
  }

  def asAspRule(larsRule: LarsRule): NormalRule = {
    val head = larsRule.head.atom //we are not using @ here
    val pos = larsRule.pos map (_.asInstanceOf[Atom])
    val neg = larsRule.neg map (_.asInstanceOf[Atom])
    UserDefinedAspRule(head, pos, neg)
  }

  def modelFromClingo(s: String): Model = {
    s.split(" ") map (xatom(_).asInstanceOf[Atom]) toSet
  }

  def asInt(v: Value): Int = v match {
    case StringValue(s) => Integer.parseInt(s)
    case IntValue(i) => i
    case _ => throw new RuntimeException("argument %s cannot be casted to int".format(v))
  }

  def readProgramFromFile(filename: String): LarsProgram = {
    readProgram(Source.fromURL(getClass.getResource(filename)))
  }

  def readProgram(source: BufferedSource): LarsProgram = {

    val rules = source.getLines().toSeq map rule
    LarsProgram(rules)
  }
}
