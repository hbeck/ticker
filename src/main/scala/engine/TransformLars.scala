package engine

import core.asp.AspRule
import core.{AtomWithArguments, Atom}
import core.lars._

/**
  * Created by FM on 05.05.16.
  */
object TransformLars {


  val T = "T"

  def apply(atom: Atom): Atom = {
    atom(T)
  }

  def apply(atAtom: AtAtom): Atom = {
    atAtom.atom(atAtom.time.toString)
  }

  def apply(windowAtom: WindowAtom) = {
    val arguments = windowAtom.atom match {
      case AtomWithArguments(_, arguments) => arguments :+ T
      case a: Atom => Seq(T)
    }
    Atom(nameFor(windowAtom))(arguments: _*)
  }

  def nameFor(window: WindowAtom) = {
    val windowFunction = window.windowFunction match {
      case SlidingTimeWindow(size) => f"w_$size"
    }
    val operator = window.temporalOperator match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = window.atom match {
      case AtomWithArguments(atom, _) => atom.toString
      case a: Atom => a.toString
    }
    f"${windowFunction}_${operator}_${atomName}"
  }

  def ruleFor(extendedAtom: ExtendedAtom): Option[AspRule] = extendedAtom match {
    case w: WindowAtom => Some(AspRule(Atom("foo")))
    case _ => None
  }
}
