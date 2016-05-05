package engine

import core.asp.AspRule
import core.{AtomWithArguments, Atom}
import core.lars._
import engine.implementations.StreamingAspTransformation

/**
  * Created by FM on 05.05.16.
  */
object TransformLars {

  val now = StreamingAspTransformation.now
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
    case w: WindowAtom => Some(ruleForBox(w))
    case _ => None
  }

  def ruleForBox(windowAtom: WindowAtom): AspRule = {
    val windowSize = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => size
    }

    val generateAtoms = (1 to windowSize) map (T + "-" + _) map (windowAtom.atom(_))

    val posBody = generateAtoms ++ Set(TransformLars(now), TransformLars(windowAtom.atom))

    AspRule(Atom(nameFor(windowAtom)), posBody.toSet)
  }
}
