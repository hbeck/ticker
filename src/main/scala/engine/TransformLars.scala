package engine

import core.Atom
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
    Atom(nameFor(windowAtom))(T)
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
    f"${windowFunction}_${operator}_${window.atom}"
  }
}
