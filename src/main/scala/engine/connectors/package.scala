package engine

import core.Atom
import core.lars.TimeUnit

import scala.concurrent.duration.Duration

package object connectors {
  val atomsSeparator = ';'


  def parseInput(inputUnit: TimeUnit)(line: String): (Option[Duration], Seq[Atom]) = {
    if (line.startsWith("@")) {
      val parts = line.split(':')
      (parseTime(inputUnit, parts(0)), parseAtoms(parts(1)))
    } else {
      (None, parseAtoms(line))
    }
  }

  def parseTime(inputUnit: TimeUnit, time: String) = time.trim.replace("@", "") match {
    case Int(x) => Some(Duration(x, inputUnit))
    case _ => None
  }

  def parseAtoms(atoms: String) = atoms.
    split(atomsSeparator).
    map(_.trim).
    map(Load.signal)

}
