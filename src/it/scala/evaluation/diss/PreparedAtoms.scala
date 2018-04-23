package evaluation.diss

import core._

/**
  * Created by hb on 05.04.18.
  */
object PreparedAtoms {

  val a: Atom = "a"
  val b: Atom = "b"

  val gX: Atom = "g(X)"
  val gY: Atom = "g(Y)"
  val gZ: Atom = "g(Z)"

  def b(i: Int, j: Int): Atom = "b("+i+","+j+")"
  def g(i: Int): Atom = "g("+i+")"

  implicit def string2Atom(atomStr: String): Atom = {
    if (atomStr.indexOf("(") == -1) return Atom(Predicate(atomStr))
    val tokens: Seq[String] = {
      atomStr.replace("("," ").
        replace(")","").
        replace(","," ").split(" ")
    }

    val predicate = Predicate(tokens(0))

    val arguments: Seq[Argument] = tokens.tail.map { s =>
      if (s.charAt(0).isLower) {
        StringValue(s)
      }
      else if (s.charAt(0).isUpper) {
        Variable(s)
      }
      else if (s.charAt(0).isDigit) {
        IntValue(s)
      }
      else {
        throw new RuntimeException("cannot parse " + s + " within atom " + atomStr)
      }
    }

    Atom(predicate,arguments)


  }

}
