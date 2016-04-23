package engine.implementations

import asp.AspConversion
import core.{Atom, Fact}
import engine.{Atom, Time}

/**
  * Created by FM on 22.04.16.
  */
object StreamingAspTransformation {
  val now = Atom("now")

  def transform(time: Time, atoms: Set[Atom]) = {
    val nowAtT = now(time.milliseconds.toString)

    val atomFacts = (atoms + nowAtT) map (x => Fact(x))

    atomFacts map (x => AspConversion(x))
  }


}
