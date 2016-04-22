package engine.implementations

import engine.{Atom, Time}

/**
  * Created by FM on 22.04.16.
  */
object StreamingAspTransformation {
  def transform(time: Time, atoms: Set[Atom]) = {
    val now = f"now(${time.milliseconds})."
    val atomFacts = atoms map (x => f"${x.toString}.")
    atomFacts + now
  }


}
