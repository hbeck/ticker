import core.Atom
import core.lars.TimePoint

/**
  * Created by FM on 05.04.16.
  */
package object engine {

  // TODO: discuss if now should also be moved into core.lars._
  val now = Atom("now")

  // From an outside perspective we only want to pass in anonymous data

  case class EngineAtom(name: String, arguments: Seq[String] = List())


  case class StreamEntry(time: TimePoint, atoms: Set[Atom])

  type Stream = Set[StreamEntry]
}



