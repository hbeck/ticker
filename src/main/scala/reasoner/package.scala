import core.Atom
import core.lars.TimePoint
import reasoner.common.DefaultTrackedSignal

/**
  * Created by FM on 05.04.16.
  */
package object reasoner {

  // From an outside perspective we only want to pass in anonymous data
  //case class EngineAtom(name: String, arguments: Seq[String] = List())

  //single 'line' in experimental.evaluation function
  case class StreamEntry(time: TimePoint, atoms: Set[Atom])

  type Stream = Set[StreamEntry]

  type SignalStream = Set[DefaultTrackedSignal]

}



