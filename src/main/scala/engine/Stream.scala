package engine

import core.Atom
import core.lars.TimePoint

/**
  * Created by FM on 10.04.16.
  */

// TODO: consider naming - stream or observable?
// + observable: use RxScala (they provide operators like filter, window/sample) - no need to reimplement
// + time/durations are first class
// - observable: might not be the right abstraction

// + stream: derive from Scala stream - operators are defined for free
// - time is not handled explicitly - only sequence of data


trait Observable {
  def subscribe(observer: Observer)
}

trait Observer {
  def append(evaluation: StreamEntry)
}




object Stream {


  def fromItems(items: (TimePoint, Set[Atom])*): Observable = {
    new ObservableList(items.map(x => StreamEntry(x._1, x._2)))
  }

  def fromItem(items: (TimePoint, Atom)*): Observable = {
    fromItems(items map (x => (x._1, Set(x._2))): _*)
  }
}

class ObservableList(private val items: Seq[StreamEntry]) extends Observable {

  override def subscribe(observer: Observer): Unit = {
    items foreach observer.append
  }
}