package engine

import scala.collection.mutable

/**
  * Created by FM on 10.04.16.
  */

// TODO: consider naming - stream or observable?
// + observable: use RxScala (they provide operators like filter, window/sample) - no need to reimplement
// + time/durations are first class
// - observable: might not be the right abstraction

// + stream: derive from Scala stream - operators are defined for free
// - time is not handled explicitly - only sequence of data
trait Collector {
  def append(evaluation: Evaluation)
}

trait Stream {
  def emit(collector: Collector)
}


trait Observable {
  def subscribe(observer: Observer)
}

trait Observer {
  def append(evaluation: Evaluation)
}

case class Evaluation(time: Time, atoms: Set[Atom])


object Stream {


  def fromItems(items: (Time, Set[Atom])*): Observable = {
    new ObservableList(items.map(x => Evaluation(x._1, x._2)))
  }

  def fromItem(items: (Time, Atom)*): Observable = {
    fromItems(items map (x => (x._1, Set(x._2))): _*)
  }
}

class ObservableList(private val items: Seq[Evaluation]) extends Observable {

  override def subscribe(observer: Observer): Unit = {
    items foreach observer.append
  }
}