package engine

import scala.collection.mutable

/**
  * Created by FM on 10.04.16.
  */

trait Collector {
  def append(time: Time, atom: Atom*)
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