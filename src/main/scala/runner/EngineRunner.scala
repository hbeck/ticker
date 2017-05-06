package runner

import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import core.Atom
import core.lars.TimePoint
import engine.{EvaluationEngine, Result}

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

trait ConnectToEngine {

  def startWith(engineRunner: EngineRunner): Startable

}

/**
  * Created by FM on 10.11.16.
  */
case class EngineRunner(engine: EvaluationEngine, engineSpeed: Duration, outputSpeed: Duration) {

  type ResultCallback = (Result, TimePoint) => Unit

  private implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  var connectors: Seq[Startable] = List()

  var resultCallbacks: Seq[ResultCallback] = List()

  @volatile private var engineTimePoint: TimePoint = TimePoint(0)

  def convertToTimePoint(duration: Duration): TimePoint = Duration(duration.toMillis / engineSpeed.toMillis, engineSpeed.unit).length

  def convertOutput(timePoint: TimePoint) = Duration(timePoint.value * engineSpeed.toMillis / outputSpeed.toMillis, outputSpeed.unit)

  def convertInput(timePoint: TimePoint) = Duration(Duration(timePoint.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS).toUnit(engineSpeed.unit), engineSpeed.unit)

  private def updateBeat(): Unit = engineTimePoint = engineTimePoint + 1

  def evaluateModel(): Unit = {
    // capture engineTimePoint (threading!)
    val currentTimePoint = engineTimePoint

    val model = engine.evaluate(currentTimePoint)
    resultCallbacks.foreach(callback => callback(model, currentTimePoint))
  }

  def append(enteredTimePoint: Option[TimePoint], atoms: Seq[Atom]): Unit = {
    Future {
      val timePoint = enteredTimePoint.getOrElse(engineTimePoint)

      val inputTimePoint = convertInput(timePoint)

      println(f"Received input ${atoms.mkString(", ")} at T $inputTimePoint")

      engine.append(timePoint)(atoms: _*)
    }
  }

  def start(): Unit = {
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = updateBeat
    }, engineSpeed.toMillis, engineSpeed.toMillis)

    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = Future {
        evaluateModel()
      }
    }, outputSpeed.toMillis, outputSpeed.toMillis)

    connectors.foreach(startable => startable())

    // forces the caller thread to wait
    Thread.currentThread().join()
  }

  def connect(connect: ConnectToEngine): Unit = {
    connectors = connectors :+ connect.startWith(this)
  }

  def registerOutput(callback: ResultCallback): Unit = {
    resultCallbacks = resultCallbacks :+ callback
  }
}
