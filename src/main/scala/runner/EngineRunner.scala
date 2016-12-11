package runner

import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import core.{Atom, Model}
import core.lars.TimePoint
import engine.{EvaluationEngine, NoResult, Result}

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

  @volatile private var ticks: TimePoint = TimePoint(0)

  def convertToTicks(duration: Duration): TimePoint = Duration(duration.toMillis / engineSpeed.toMillis, engineSpeed.unit).length

  def convertTicksToOutput(tick: TimePoint) = Duration(tick.value * engineSpeed.toMillis / outputSpeed.toMillis, outputSpeed.unit)

  def convertTicksToInputSpeed(tick: TimePoint) = Duration(Duration(tick.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS).toUnit(engineSpeed.unit), engineSpeed.unit)

  private def updateTicks(): Unit = ticks = ticks + 1

  def evaluateModel(): Unit = {
    // capture ticks (threading!)
    val currentTicks = ticks

    val model = engine.evaluate(currentTicks)
    resultCallbacks.foreach(callback => callback(model, currentTicks))
  }

  def append(enteredTick: Option[TimePoint], atoms: Seq[Atom]): Unit = {
    // TODO: discuss which time to use
    // if we are not provided with a user entered time-point
    // (capture ticks here or inside future (which is usually later))

    Future {
      val tick = enteredTick.getOrElse(ticks)

      val inputTicks = convertTicksToInputSpeed(tick)

      println(f"Received input ${atoms.mkString(", ")} at T $inputTicks")

      engine.append(tick)(atoms: _*)
    }
  }

  def start(): Unit = {
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = updateTicks
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
