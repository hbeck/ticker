package runner

import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import com.typesafe.scalalogging.Logger
import core.Atom
import core.lars.TimePoint
import engine.{Engine, Result}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait ConnectToEngine {

  def startWith(engineRunner: EngineRunner): Startable

}

/**
  * Created by FM on 10.11.16.
  */
case class EngineRunner(engine: Engine, clockTime: Duration, outputTiming: OutputTiming) {

  val logger = Logger[EngineRunner]

  type ResultCallback = (Result, TimePoint) => Unit

  private implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  var connectors: Seq[Startable] = List()

  var resultCallbacks: Seq[ResultCallback] = List()

  @volatile private var engineTimePoint: TimePoint = TimePoint(0)

  @volatile private var outputTracking: OutputTracking = outputTiming match {
    case Change => DiffTracking
    case Time(None) => TimeTracking(clockTime, clockTime)
    case Time(Some(interval)) => TimeTracking(interval, clockTime)
    case Signal(interval) => SignalTracking(interval)
  }

  def convertToTimePoint(duration: Duration): TimePoint = Duration(duration.toMillis / clockTime.toMillis, clockTime.unit).length

  def convertToInputSpeed(timePoint: TimePoint) = Duration(Duration(timePoint.value * clockTime.toMillis, TimeUnit.MILLISECONDS).toUnit(clockTime.unit), clockTime.unit)

  private def updateBeat(): Unit = {
    engineTimePoint = engineTimePoint + 1

    outputTracking match {
      case t: TimeTracking if wasUpdated(t, engineTimePoint) => {
        val timePoint = engineTimePoint
        Future {
          evaluateModel(timePoint)
        }
      }
      case DiffTracking => Future {
        evaluateModel(engineTimePoint)
      }
      case _ => /* noop*/
    }
  }

  // capture engineTimePoint (threading!)
  def evaluateModel(): Unit = evaluateModel(engineTimePoint)

  def evaluateModel(currentTimePoint: TimePoint): Unit = {
    val model = engine.evaluate(currentTimePoint)

    outputTracking match {
      case DiffTracking => {
        if (wasUpdated(DiffTracking, model))
          publishModelChange()
      }
      case _ => publishModelChange()
    }

    def publishModelChange() = resultCallbacks.foreach(callback => callback(model, currentTimePoint))
  }


  def wasUpdated[T](outputTrackingEvery: OutputTrackingEvery[T], model: T) = {
    val updated = outputTrackingEvery.shouldUpdateWithNewData(model) //TODO should != was
    outputTrackingEvery.registerUpdate(model)
    updated
  }

  def append(enteredTimePoint: Option[TimePoint], atoms: Seq[Atom]): Unit = {
    if (atoms.nonEmpty) {
      Future {
        // if we are not provided with an explicit user entered time-point
        // we add the atoms at the first time-point when they can be added in the engine
        // (which is determined by calling the code inside the future;
        // another strategy could be fixing the time-point at the moment when the atom arrives at the engine boundary,
        // but this would lead to adding atoms always in the past)
        val timePoint = enteredTimePoint.getOrElse(engineTimePoint)

        val inputTimePoint = convertToInputSpeed(timePoint)

        logger.debug(f"Received input ${atoms.mkString(", ")} at T $inputTimePoint")

        engine.append(timePoint)(atoms: _*)

        outputTracking match {
          case DiffTracking => evaluateModel()
          case s: SignalTracking => {
            if (wasUpdated(s, atoms)) {
              evaluateModel()
            }
          }
          case _ => /* noop */
        }
      }
    }
  }

  def start(): Unit = {
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = updateBeat()
    }, clockTime.toMillis, clockTime.toMillis)

    connectors.foreach(startable => new Thread(() => startable()).start())
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
