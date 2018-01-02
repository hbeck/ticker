package runner

import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import com.typesafe.scalalogging.Logger
import core.Atom
import core.lars.{ClockTime, TimePoint}
import reasoner.{Reasoner, Result}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait ConnectToEngine {

  def startWith(engineRunner: Engine): Startable

}

/**
  * Created by FM on 10.11.16.
  */
case class Engine(reasoner: Reasoner, clockTime: ClockTime, outputTiming: OutputTiming) {

  val logger = Logger[Engine]

  type ResultCallback = (Result, TimePoint) => Unit

  private implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  var connectors: Seq[Startable] = List()

  var resultCallbacks: Seq[ResultCallback] = List()

  @volatile private var engineTimePoint: TimePoint = TimePoint(0)

  @volatile private var outputWriting: OutputWriting = outputTiming match {
    case Change => ChangeBasedWriting
    case Time(None) => TimeBasedWriting(clockTime, clockTime)
    case Time(Some(interval)) => TimeBasedWriting(interval, clockTime)
    case Signal(interval) => SignalBasedWriting(interval)
  }

  def convertToTimePoint(duration: Duration): TimePoint = Duration(duration.toMillis / clockTime.toMillis, clockTime.unit).length

  def convertToInputSpeed(timePoint: TimePoint) = Duration(Duration(timePoint.value * clockTime.toMillis, TimeUnit.MILLISECONDS).toUnit(clockTime.unit), clockTime.unit)

  private def updateClock(): Unit = {
    engineTimePoint = engineTimePoint + 1

    outputWriting match {
      case timeBasedWriting: TimeBasedWriting if shouldWrite(timeBasedWriting, engineTimePoint) => {
        val timePoint = engineTimePoint
        Future {
          evaluateModel(timePoint)
        }
      }
      case `ChangeBasedWriting` => Future { //TODO hb why not test (i) shouldWrite here, and (ii) other modes (cases)?
        evaluateModel(engineTimePoint)
      }
      case _ => /* noop*/
    }
  }

  // capture engineTimePoint (threading!)
  def evaluateModel(): Unit = evaluateModel(engineTimePoint)

  def evaluateModel(currentTimePoint: TimePoint): Unit = {
    val model = reasoner.evaluate(currentTimePoint)

    outputWriting match {
      case ChangeBasedWriting => {
        if (shouldWrite(ChangeBasedWriting, model))
          publishModel()
      }
      case _ => publishModel() //TODO other modes? shouldWrite?
    }

    def publishModel() = resultCallbacks.foreach(callback => callback(model, currentTimePoint))
  }


  def shouldWrite[TUpdate](outputWriting: OutputWritingEvery[TUpdate], update: TUpdate) = {
    val shouldWrite = outputWriting.shouldWriteAfterUpdate(update)
    outputWriting.registerUpdate(update)
    shouldWrite
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

        reasoner.append(timePoint)(atoms: _*)

        outputWriting match {
          case ChangeBasedWriting => evaluateModel()
          case s: SignalBasedWriting => {
            if (shouldWrite(s, atoms)) {
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
      override def run(): Unit = updateClock()
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
