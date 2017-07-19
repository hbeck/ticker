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
case class EngineRunner(engine: EvaluationEngine, engineSpeed: Duration, output: OutputEvery) {

  type ResultCallback = (Result, TimePoint) => Unit

  private implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  var connectors: Seq[Startable] = List()

  var resultCallbacks: Seq[ResultCallback] = List()

  @volatile private var engineTimePoint: TimePoint = TimePoint(0)

  def convertToTimePoint(duration: Duration): TimePoint = Duration(duration.toMillis / engineSpeed.toMillis, engineSpeed.unit).length

  def convertToInputSpeed(timePoint: TimePoint) = Duration(Duration(timePoint.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS).toUnit(engineSpeed.unit), engineSpeed.unit)

  private def updateBeat(): Unit = {
    engineTimePoint = engineTimePoint + 1
    output match {
      case t: Time if t.shouldUpdateWithNewData(engineTimePoint) => {
        var timePoint = engineTimePoint
        t.registerUpdate(timePoint)
        Future {
          evaluateModel(timePoint)
        }
      }
      case Diff => Future {
        evaluateModel(engineTimePoint)
      }
      case _ => /* noop*/
    }
  }

  // capture engineTimePoint (threading!)
  def evaluateModel(): Unit = evaluateModel(engineTimePoint)

  def evaluateModel(currentTimePoint: TimePoint): Unit = {
    val model = engine.evaluate(currentTimePoint)

    output match {
      case Diff  => {
        if (Diff.shouldUpdateWithNewData(model))
          publishModelChange()

        Diff.registerUpdate(model)
      }
      case _ => publishModelChange()
    }

    def publishModelChange() = resultCallbacks.foreach(callback => callback(model, currentTimePoint))
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

        println(f"Received input ${atoms.mkString(", ")} at T $inputTimePoint")

        engine.append(timePoint)(atoms: _*)

        output match {
          case Diff => evaluateModel()
          case s: Signal => {
            if (s.shouldUpdateWithNewData(atoms)) {
              evaluateModel()
            }
            s.registerUpdate(atoms)
          }
          case _ => /* noop */
        }
      }
    }
  }

  def start(): Unit = {
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = updateBeat
    }, engineSpeed.toMillis, engineSpeed.toMillis)
    if (output.isInstanceOf[Time]) {
      output.asInstanceOf[Time].registerEngineSpeed(engineSpeed)
    }
    //    output match {
    //      case Time(outputSpeed) => {
    //
    //        timer.scheduleAtFixedRate(new TimerTask {
    //          override def run(): Unit = Future {
    //            evaluateModel()
    //          }
    //        }, outputSpeed.toMillis, outputSpeed.toMillis)
    //      }
    //      case _ => /* noop */
    //    }


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
