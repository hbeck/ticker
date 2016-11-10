import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import core.Atom
import core.lars.TimePoint
import engine.{EvaluationEngine, NoResult, Result}
import unfiltered.util.Of.Int

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
  * Created by FM on 10.11.16.
  */
case class EngineRunner(engine: EvaluationEngine, engineSpeed: Duration, outputSpeed: Duration) {

  implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  @volatile var ticks: TimePoint = TimePoint(0)

  @volatile var lastModel: Result = NoResult

  def convertToTicks(duration: Duration): TimePoint = Duration(duration.toMillis / engineSpeed.toMillis, engineSpeed.unit).length

  def convertTicksToOutput(tick: TimePoint) = Duration(tick.value * engineSpeed.toMillis / outputSpeed.toMillis, outputSpeed.unit)

  def convertTicksToInputSpeed(tick: TimePoint) = Duration(Duration(tick.value * engineSpeed.toMillis, TimeUnit.MILLISECONDS).toUnit(engineSpeed.unit), engineSpeed.unit)

  def updateTicks(): Unit = ticks = ticks + 1

  def evaluateModel(): Unit = {

    val model = engine.evaluate(ticks)
    if (model.get != lastModel.get) {
      val timeInOutput = convertTicksToOutput(ticks)
      model.get match {
        case Some(m) => println(f"Model at T $timeInOutput: $m")
        case None => println(f"No model at T $timeInOutput")
      }
      lastModel = model
    }
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
  }

  def receiveInputFromStdIn(inputUnit: TimeUnit): Unit = {
    val parser = Input(inputUnit)

    Iterator.continually(scala.io.StdIn.readLine).
      map(parser.parseInput).
      takeWhile(_._2.nonEmpty).
      foreach(input => append(input._1, input._2))
  }

  case class Input(inputUnit: TimeUnit) {

    def parseInput(line: String): (Option[TimePoint], Seq[Atom]) = {
      if (line.startsWith("@")) {
        val parts = line.split(':')
        (parseTime(parts(0)), parseAtoms(parts(1)))
      } else {
        (None, parseAtoms(line))
      }
    }

    def parseTime(time: String) = time.trim.replace("@", "") match {
      case Int(x) => Some(convertToTicks(Duration(x, inputUnit)))
      case _ => None
    }

    def parseAtoms(atoms: String) = atoms.
      split(',').
      map(_.trim).
      map(Atom(_))
  }

}
