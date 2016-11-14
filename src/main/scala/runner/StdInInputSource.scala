package runner

import java.util.concurrent.TimeUnit

import core.Atom
import core.lars.TimePoint
import unfiltered.util.Of.Int

import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class StdInInputSource(runner: EngineRunner) {
  def receiveInputFromStdIn(inputUnit: TimeUnit): Unit = {
    val parser = Input(inputUnit)

    val keyboardInput = new Thread(new Runnable() {
      override def run(): Unit = Iterator.continually(scala.io.StdIn.readLine).
        map(parser.parseInput).
        takeWhile(_._2.nonEmpty).
        foreach(input => runner.append(input._1, input._2))
    }, "Read Input form keyboard")

    keyboardInput.setDaemon(false)

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
      case Int(x) => Some(runner.convertToTicks(Duration(x, inputUnit)))
      case _ => None
    }

    def parseAtoms(atoms: String) = atoms.
      split(',').
      map(_.trim).
      map(Atom(_))
  }

}
