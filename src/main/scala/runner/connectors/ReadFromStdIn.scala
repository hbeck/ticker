package runner.connectors

import java.util.concurrent.TimeUnit

import core.Atom
import runner._

import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class ReadFromStdIn(inputUnit: TimeUnit) extends ConnectToEngine {

  def startWith(runner: EngineRunner): Startable = {

    val keyboardInput = new Thread(new Runnable() {
      override def run(): Unit = Iterator.continually(scala.io.StdIn.readLine).
        map(parseInput(inputUnit)).
        takeWhile(_._2.nonEmpty).
        foreach(input => runner.append(input._1.map(runner.convertToTimePoint), input._2))
    }, "Read Input form keyboard")

    keyboardInput.setDaemon(false)

    println("Receiving input from keyboard: ")
    println("List of atoms: <atom>,<atom>")
    println("eg: a,b(1),d(2)")
    println("Or time and List of Atoms: @<time>: <atom>,<atom>")
    println("eg: @15: a,b(1),d(2)")

    keyboardInput.start _
  }


}
