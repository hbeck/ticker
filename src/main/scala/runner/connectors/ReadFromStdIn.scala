package runner.connectors

import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.Logger
import core.Atom
import runner._

import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class ReadFromStdIn(inputUnit: TimeUnit) extends ConnectToEngine {

  def startWith(runner: Engine): Startable = {

    println("Receiving input from keyboard: ")
    println("List of atoms: <atom>,<atom>")
    println("eg: a,b(1),d(2)")
    println("Or time and List of Atoms: @<time>: <atom>,<atom>")
    println("eg: @15: a,b(1),d(2)")

    () => {
      Iterator.continually(scala.io.StdIn.readLine).
        filter(_ != null).
        map(parseInput(inputUnit)).
        takeWhile(_._2.nonEmpty).
        foreach(input => runner.append(input._1.map(runner.convertToTimePoint), input._2))
    }
  }
}
