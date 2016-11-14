package runner

import java.net.InetSocketAddress
import java.util.TimerTask
import java.util.concurrent.{Executors, TimeUnit}

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import core.Atom
import core.lars.TimePoint
import engine.{EvaluationEngine, NoResult, Result}
import unfiltered.util.Of.Int

import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Created by FM on 10.11.16.
  */
case class EngineRunner(engine: EvaluationEngine, engineSpeed: Duration, outputSpeed: Duration) {

  implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  val timer = new java.util.Timer()

  var inputSources: Seq[Thread] = List()

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

    inputSources.foreach(_.start())

    Thread.currentThread().join()
  }

  def receiveInputFromStdIn(inputUnit: TimeUnit): Unit = {
    val parser = Input(inputUnit)

    val keyboardInput = new Thread(new Runnable() {
      override def run(): Unit = Iterator.continually(scala.io.StdIn.readLine).
        map(parser.parseInput).
        takeWhile(_._2.nonEmpty).
        foreach(input => append(input._1, input._2))
    }, "Read Input form keyboard")

    keyboardInput.setDaemon(false)

    inputSources = inputSources :+ keyboardInput
  }

  def receiveInputOverHttp(): Unit = {
    new SimpleHttpServer()
      .withContext("/") {
        case "/hello" => Future {
          (200, "OK")
        }
      }
      .start()
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

case class SimpleHttpServer(port: Int = 8080) {
  type HttpFunction = String => Future[(Int, String)]

  val httpServer = HttpServer.create(new InetSocketAddress(port), 0)

  def withContext(root: String)(httpFunction: HttpFunction) = {
    def send(httpExchange: HttpExchange, responseCode: Int, content: String) = {
      val response = content.getBytes
      httpExchange.sendResponseHeaders(responseCode, response.length)
      httpExchange.getResponseBody.write(response)
      httpExchange.close()
    }

    httpServer.createContext("/", new HttpHandler {
      override def handle(exchange: HttpExchange): Unit = {
        println(Thread.currentThread())

        val path = exchange.getRequestURI.getPath

        val futureResponse = httpFunction(path)
        implicit val executionContext = scala.concurrent.ExecutionContext.global
        futureResponse onComplete {
          case Success((responseCode, content)) => send(exchange, responseCode, content)
          case Failure(e) => send(exchange, 500, "Servererror " + e.getMessage)
        }
      }
    })

    this
  }

  def start() = httpServer.start()

  def stop() = httpServer.stop(0)
}
