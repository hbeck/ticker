package runner

import java.net.InetSocketAddress

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import core.Atom
import core.lars.TimeUnit
import unfiltered.request.QueryParams
import unfiltered.util.Of.Int

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Created by FM on 14.11.16.
  */
case class ReadFromHttp(inputUnit: TimeUnit) extends ConnectToEngine {
  type QueryParams = Map[String, Seq[String]]

  def startWith(engineRunner: EngineRunner): Startable = {
    val server = SimpleHttpServer()
      .withContext("/") {
        case "/input" => params => Future {
          val time = parseTime(params)
          val atoms = parseAtoms(params)
          if (atoms.nonEmpty) {
            engineRunner.append(time.map(engineRunner.convertToTicks), atoms)
            (200, f"OK, appending @$time $atoms")
          }
          else
            (400, f"Could not interpret $params")
        }
      }

    server.start
  }

  def parseTime(params: QueryParams): Option[Duration] = {
    if (params.contains("time"))
      params("time").head.trim match {
        case Int(x) => Some(Duration(x, inputUnit))
        case _ => None
      }
    else
      None
  }

  def parseAtoms(params: QueryParams): Seq[Atom] = {
    if (params.contains("atoms"))
      params("atoms").
        map(_.trim).
        flatMap(_.split(',')).
        map(_.trim).
        map(Atom(_))
    else
      Seq()
  }
}

case class SimpleHttpServer(port: Int = 8080) {
  type QueryParams = Map[String, Seq[String]]
  type HttpFunction = String => QueryParams => Future[(Int, String)]

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
        val path = exchange.getRequestURI.getPath
        val params = QueryParams.urldecode(exchange.getRequestURI.toString)
        val futureResponse = httpFunction(path)(params)
        //        implicit val executionContext = scala.concurrent.ExecutionContext.global
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
