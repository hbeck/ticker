package runner

import java.net.InetSocketAddress
import java.nio.charset.Charset

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import core.Atom
import core.lars.TimeUnit

import scala.collection.immutable.Stream.Empty
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
        case "/input" => params =>
          Future {
            val time = parseTime(params)
            val atoms = parseAtoms(params)
            if (atoms.nonEmpty) {
              engineRunner.append(time.map(engineRunner.convertToTimePoint), atoms)
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
        val params = splitQuery(exchange.getRequestURI.toString)
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


  import java.net.URLDecoder


  def splitQuery(url: String) = {
    var query_pairs: Map[String, Seq[String]] = Map()
    val pairs = url.split("&")
    for (pair <- pairs) {
      val idx = pair.indexOf("=")
      val key = if (idx > 0) URLDecoder.decode(pair.substring(0, idx), "UTF-8")
      else pair
      if (!query_pairs.contains(key))
        query_pairs = query_pairs.updated(key, Seq())
      val value = if (idx > 0 && pair.length > idx + 1) URLDecoder.decode(pair.substring(idx + 1), "UTF-8")
      else null
      query_pairs = query_pairs.updated(key, query_pairs(key) ++ Seq(value))
    }
    query_pairs
  }

}
