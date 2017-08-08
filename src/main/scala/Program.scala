import java.io.File

import common.Util
import core.Atom
import core.lars.{Format, LarsBasedProgram, LarsProgram, TimeWindowSize}
import engine.{EvaluationEngine, NoResult}
import engine.config.Reasoner._
import engine.config.{BuildEngine, EvaluationModifier, Reasoner}
import engine.parser.LarsParser
import runner._
import runner.connectors.{OutputToSocket, OutputToStdOut, ReadFromSocket, ReadFromStdIn}

import scala.concurrent.duration._

/**
  * Created by FM on 05.11.16.
  */
object Program {

  def main(args: Array[String]): Unit = {

    val sampleArgs = Seq(
      "--program", "src/test/resources/test.rules",
      "--reasoner", "Clingo",
      "--inputType", "Http,StdIn",
      "--timeunit", "1s",
      "--outputEvery", "2signals"
    ).toArray

    parseParameters(args) match {
      case Some(config) => {
        val program = config.parseProgram

        printProgram(program)

        println()

        println(f"Engine Configuration: " + Util.prettyPrint(config))

        println()

        val timeWindowSmallerThanEngineUnit = program.slidingTimeWindowsAtoms.
          exists {
            t => Duration(t.windowSize.length, t.windowSize.unit) lt config.timeUnit
          }
        if (timeWindowSmallerThanEngineUnit)
          throw new IllegalArgumentException("Cannot specify a sliding time window with a smaller window size than the engine timeUnit.")

        val engine = config.buildEngine(program)

        val runner = EngineRunner(engine, config.timeUnit, config.outputEvery)
        config.inputs foreach {
          case SocketInput(port) => runner.connect(ReadFromSocket(config.timeUnit._2, port))
          case StdIn => runner.connect(ReadFromStdIn(config.timeUnit._2))
        }
        config.outputs foreach {
          case StdOut => runner.connect(OutputToStdOut)
          case SocketOutput(port) => runner.connect(OutputToSocket(port))
        }

        runner.start()
      }
      case None => throw new RuntimeException("Could not parse all arguments correcly")
    }
  }

  def printProgram(program: LarsProgram): Unit = {
    println(f"Lars Program of ${program.rules.size} rules with ${program.extendedAtoms.size} different atoms.")

    if (program.rules.length <= 10) {
      Format.parsed(program) foreach println
    } else {
      println("First 10 rules:")
      Format.parsed(program.rules.take(10)) foreach println
    }
  }

  def parseParameters(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("scopt", "3.x")

      opt[File]('p', "program").required().valueName("<file>").
        action((x, c) => c.copy(programFile = x)).
        text("program is a required file property")

      opt[Reasoner]('r', "reasoner").optional().valueName("<reasoner type>").
        action((x, c) => c.copy(reasoner = x)).
        text("An reasoner required, possible values: " + Reasoner.values)

      opt[Duration]("timeunit").optional().valueName("<value><unit>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("inputSpeed must be > 0")
          else
            Right((): Unit)
        ).
        action((x, c) => c.copy(timeUnit = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[OutputEvery]("outputEvery").
        optional().
        valueName("diff | signal | time | <value>signals | <value><time-unit>").
        validate {
          case Signal(count) if count < 0 => Left("signal count must be > 0")
          case Time(Some(duration)) if duration lt Duration.Zero => Left("duration must be > 0")
          case _ => Right((): Unit)
        }.
        action((x, c) => c.copy(outputEvery = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[Seq[InputTypes]]('i', "inputType").optional().valueName("<input type>,<input type>,...").
        action((x, c) => c.copy(inputs = x)).
        text("Possible Input Types: read from input with 'StdIn', read from a socket with 'socket:<port>'")

      opt[Seq[OutputTypes]]('o', "outputType").optional().valueName("<output type>,<output type>,...").
        action((x, c) => c.copy(outputs = x)).
        text("Possible Output Types: write to output with 'StdOut', write to a socket with 'socket:<port>'")

      help("help").
        text("Specify init parameters for running the engine")

      checkConfig(c => {
        c.outputEvery match {
          case Time(Some(duration)) if duration lt c.timeUnit =>
            reportWarning("outputEvery time interval is less than the engine timeUnit. The output time interval will be set to the engine unit")
          case _ =>
        }

        Right((): Unit)
      })
    }

    parser.parse(args, Config(programFile = new File("")))
  }

  private val SignalPattern = "(\\d+)signals".r

  implicit val outputEveryRead: scopt.Read[OutputEvery] =
    scopt.Read.reads(s => s.toLowerCase match {
      case "diff" => Diff
      case "signal" => Signal()
      case "timeunit" => Time()
      case SignalPattern(count) => Signal(count.toInt)
      case shouldBeTime => Time(Some(Duration.create(shouldBeTime)))
    })

  implicit val evaluationTypesRead: scopt.Read[Reasoner.Value] =
    scopt.Read.reads(Reasoner withName)

  implicit val evaluationModifierRead: scopt.Read[EvaluationModifier.Value] = scopt.Read.reads(EvaluationModifier withName)

  private val SocketPattern = "socket:(\\d+)".r
  implicit val inputTypesRead: scopt.Read[InputTypes] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdin" => StdIn
    case SocketPattern(port) => SocketInput(port.toInt)
  })

  implicit val outputTypesRead: scopt.Read[OutputTypes] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdout" => StdOut
    case SocketPattern(port) => SocketOutput(port.toInt)
  })


  sealed trait InputTypes

  object StdIn extends InputTypes

  case class SocketInput(port: Int) extends InputTypes

  sealed trait OutputTypes

  object StdOut extends OutputTypes

  case class SocketOutput(port: Int) extends OutputTypes

  case class Config(reasoner: Reasoner = Reasoner.Ticker,
                    programFile: File,
                    timeUnit: Duration = 1 second,
                    outputEvery: OutputEvery = Diff,
                    inputs: Seq[InputTypes] = Seq(StdIn),
                    outputs: Seq[OutputTypes] = Seq(StdOut),
                    filter: Option[Set[String]] = None
                   ) {

    def parseProgram = LarsParser(programFile.toURI.toURL)

    def buildEngine(program: LarsProgram): EvaluationEngine = {
      val engineBuilder = BuildEngine.
        withProgram(program).
        withTimePointDuration(timeUnit)

      val startableEngine = reasoner match {
        case Reasoner.Ticker =>
          engineBuilder.configure().withTms().withIncremental()
        case Reasoner.Clingo => outputEvery match {
          case Time(_) => engineBuilder.configure().withClingo().use().usePull()
          case _ => engineBuilder.configure().withClingo().use().usePush()
        }
      }
      filter match {
        case Some(atoms) if atoms.nonEmpty => startableEngine.filterTo(atoms.map(Atom(_))).start()
        case _ => startableEngine.start()
      }
    }
  }

}