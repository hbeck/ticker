import java.io.File

import com.typesafe.scalalogging.Logger
import common.Util
import core.Atom
import core.lars.{ClockTime, Format, LarsProgram}
import reasoner.Reasoner
import reasoner.config.ReasonerChoice._
import reasoner.config.{BuildReasoner, EvaluationModifier, ReasonerChoice}
import reasoner.parser.LarsParser
import engine._
import engine.connectors.{OutputToSocket, OutputToStdOut, ReadFromSocket, ReadFromStdIn}

import scala.concurrent.duration._

/**
  * Created by FM on 05.11.16.
  */
object Program {

  val logger = Logger("Program")

  def main(args: Array[String]): Unit = {

//    val sampleArgs = Seq(
//      "--program", "src/test/resources/test.rules",
//      "--reasoner", "clingo",
//      "--inputSource", "stdin",
//      "--clock", "1s",
//      "--outputEvery", "2signals"
//    ).toArray

    parseParameters(args) match {
      case Some(config) => {
        val program = config.larsProgram

        printProgram(program)

        logger.info(f"Engine Configuration: " + Util.prettyPrint(config))

        val timeWindowSmallerThanEngineUnit = program.slidingTimeWindowsAtoms.
          exists {
            t => Duration(t.windowSize.length, t.windowSize.unit) lt config.clockTime
              //TODO have to check that every window is a multiple of clock time
          }
        if (timeWindowSmallerThanEngineUnit)
          throw new IllegalArgumentException("Cannot specify a sliding time window with a smaller window size than the clock time.")

        val reasoner = config.buildReasoner()

        val engine = Engine(reasoner, config.clockTime, config.outputTiming)
        config.inputs foreach {
          case SocketInput(port) => engine.connect(ReadFromSocket(config.clockTime._2, port))
          case StdIn => engine.connect(ReadFromStdIn(config.clockTime._2))
        }
        config.outputs foreach {
          case StdOut => engine.connect(OutputToStdOut)
          case SocketOutput(port) => engine.connect(OutputToSocket(port))
        }

        engine.start()
      }
      case None => throw new RuntimeException("Could not parse all arguments")
    }
  }

  def printProgram(program: LarsProgram): Unit = {
    logger.info(f"Lars Program of ${program.rules.size} rules with ${program.extendedAtoms.size} different atoms.")
    Format.parsed(program).foreach(logger.info(_))
  }

  def parseParameters(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("scopt") {
      head("scopt", "3.x")

      opt[File]('p', "program").required().valueName("<file>"). //TODO PPS allow multiple files
        action((x, c) => c.copy(programFile = x)).
        text("program is a required file property")

      opt[ReasonerChoice]('r', "reasoner").optional().valueName("<reasoning strategy>").
        action((x, c) => c.copy(reasoner = x)).
        text("Reasoning stragety required, possible values: " + ReasonerChoice.values)

      //TODO filter missing

      opt[Duration]('c', "clock").optional().valueName("<value><time-unit>").
        validate(d =>
          if (d lt Duration.Zero)
            Left("clock time must be > 0")
          else
            Right((): Unit)
        ).
        action((x, c) => c.copy(clockTime = x)).
        text("valid units: ms, s, min, h. eg: 10ms") //TODO

      opt[OutputTiming]('e',"outputEvery").
        optional().
        valueName("change | signal | time | <value>signals | <value><time-unit>").
        validate {
          case Signal(count) if count < 0 => Left("signal count must be > 0")
          case Time(Some(duration)) if duration lt Duration.Zero => Left("duration must be > 0")
          case _ => Right((): Unit)
        }.
        action((x, c) => c.copy(outputTiming = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[Seq[InputSource]]('i', "inputSource").optional().valueName("<input source>,<input source>,...").
        action((x, c) => c.copy(inputs = x)).
        text("Possible Input Sources: read from input with 'StdIn', read from a socket with 'socket:<port>'")

      opt[Seq[OutputSink]]('o', "outputSink").optional().valueName("<output sink>,<output sink>,...").
        action((x, c) => c.copy(outputs = x)).
        text("Possible Output Sinks: write to output with 'StdOut', write to a socket with 'socket:<port>'")

      help("help").
        text("Specify init parameters for running the engine")

      checkConfig(c => {
        c.outputTiming match {
          case Time(Some(duration)) if duration lt c.clockTime =>
            reportWarning("outputEvery time interval is less than the engine timeUnit. The output time interval will be set to the engine unit")
          case _ =>
        }

        Right((): Unit)
      })
    }

    parser.parse(args, Config(programFile = new File("")))
  }

  private val SignalPattern = "(\\d+)signals".r

  implicit val outputEveryRead: scopt.Read[OutputTiming] =
    scopt.Read.reads(s => s.toLowerCase match {
      case "change" => Change
      case "signal" => Signal()
      case "time" => Time()
      case SignalPattern(count) => Signal(count.toInt)
      case shouldBeTime => Time(Some(Duration.create(shouldBeTime)))
    })

  implicit val evaluationTypesRead: scopt.Read[ReasonerChoice.Value] =
    scopt.Read.reads(ReasonerChoice withName)

  implicit val evaluationModifierRead: scopt.Read[EvaluationModifier.Value] = scopt.Read.reads(EvaluationModifier withName)

  private val SocketPattern = "socket:(\\d+)".r
  implicit val inputSourcesRead: scopt.Read[InputSource] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdin" => StdIn
    case SocketPattern(port) => SocketInput(port.toInt)
  })

  implicit val outputSinksRead: scopt.Read[OutputSink] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdout" => StdOut
    case SocketPattern(port) => SocketOutput(port.toInt)
  })


  sealed trait InputSource

  object StdIn extends InputSource

  case class SocketInput(port: Int) extends InputSource

  sealed trait OutputSink

  object StdOut extends OutputSink

  case class SocketOutput(port: Int) extends OutputSink

  case class Config(reasoner: ReasonerChoice = ReasonerChoice.Incremental,
                    programFile: File, //TODO PPS multiple
                    clockTime: ClockTime = 1 second,
                    outputTiming: OutputTiming = Change,
                    inputs: Seq[InputSource] = Seq(StdIn),
                    outputs: Seq[OutputSink] = Seq(StdOut),
                    filter: Option[Set[String]] = None
                   ) {

    val larsProgram = LarsParser(programFile.toURI.toURL)

    def buildReasoner(): Reasoner = {

      val reasonerBuilder = BuildReasoner.
        withProgram(larsProgram).
        withClockTime(clockTime)

      val preparedReasoner = reasoner match {
        case ReasonerChoice.Incremental =>
          reasonerBuilder.configure().withIncremental().use()
        case ReasonerChoice.Clingo => outputTiming match {
          case Time(_) => reasonerBuilder.configure().withClingo().withDefaultEvaluationMode().usePull() //TODO n signals, n > 1
          case _ => reasonerBuilder.configure().withClingo().withDefaultEvaluationMode().usePush()
        }
      }
      filter match {
        case Some(atoms) if atoms.nonEmpty => preparedReasoner.withFilter(atoms.map(Atom(_))).seal()
        case _ => preparedReasoner.seal()
      }
    }
  }

}