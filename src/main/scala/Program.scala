import java.io.File

import com.typesafe.scalalogging.Logger
import common.Util
import core.Predicate
import core.lars.{ClockTime, Format, LarsProgram}
import engine._
import engine.connectors.{OutputToSocket, OutputToStdOut, ReadFromSocket, ReadFromStdIn}
import reasoner.Reasoner
import reasoner.config.ReasonerChoice._
import reasoner.config.{BuildReasoner, EvaluationModifier, ReasonerChoice}
import reasoner.parser.LarsParser

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
        val program = config.parseProgramFromFile()

        printProgram(program)

        logger.info(f"Engine Configuration: " + Util.prettyPrint(config))

        val timeWindowSmallerThanEngineUnit = program.slidingTimeWindowsAtoms.
          exists {
            t => Duration(t.windowSize.length, t.windowSize.unit).lt(config.clockTime)
            //TODO have to check that every window is a multiple of clock time
          }
        if (timeWindowSmallerThanEngineUnit)
          throw new IllegalArgumentException("Cannot specify a sliding time window with a smaller window size than the clock time.")

        val reasoner = config.buildReasoner(program)

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

      opt[Seq[File]]('p', "programs").required().valueName("<file>,<file>,...").
        action((x, c) => c.copy(programFiles = x)).
        text("Program fiel required")

      opt[ReasonerChoice]('r', "reasoner").optional().valueName("<reasoner>").
        action((x, c) => c.copy(reasoner = x)).
        text("Reasoning strategy required, possible values: " + ReasonerChoice.values)

      opt[Seq[String]]('f', "filter").optional().valueName("none | inference | <predicate>,<predicate>,...").
        action((x,c) => c.copy(filter = x)).
        text("Possible filters: none | inference | <predicate>,<predicate>,...")

      opt[Duration]('c', "clock").optional().valueName("<value><time-unit>").
        validate(d =>
          if (d.lt(Duration.Zero))
            Left("clock time must be > 0")
          else
            Right((): Unit)
        ).
        action((x, c) => c.copy(clockTime = x)).
        text("valid units: ms, s, sec, min, h. eg: 10ms")

      opt[OutputTiming]('e', "outputEvery").
        optional().
        valueName("change | signal | time | <value>signals | <value><time-unit>").
        validate {
          case Signal(count) if count < 0 => Left("signal count must be > 0")
          case Time(Some(duration)) if duration lt Duration.Zero => Left("duration must be > 0")
          case _ => Right((): Unit)
        }.
        action((x, c) => c.copy(outputTiming = x)).
        text("valid units: ms, s, min, h. eg: 10ms")

      opt[Seq[Source]]('i', "input").optional().valueName("<source>,<source>,...").
        action((x, c) => c.copy(inputs = x)).
        text("Possible input sources: read from input with 'stdin', read from a socket with 'socket:<port>'")

      opt[Seq[Sink]]('o', "output").optional().valueName("<sink>,<sink>,...").
        action((x, c) => c.copy(outputs = x)).
        text("Possible output sinks: write to output with 'stdout', write to a socket with 'socket:<port>'")

      help("help").
        text("Specify init parameters for running the engine")

      checkConfig(c => {
        c.outputTiming match {
          case Time(Some(duration)) if duration lt c.clockTime =>
            reportWarning("outputEvery time interval is less than the engine clock time. The output time interval will be set to the engine unit")
          case _ =>
        }

        Right((): Unit)
      })
    }

    parser.parse(args, Config())
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
  implicit val inputSourcesRead: scopt.Read[Source] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdin" => StdIn
    case SocketPattern(port) => SocketInput(port.toInt)
  })

  implicit val outputSinksRead: scopt.Read[Sink] = scopt.Read.reads(s => s.toLowerCase match {
    case "stdout" => StdOut
    case SocketPattern(port) => SocketOutput(port.toInt)
  })


  sealed trait Source
  object StdIn extends Source
  case class SocketInput(port: Int) extends Source

  sealed trait Sink
  object StdOut extends Sink
  case class SocketOutput(port: Int) extends Sink

  case class Config(programFiles: Seq[File] = null,
                    reasoner: ReasonerChoice = ReasonerChoice.Incremental,
                    filter: Seq[String] = Seq(),
                    clockTime: ClockTime = 1 second,
                    outputTiming: OutputTiming = Change,
                    inputs: Seq[Source] = Seq(StdIn),
                    outputs: Seq[Sink] = Seq(StdOut)
                   ) {

    def parseProgramFromFile() = {
      if (programFiles.isEmpty) {
        throw new RuntimeException("program argument missing")
      }
      val programs: Seq[LarsProgram] = programFiles.map{file => LarsParser(file.toURI.toURL)}
      if (programs.size == 1) {
        programs(0)
      } else {
        programs.reduce { (p1,p2) => p1 ++ p2 }
      }
    }

    def buildReasoner(program: LarsProgram): Reasoner = {

      val reasonerBuilder = BuildReasoner.
        withProgram(program).
        withClockTime(clockTime)

      val preparedReasoner = reasoner match {
        case ReasonerChoice.Incremental =>
          reasonerBuilder.configure().withIncremental().use()
        case ReasonerChoice.Clingo => outputTiming match {
          case Time(_) => reasonerBuilder.configure().withClingo().withDefaultEvaluationMode().usePull() //TODO n signals, n > 1
          case _ => reasonerBuilder.configure().withClingo().withDefaultEvaluationMode().usePush()
        }
      }

      if (filter.isEmpty) {
        preparedReasoner.withNoFilter().seal()
      } else if (filter.size == 1) {
        filter(0) match {
          case "none" => preparedReasoner.withNoFilter().seal()
          case "inferences" => preparedReasoner.withIntensionalFilter().seal()
          case pred:String => preparedReasoner.withPredicateFilter(Set(Predicate(pred))).seal()
        }
      } else {
        preparedReasoner.withPredicateFilter(filter.map(Predicate(_)).toSet).seal()
      }
    }
  }

}