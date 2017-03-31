package clingo.reactive

import clingo._
import common.Resource
import core.{GroundAtom, GroundAtomWithArguments}

/**
  * Created by fm on 25/01/2017.
  */
class ReactiveClingo(wrapper: ClingoWrapper, port: Int = 5123) {

  def executeProgram(program: ReactiveClingoProgram) = {

    val reactiveClingoProcess = wrapper.runReactive(program.program)

    Thread.sleep(100)

    val client = ReactiveClingoClient.connect(port)

    new RunningReactiveClingo(reactiveClingoProcess, client)
  }

  class RunningReactiveClingo(private val clingoProcess: scala.sys.process.Process, client: ReactiveClingoClient) extends Resource {

    def close = {
      client.close()
      clingoProcess.destroy()
    }

    // TODO: pinned atom as argument? TODO hb review
    def signal(signals: Seq[(GroundAtom, Seq[ClingoTick])]) = {
      client.sendSignal(convertToSignalArgument(signals))
    }

    private def convertToSignalArgument(signals: Seq[(GroundAtom, Seq[ClingoTick])]) = {
      signals.collect {
        case (groundAtom, ticks) => groundAtom match {
          case GroundAtomWithArguments(p, args) => ReactiveClingoSignal(p.caption, args, ticks)
          case g: GroundAtom => ReactiveClingoSignal(g.predicate.caption, Seq(), ticks)
        }
      }
    }

    def expire(signals: Seq[(GroundAtom, Seq[ClingoTick])]) = {
      client.sendExpire(convertToSignalArgument(signals))
    }

    def evaluate(ticks: Seq[ClingoTick]) = client.evaluate(ticks)
  }

}
