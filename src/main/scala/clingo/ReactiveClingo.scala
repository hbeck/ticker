package clingo

import core.{GroundAtom, GroundAtomWithArguments, Predicate, Value}

/**
  * Created by fm on 25/01/2017.
  */
class ReactiveClingo(wrapper: ClingoWrapper, port: Int = 5123) {

  def executeProgram(program: ReactiveClingoProgram) = {

    val reactiveClingoProcess = wrapper.runReactive(program.program)

    val client = ReactiveClingoClient.connect(port)

    new RunningReactiveClingo(reactiveClingoProcess, client)
  }

  class RunningReactiveClingo(private val clingoProcess: scala.sys.process.Process, client: ReactiveClingoClient) {


    def terminate = {
      client.terminate()
      clingoProcess.destroy()
    }

    // TODO: pinned atom as argument?
    def signal(signals: Seq[(GroundAtom, Seq[Tick])]) = {
      client.sendSignal(convertToSignalArgument(signals))
    }

    private def convertToSignalArgument(signals: Seq[(GroundAtom, Seq[Tick])]) = {
      signals.collect {
        case (groundAtom, ticks) => groundAtom match {
          case GroundAtomWithArguments(p, args) => ReactiveClingoSignal(p.caption, args, ticks)
          case g: GroundAtom => ReactiveClingoSignal(g.predicate.caption, Seq(), ticks)
        }
      }
    }

    def expire(signals: Seq[(GroundAtom, Seq[Tick])]) = {
      client.sendExpire(convertToSignalArgument(signals))
    }

    def evaluate(ticks: Seq[Tick]) = client.evaluate(ticks)
  }

}
