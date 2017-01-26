package clingo

/**
  * Created by fm on 25/01/2017.
  */
class ReactiveClingo(wrapper: ClingoWrapper, port: Int = 5123) {

  def executeProgram(program: ReactiveClingoProgram) = {

    val reactiveClingoProcess = wrapper.runReactive(program.program)

    val client = new ReactiveClingoClient(port)

    client.connect()

    new RunningReactiveClingo(reactiveClingoProcess, client)
  }

  class RunningReactiveClingo(private val clingoProcess: scala.sys.process.Process, client: ReactiveClingoClient) {


    def ticks(tick: Seq[TickValue]) = client.sendTick(tick)

    def terminate = {
      client.terminate()
      clingoProcess.destroy()
    }

    def signal(atom: Seq[ClingoAtom]) = {
      client.sendSignal(atom)
    }

    def evaluate = client.evaluate()
  }

}
