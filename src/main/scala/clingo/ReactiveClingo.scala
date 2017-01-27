package clingo

import core.{Predicate, Value}

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


    def terminate = {
      client.terminate()
      clingoProcess.destroy()
    }

    def signal(signals: Seq[(Predicate, Seq[Value], Seq[Tick])]) = {
      client.sendSignal(signals)
    }

    def evaluate(ticks: Seq[Tick]) = client.evaluate(ticks)
  }

}
