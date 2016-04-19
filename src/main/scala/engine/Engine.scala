package engine

/**
  * Created by FM on 16.04.16.
  */
case class Engine(private val evaluationEngine: EvaluationEngine) {


  def add(observable: Observable) = {

    observable.subscribe(new Observer {
      override def append(evaluation: Evaluation): Unit = {
        evaluationEngine.append(evaluation.time)(evaluation.atoms.toSeq: _*)
      }
    })
  }

  def evaluate(time: Time) = evaluationEngine.evaluate(time)

}
