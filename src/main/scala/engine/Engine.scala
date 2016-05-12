package engine

/**
  * Created by FM on 16.04.16.
  */
case class Engine(private val evaluationEngine: EvaluationEngine) {

  // TODO: what is the concrete purpose of Engine-class?
  // currently its only plumbing & merging of streams/observables to the evaluation

  // intensional atom stream should be merged/maintained here
  // --> truncating can be done here as well

  // downside: what's the API for the Evaluation-Engine then?
  // currently we have a clear API: append only & evaluation only
  // otherwise we would have to pass in a 'mutable' merged stream


  def add(observable: Observable) = {

    observable.subscribe(new Observer {
      override def append(evaluation: StreamEntry): Unit = {
        evaluationEngine.append(evaluation.time)(evaluation.atoms.toSeq: _*)
      }
    })
  }

  def evaluate(time: TimePoint) = evaluationEngine.evaluate(time)

}


/*
+Potential Builder?
+
+val engine = Engine
+  .initialize()
+  .forProgram(program)
+  .withAsp()
+    .pull().withFuture(5 seconds)
+  .useInput(Stream.load(..))
+  .build()
+
+engine.run()
+
+ */