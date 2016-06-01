package fixtures

import core.lars.Program
import engine.EvaluationEngine
import org.scalatest._


/**
  * Created by FM on 01.06.16.
  */

trait ConfigurableEvaluationSpec extends FlatSpec with EvaluationEngineBuilder {

  val program: Program

  private var engineCache: EvaluationEngine = null

  def evaluationEngine: EvaluationEngine = engineCache

  override def withFixture(test: NoArgTest): Outcome = {

    val engineConfig = test.configMap.get("engine")

    engineConfig match {
        // TODO: is there a way to make this type safe?
      case Some(builder: EngineBuilder) => this.engineCache = builder(program)
      case _ => this.engineCache = defaultEngine(program)
    }

    ConfigurableEvaluationSpec.super.withFixture(test)
  }

}

trait ConfigurableEvaluationSuite extends Suite with EvaluationEngineBuilder {

  protected override def runNestedSuites(args: Args): Status = {

    val c = args.configMap +("engine", (p: Program) => this.defaultEngine(p))

    val argumentsWithEngine = Args(args.reporter, configMap = c)

    //    profile.withWarmup(10) {
    super.runNestedSuites(argumentsWithEngine)
    //    }
  }
}
