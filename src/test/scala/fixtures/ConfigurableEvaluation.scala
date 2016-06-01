package fixtures

import core.lars.Program
import engine.EvaluationEngine
import org.scalatest._


/**
  * Created by FM on 01.06.16.
  */

trait ConfigurableEvaluationSpec extends FlatSpec with EvaluationEngineBuilder {

  val program: Program

  private var engineCache: Option[EvaluationEngine] = None

  def evaluationEngine: EvaluationEngine = engineCache.getOrElse(defaultEngine(program))

  override def withFixture(test: NoArgTest): Outcome = {

    val engineConfig = test.configMap.get("engine")

    engineConfig match {
      case Some(builder: EngineBuilder) => this.engineCache = Some(builder(program))
      case _ => this.engineCache = None
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
