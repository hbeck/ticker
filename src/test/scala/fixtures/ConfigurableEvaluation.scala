package fixtures

import core.lars.LarsProgram
import engine.EvaluationEngine
import org.scalatest._


/**
  * Created by FM on 01.06.16.
  */

trait ConfigurableEvaluationSpec extends FlatSpec with EvaluationEngineBuilder {

  val program: LarsProgram

  private var engineEvaluationType: EvaluationType = this.defaultEvaluationType
  private var engineCache: EvaluationEngine = null //TODO: null nicht die art der feinen scala leute

  def evaluationEngine: EvaluationEngine = engineCache

  override def withFixture(test: NoArgTest): Outcome = {

    val engineConfig = test.configMap.get("engineConfig")

    engineConfig match {
      case Some(config) => {
        // TODO: is there a way to make this type safe? - this will fail at runtime if not EngineBuilder

        val c = config.asInstanceOf[EngineConfig]
        this.engineEvaluationType = c.evaluationType
        this.engineCache = c.builder(program)
      }
      case _ => this.engineCache = defaultEngine(program)
    }

    ConfigurableEvaluationSpec.super.withFixture(test)
  }

  protected def evaluationType = this.engineEvaluationType

  protected def pendingWithTms(f: => Unit) = {
    evaluationType match {
      case AspBasedTms => pendingUntilFixed(f)
      case _ => f
    }
  }
}


trait ConfigurableEvaluationSuite extends Suite with EvaluationEngineBuilder {


  protected override def runNestedSuites(args: Args): Status = {
    val config = EngineConfig(this.defaultEvaluationType, (p: LarsProgram) => this.defaultEngine(p))
    val configEntry = ("engineConfig", config)

    val c = args.configMap + configEntry

    val argumentsWithEngine = Args(args.reporter, configMap = c)

    //    profile.withWarmup(10) {
    super.runNestedSuites(argumentsWithEngine)
    //    }
  }
}
