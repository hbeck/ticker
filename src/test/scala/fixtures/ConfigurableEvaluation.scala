package fixtures

import core.lars.LarsProgram
import engine.EvaluationEngine
import fixtures.tags.NoTmsDirectPolicy
import org.scalatest._


/**
  * Created by FM on 01.06.16.
  */

trait ConfigurableEvaluationSpec extends FlatSpec with EvaluationEngineBuilder {

  val program: LarsProgram

  private var engineEvaluationType: EvaluationType = this.defaultEvaluationType
  private var engineCache: Option[EvaluationEngine] = None

  def evaluationEngine: EvaluationEngine = engineCache.get

  override def withFixture(test: NoArgTest): Outcome = {

    val engineConfig = test.configMap.get("engineConfig")

    engineConfig match {
      case Some(config) => {
        // TODO: is there a way to make this type safe? - this will fail at runtime if not EngineBuilder

        val c = config.asInstanceOf[EngineConfig]
        this.engineEvaluationType = c.evaluationType
        this.engineCache = Some(c.builder(program))
      }
      case _ => this.engineCache = Some(defaultEngine(program))
    }

    info("Using engine " + evaluationType)
    ConfigurableEvaluationSpec.super.withFixture(test)
  }

  protected def evaluationType = this.engineEvaluationType

  protected def pendingWithTms(f: => Unit): Unit = pendingWithTms("")(f)

  protected def pendingWithTms(message: String = "")(f: => Unit): Unit = {
    evaluationType match {
      case AspBasedTms => {
        if (message != null && message.nonEmpty)
          info(message)
        pendingUntilFixed(f)
      }
      case _ => f
    }
  }
}


trait ConfigurableEvaluationSuite extends Suite with EvaluationEngineBuilder {


  protected override def runNestedSuites(args: Args): Status = {
    val config = EngineConfig(this.defaultEvaluationType, (p: LarsProgram) => this.defaultEngine(p))
    val configEntry = ("engineConfig", config)

    val c = args.configMap + configEntry
    var filter = args.filter

    if (this.isInstanceOf[TmsDirectPolicyEngine]) {
      filter = Filter.apply(tagsToExclude = Set(NoTmsDirectPolicy.name))
    }

    val argumentsWithEngine = Args(args.reporter, configMap = c, filter = filter)

    //    profile.withWarmup(10) {
    super.runNestedSuites(argumentsWithEngine)
    //    }
  }
}
