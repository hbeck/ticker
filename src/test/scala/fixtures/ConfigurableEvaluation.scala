package fixtures

import common.Resource
import core.lars.LarsProgram
import reasoner.Reasoner
import fixtures.tags.NoTmsDirectPolicy
import org.scalatest._


/**
  * Created by FM on 01.06.16.
  */

trait ConfigurableReasonerSpec extends FlatSpec with ReasonerBuilder {

  val program: LarsProgram

  private var evaluationMode: EvaluationMode = IncrementalEvaluation
  private var reasonerCache: Option[Reasoner] = None

  def reasoner: Reasoner = reasonerCache.get

  override def withFixture(test: NoArgTest): Outcome = {

    this.reasonerCache = Some(reasonerBuilder(program))

    //val reasonerConfig = test.configMap.get("reasonerConfig")

//    reasonerConfig match {
//      case Some(config) => {
//
//        //val c = config.asInstanceOf[ReasonerBuilderConfig]
//        //this.engineEvaluationType = c.evaluationType
//        this.reasonerCache = Some(reasonerBuilder(program))
//      }
//      case _ =>this.reasonerCache = Some(reasonerBuilder(program))
//    }

//    info("Using engine " + evaluationType)
    try {
      ConfigurableReasonerSpec.super.withFixture(test)
    } finally {
      reasonerCache match {
        case Some(x: Resource) => x.close()
        case _ =>
      }
    }
  }

  //protected def evaluationType = this.engineEvaluationType

  protected def pendingWithJtms(f: => Unit): Unit = pendingWithJtms("")(f)

  protected def pendingWithJtms(message: String = "")(f: => Unit): Unit = {
    evaluationMode match {
      case IncrementalEvaluation => {
        if (message != null && message.nonEmpty)
          info(message)
        pendingUntilFixed(f)
      }
      case _ => f
    }
  }
}


trait ConfigurableEvaluationSuite extends Suite with ReasonerBuilder {

  protected override def runNestedSuites(args: Args): Status = {
    //val config = ReasonerBuilderConfig(this.defaultEvaluationType, (p: LarsProgram) => this.reasonerBuilder(p))
    //val configEntry = ("engineConfig", config)

    val c = args.configMap //+ configEntry
    var filter = args.filter

    if (this.isInstanceOf[JtmsIncrementalReasoner]) {
      filter = Filter.apply(tagsToExclude = Set(NoTmsDirectPolicy.name))
    }

    val argumentsWithEngine = Args(args.reporter, configMap = c, filter = filter)

    //    profile.withWarmup(10) {
    super.runNestedSuites(argumentsWithEngine)
    //    }
  }
}
