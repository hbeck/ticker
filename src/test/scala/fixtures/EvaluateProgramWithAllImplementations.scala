package fixtures

import core.lars.LarsProgram
import engine.Reasoner
import engine.config.BuildReasoner
import org.scalatest.FlatSpec

/**
  * Created by FM on 28.05.16.
  */

trait EvaluateProgramWithAllImplementations  {

  this: FlatSpec =>

  def runInAllImplementations(program: LarsProgram)(testSpecifications: (=> Reasoner) => Unit): Unit = {
    val config = BuildReasoner.withProgram(program).configure()

    "Using Clingo-Pull" should behave like testSpecifications(config.withClingo().use().usePull().seal())
    "Using Clingo-Push" should behave like testSpecifications(config.withClingo().use().usePush().seal())

    "Using TMS-Pull" should behave like testSpecifications(config.withJtms().seal())
    "Using TMS-Push" should behave like testSpecifications(config.withJtms().seal())
  }

}
