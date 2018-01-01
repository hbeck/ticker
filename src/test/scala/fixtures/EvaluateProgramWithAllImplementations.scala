package fixtures

import core.lars.LarsProgram
import engine.Engine
import engine.config.BuildEngine
import org.scalatest.FlatSpec

/**
  * Created by FM on 28.05.16.
  */

trait EvaluateProgramWithAllImplementations  {

  this: FlatSpec =>

  def runInAllImplementations(program: LarsProgram)(testSpecifications: (=> Engine) => Unit): Unit = {
    val config = BuildEngine.withProgram(program).configure()

    "Using Clingo-Pull" should behave like testSpecifications(config.withClingo().use().usePull().seal())
    "Using Clingo-Push" should behave like testSpecifications(config.withClingo().use().usePush().seal())

    "Using TMS-Pull" should behave like testSpecifications(config.withJtms().seal())
    "Using TMS-Push" should behave like testSpecifications(config.withJtms().seal())
  }

}
