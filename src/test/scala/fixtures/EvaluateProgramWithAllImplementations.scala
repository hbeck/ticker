package fixtures

import core.lars.Program
import engine.EvaluationEngine
import engine.config.BuildEngine
import org.scalatest.FlatSpec

/**
  * Created by FM on 28.05.16.
  */

trait EvaluateProgramWithAllImplementations  {

  this: FlatSpec =>

  def runInAllImplementations(program: Program)(testSpecifications: (=> EvaluationEngine) => Unit): Unit = {
    val config = BuildEngine.withProgram(program).useAsp()

    "Using Clingo-Pull" should behave like testSpecifications(config.withClingo().use().usePull().start())
    "Using Clingo-Push" should behave like testSpecifications(config.withClingo().use().usePush().start())

    "Using TMS-Pull" should behave like testSpecifications(config.withTms().use().usePull().start())
    "Using TMS-Push" should behave like testSpecifications(config.withTms().use().usePush().start())
  }

}
