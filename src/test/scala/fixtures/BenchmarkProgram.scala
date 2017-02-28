package fixtures

import org.scalatest.{Outcome, Suite}

/**
  * Created by FM on 28.05.16.
  */
trait BenchmarkProgram extends Suite {
  this: Suite =>

  override def withFixture(test: NoArgTest): Outcome = {

    println

    println("before " + test.name)

    try {

      profile.withWarmup(10) {
        BenchmarkProgram.super.withFixture(test)
      }

    } finally {

      println("after " + test.name)

    }

  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}
