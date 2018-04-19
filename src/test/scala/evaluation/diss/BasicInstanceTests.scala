package evaluation.diss

import org.scalatest.FunSuite

class BasicInstanceTests extends FunSuite {

  test("basic instance") {
    runAllConfigs("basic_w")
  }

  test("basic dual instance") {
    runAllConfigs("basic_dual_w")
  }

  val stdArgs = "verify true reasoner incr timepoints 100 winsize 10 pre 0 runs 1 header false "

  def runAllConfigs(prefix: String): Unit = {
    for (window <- Seq("t","c")) {
      for (mod <- Seq("a","d","b")) {
        for (signalEvery <- Seq("1", "2", "10", "20")) {
          val instName = prefix+window+mod+"_"+signalEvery
          val args = (stdArgs + "inst " + instName).split(" ")
          DissEvalMain.main(args)
        }
      }
    }
  }

  test("specific") {
    val instName = "basic_dual_wcb_20"
    val args = (stdArgs + "inst " + instName).split(" ")
    DissEvalMain.main(args)
  }

}
