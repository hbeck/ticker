package evaluation.diss

import org.scalatest.FunSuite

class ManualInstanceTests extends FunSuite {

  test("basic instance") {
    runAllBasicConfigs("basic_w")
  }

  test("basic dual instance") {
    runAllBasicConfigs("basic_dual_w")
  }

  val stdArgs = "verify true reasoner incr timepoints 100 winsize 10 pre 0 runs 1 header false"

  def runAllBasicConfigs(prefix: String): Unit = {
    for (window <- Seq("t","c")) {
      for (mod <- Seq("a","d","b")) {
        for (signalEvery <- Seq("1", "2", "9", "10", "11", "20")) {
          val instName = prefix+window+mod+"_"+signalEvery
          val args = (f"$stdArgs inst $instName").split(" ")
          DissEvalMain.main(args)
        }
      }
    }
  }

  test("join_wtb_1_10") {
    val instName = "join_wtb_1_10"
    val args = (f"$stdArgs inst $instName").split(" ")
    DissEvalMain.main(args)
  }

  test("reach_wtd_1_10") {
    val instName = "reach_wtd_1_10"
    val args = (f"$stdArgs inst $instName").split(" ")
    DissEvalMain.main(args)
  }

  //pending (stack overflow; probably hash clash)
  test("reach_wta_10_100") {
    val instName = "reach_wta_10_100"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

  test("reach2_wta_10_100") {
    val instName = "reach2_wta_10_100"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

  test("rp_wtd_e10_n20_p70") {
    val instName = "rp_wtd_e10_n20_p70"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

  test("lrp_wtd_e10_n20_p70") {
    val instName = "lrp_wtd_e10_n20_p70"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

  test("ra_wtd_n10_av.9_fe2_p95") {
    val instName = "ra_wtd_n10_av.9_fe2_p95"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

  test("tme_wtd_n10_a0.9") {
    val instName = "tme_wtd_n10_a0.9"
    val args = (f"$stdArgs inst $instName rand -1").split(" ")
    DissEvalMain.main(args)
  }

}
