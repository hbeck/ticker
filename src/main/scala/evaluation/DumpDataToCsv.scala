package evaluation

import java.io.File

/**
  * Created by FM on 08.08.16.
  */
object DumpDataToCsv {

  def printResults(filePath:String)(additionalCaptions: Seq[String], results: Seq[(Seq[String], (StatisticResult, StatisticResult))]) {
    printToFile(new File(filePath)) { p =>
      val captions = additionalCaptions ++ Seq(
        "Append-Min [ms]",
        "Append-Max [ms]",
        "Append-Avg [ms]",
        "Append-Median [ms]",
        "Evaluate-Min [ms]",
        "Evaluate-Max [ms]",
        "Evaluate-Avg [ms]",
        "Evaluate-Median [ms]"
      )

      p.println(captions.mkString(";"))

      val resultStrings = results map (r => r._1 ++ r._2._1.asResult() ++ r._2._2.asResult())

      resultStrings foreach (r => p.println(r.mkString(";")))
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}
