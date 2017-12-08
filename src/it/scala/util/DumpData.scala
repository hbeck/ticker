package util

import java.io.File

/**
  * Created by FM on 08.08.16.
  */
case class DumpData(configCaption: String, instanceSizeCaption: String) {

  def printResults(filePath: String)(results: Seq[AlgorithmResult[TimingsConfigurationResult]]) {
    printToFile(new File(filePath)) { printer => 
      val captions = Seq(
        configCaption,
        instanceSizeCaption,
        "Append-Min [ms]",
        "Append-Max [ms]",
        "Append-Avg [ms]",
        "Append-Median [ms]",
        "Evaluate-Min [ms]",
        "Evaluate-Max [ms]",
        "Evaluate-Avg [ms]",
        "Evaluate-Median [ms]"
      )

      printer println (captions.mkString(";"))

      val resultStrings = results map (a => a.runs flatMap (r => Seq(a.caption, r.instanceCaption) ++ configResultFormatted(r)))

      resultStrings foreach (r => printer println (r.mkString(";")))
    }
  }

  def printSuccessResults(filePath: String)(results: Seq[AlgorithmResult[SuccessConfigurationResult]]): Unit = {
    printToFile(new File(filePath)) { printer =>
      val captions = Seq(
        configCaption,
        instanceSizeCaption,
        "Timepoint",
        "Found Model"
      )

      printer.println(captions.mkString(";"))

      val resultStrings = results.flatMap(a => a.runs flatMap (r => r.successFailures map {
        case (time, success) => Seq(a.caption, r.instanceCaption, time.toString, success.toString)
      }))

      resultStrings foreach (r => printer.println(r.mkString(";")))
    }
  }

  def configResult(config: TimingsConfigurationResult) = {
    config.appendResult.asResult() ++ config.evaluateResult.asResult()
  }

  def configResultFormatted(config: TimingsConfigurationResult) = {
    configResult(config) map (_.formatted("%f"))
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }


  def failureComputed(wasModelComputed: Boolean) = true.compareTo(wasModelComputed)


}
