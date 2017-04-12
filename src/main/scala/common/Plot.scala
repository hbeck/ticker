package common

import java.io.File

import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.Style.FillStyle
import org.sameersingh.scalaplot._
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter

import scala.io.Source

/**
  * Created by fm on 12/04/2017.
  */
object Plot {
  def main(args: Array[String]): Unit = {
    val plot = Plot(File.createTempFile("plot", "testrun").getParent)

    val s = plot.simpleBarChart("test", Seq(("a", 1), ("b", 2), ("c", 1)))
    val g = plot.groupedBarChart("test",
      Seq(
        ("greedy", Seq(("a", 1), ("b", 2), ("c", 1))),
        ("doyle", Seq(("a", 2), ("b", 3), ("c", 4))),
        ("clingo", Seq(("a", 3), ("b", 1), ("c", 2)))
      ),
      Options("bar foo")
    )

    println(s.png().renderedFile)

    println(g.png().renderedFile)

  }
}

case class Options(yAxisTitle: Option[String] = None) {
  def configure(chart: BarChart) = {
    yAxisTitle.foreach(chart.y.label = _)
  }
}

case class Plot(outputDirectory: String, generateRandomFilename: Boolean = true) {

  private val outputDirectoryFile = {
    val file = new File(outputDirectory)
    file.mkdirs()
    file.mkdir()
    file
  }


  def simpleBarChart(title: String, bars: Seq[(String, Double)], options: Options = Options()): Plotter = {
    val series = new MemBarSeries(bars.map(_._2))
    val data = new BarData(i => bars(i)._1, Seq(series))

    val chart = new BarChart(title, data)

    chart.y.range_=(0, bars.map(_._2).max)

    options.configure(chart)

    val file = generateOutputFiles(title, "simpleBar")

    Plotter(chart, file)
  }

  def groupedBarChart(title: String, groups: Seq[(String, Seq[(String, Double)])], options: Options = Options()): Plotter = {
    val series = groups.map { case (name, values) =>
      new MemBarSeries(values.map(_._2), name)
    }
    val axisCaptions = groups.map(_._2).head

    val data = new BarData(i => axisCaptions(i)._1, series)

    data.serieses.foreach { s =>
      s.fillStyle = FillStyle.Pattern
    }
    val chart = new BarChart(title, data)
    chart.showLegend = true
    chart.legendPosX = LegendPosX.Left
    chart.legendPosY = LegendPosY.Top

    val maxValue = series.flatMap(_.ys).max

    chart.y.range_=(0.0, maxValue)

    options.configure(chart)

    val file = generateOutputFiles(title, "groupedBar")

    Plotter(chart, file)
  }

  private def generateOutputFiles(prefix: String, postfix: String) = {

    val file = if (generateRandomFilename)
      java.io.File.createTempFile(prefix, postfix, outputDirectoryFile)
    else
      new File(outputDirectoryFile, prefix + postfix)

    OutputFiles(outputDirectoryFile.getAbsolutePath + java.io.File.separator, file.getName, file.getAbsolutePath + ".gpl")
  }

  case class OutputFiles(directory: String, name: String, gplFileName: String) {
    def as(extension: String) = new java.io.File(directory, name + "." + extension).getAbsolutePath
  }

  case class Plotter(chart: Chart, files: OutputFiles) {
    val plotter = new GnuplotPlotter(chart)

    def png() = {
      plotter.png(files.directory, files.name)

      PlotResult(Source.fromFile(files.gplFileName).mkString, files.gplFileName, files.as("png"))
    }

    def latex() = {

      val sizeString = if (chart.size.isDefined) "size %f,%f" format(chart.size.get._1, chart.size.get._2) else ""
      val terminal = "latex %s" format (sizeString)
      plotter.writeScriptFile(files.directory, files.name, terminal, "png")
      plotter.runGnuplot(files.directory, files.name)

      PlotResult(Source.fromFile(files.gplFileName).mkString, files.gplFileName, files.as("tex"))
    }
  }

}


case class PlotResult(plot: String, sourceFile: String, renderedFile: String)
