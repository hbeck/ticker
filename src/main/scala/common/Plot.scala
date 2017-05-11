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
    line()

  }

  def line(): Unit = {
    val series = new MemXYSeries(Seq(1, 2, 3), Seq(1, 3, 4), "barfoo")
    val data = new XYData(series)
    val chart = new XYChart("test", data)
    val plotter = new GnuplotPlotter(chart)

    val file = File.createTempFile("plot", "testrun")
    plotter.png(file.getParent() + "/", file.getName)

    println(file)
  }

  def bar(): Unit = {
    val plot = Plot(File.createTempFile("plot", "testrun").getParent)

    val s = plot.simpleBarChart("test", Seq(("a", 1), ("b", 2), ("c", 1)))
    val g = plot.groupedBarChart("test",
      Seq(
        ("greedy", Seq(("a", 1), ("b", 2), ("c", 1))),
        ("doyle", Seq(("a", 2), ("b", 3), ("c", 4))),
        ("clingo", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("bar", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("foo", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("baz", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("blub", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("blib", Seq(("a", 3), ("b", 1), ("c", 2))),
        ("blob", Seq(("a", 3), ("b", 1), ("c", 2)))

      ),
      Options("bar foo")
    )

    println(s.png().renderedFile)

    println(g.png().renderedFile)
  }
}


object Color extends Enumeration {
  type Type = Value
  val Black, Grey = Value
}

trait FillStyles

case class Empty(customColor: Option[Color.Type] = None) extends FillStyles

case class Solid(customColor: Option[Color.Type] = None) extends FillStyles

case class Pattern(customColor: Option[Color.Type] = None) extends FillStyles

case class Options(yAxisTitle: Option[String] = None,
                   fillStyles: Seq[FillStyles] = Seq(
                     Empty(Some(Color.Black)),
                     Solid(Some(Color.Black)),
                     Solid(Some(Color.Grey)),
                     Pattern(Some(Color.Black)),
                     Pattern(Some(Color.Grey)),
                     Pattern(Some(Color.Black)),
                     Pattern(Some(Color.Grey))
                   )
                  ) {
  def configure(chart: BarChart) = {
    yAxisTitle.foreach(chart.y.label = _)

    var patternCounter = 1 // pattern 0 is just empty
    def asPattern(series: BarSeries) = {
      series.fillStyle = FillStyle.Pattern
      series.pattern = Some(patternCounter)
      patternCounter = patternCounter + 1
      if (patternCounter == 3)
        patternCounter = 4 // skip solid
    }

    chart.data.serieses.zipWithIndex.foreach { case (s, i) =>
      if (fillStyles.length > i)
        fillStyles(i) match {
          case Empty(None) => s.fillStyle = FillStyle.Empty
          case Empty(Some(color)) =>
            s.fillStyle = FillStyle.Empty
            s.color = convertColor(color)
          case Solid(None) => s.fillStyle = FillStyle.Solid
          case Solid(Some(color)) =>
            s.fillStyle = FillStyle.Solid
            s.color = convertColor(color)
          case Pattern(None) => asPattern(s)
          case Pattern(Some(color)) =>
            asPattern(s)
            s.color = convertColor(color)
        }
    }
  }

  private def convertColor(color: Color.Type) = org.sameersingh.scalaplot.Style.Color.withName(color.toString)

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
    val data = new BarData(i => "\"%s\"" format (bars(i)._1), Seq(series))

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

    val data = new BarData(i => "\"%s\"" format (axisCaptions(i)._1), series)

    val chart = new BarChart(title, data)
    chart.showLegend = true
    chart.pointSize = Some(4)
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

    def pdf() = {
      plotter.pdf(files.directory, files.name)

      PlotResult(Source.fromFile(files.gplFileName).mkString, files.gplFileName, files.as("pdf"))
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
