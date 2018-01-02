package reasoner.asp.clingo

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import scala.sys.process._

object ClingoWrapper {

  def parseVersion(versionString: String) = {
    val regex = """^clingo version (\d\.\d\.\d)""".r("version")

    val matches = regex.findFirstMatchIn(versionString)

    if (matches.isEmpty)
      throw new IllegalArgumentException("Invalid clingo version string provided: " + versionString)

    matches.get.group("version")
  }

  def apply(executable: String = "clingo") = {
    val versionProcess = Process(executable :: List("--version"))

    val versionOutput = versionProcess.!!

    val clingoVersion = parseVersion(versionOutput)

    new ClingoWrapper(Process(executable, List("--warn=no-atom-undefined", "--verbose=0", "--models=0")), clingoVersion)
  }
}

class ClingoWrapper(val clingoProcess: ProcessBuilder, val clingoVersion: String) {

  def run(expressions: ClingoProgram): String = run(expressions.rules.mkString(System.lineSeparator))

  def run(program: String): String = {

    val inputStream = new ByteArrayInputStream(program.getBytes(StandardCharsets.UTF_8))
    val resultStream = clingoProcess.#<(inputStream).lineStream_!

    val result = resultStream.mkString(System.lineSeparator())

    result
  }

  def parseResult(result: String): Option[Set[ClingoModel]] = {

    if (result.endsWith("SATISFIABLE")) {
      val lines = result.linesWithSeparators
        // clingo outputs warnings for undefined atoms
        .filterNot(line => line.startsWith("-:") || line.startsWith(" ") || line.trim.isEmpty)
        .map(_.stripLineEnd)
        .toList


      val models = lines.init.map(line => line.split(' ').toSet)
      // An (satisfiable) but empty model is not a result for us
      if (models.nonEmpty)
        return Some(models.toSet)
    }

    None
  }
}
