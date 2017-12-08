package engine.parser


import java.net.URL

import core.lars.LarsProgram
import engine.parser.factories.ProgramFactory

import scala.io.Source

/**
  * Created by et on 10.03.17.
  *
  * This is the main class (for the LARS syntax parser.
  * .lars files will be read here and all the instances of wrapper and helper classes will be created here.
  *
  */
object LarsParser extends LarsLexer {

  def apply(path: URL): LarsProgram = {
    programFactory(readFile(path)).program
  }

  def apply(path: String): LarsProgram = {
    val programString = readFile(path)
    programFactory(programString).program
  }

  def fromString(programString: String): LarsProgram = {
    programFactory(programString).program
  }

  private def readFile(path: String): String = {
    val url = Option(getClass.getResource(path))
    if (url.isEmpty) throw new Exception("File at path " + path + " could not be found.")

    readFile(getClass.getResource(path))
  }

  private def readFile(url: URL): String = {
    val source = Source.fromURL(url)
    try source.mkString finally source.close()
  }

  @throws[InvalidSyntaxException]
  private def programFactory(programString: String): ProgramFactory = parseAll(program, programString) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) =>
      throw new InvalidSyntaxException("Failed at line %s, column %s: %s".format(
        next.pos.line, next.pos.column, msg))
  }
}
