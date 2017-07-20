package engine.parser


import java.net.URL

import core.lars.LarsProgram
import engine.parser.factories.ProgramFactory

import scala.io.Source

/**
  * Created by et on 10.03.17.
  *
  * This is the main class (it's actually an object, i know) for the LARS syntax (aka magenta) parser.
  * The way i imagine it the .lars files will be read here, and all the instances of wrapper and helper
  * classes will be created here.
  *
  * This class is a scala object, because i don't think there is any need for more than one instance of a parser.
  */
object LarsParser extends LarsLexer {

  def apply(path: URL): LarsProgram = {
    doTheThing(readFile(path)).program
  }

  def apply(input: String, isPath: Boolean = true): LarsProgram = {
    var program = input
    if (isPath) program = readFile(input)

    doTheThing(program).program
  }

  private def readFile(path: String): String = {
    var url = Option(getClass.getResource(path))
    if (url.isEmpty) throw new Exception("File at path " + path + " could not be found.")

    readFile(getClass.getResource(path))
  }

  private def readFile(url: URL): String = {
    val source = Source.fromURL(url)
    try source.mkString finally source.close()
  }

  @throws[InvalidSyntaxException]
  private def doTheThing(input: String): ProgramFactory = parseAll(program, input) match {
    case Success(result, _) => result
    case NoSuccess(msg, next) =>
      throw new InvalidSyntaxException("Failed at line %s, column %s: %s".format(
        next.pos.line, next.pos.column, msg))
  }
}
