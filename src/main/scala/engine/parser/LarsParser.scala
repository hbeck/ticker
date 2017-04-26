package engine.parser


import core.lars.LarsProgram
import engine.parser.factory.ProgramFactory

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
object LarsParser extends SimpleLarsParser {

  def apply(input: String, isPath: Boolean = true): LarsProgram = {
    var program = input
    if(isPath) program = readFile(input)

    doTheThing(program).program
  }

  private def readFile(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }

//  def doTheThing(input:String): ParseResult[ProgramFactory] = parseAll(program,input)
  @throws[InvalidSyntaxException]
  private def doTheThing(input:String): ProgramFactory = parseAll(program,input) match {
    case Success(result,_) => result
    case NoSuccess(msg,next) =>
      throw new InvalidSyntaxException("Failed at line %s, column %s: %s".format(
                next.pos.line, next.pos.column, msg))
  }
}
