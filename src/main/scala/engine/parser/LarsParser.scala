package engine.parser


import java.util.StringTokenizer

import core.lars.LarsProgram
import engine.parser.expressions.Expression
import engine.parser.factory.ProgramFactory

import scala.util.parsing.combinator._
import scala.collection.immutable.TreeMap
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

  def apply(input: String, isPath: Boolean = true): Option[LarsProgram] = {
    var program = input
    if(isPath) program = readFile(input)

    val parsedProgram: ParseResult[ProgramFactory] = doTheThing(program)
    if(parsedProgram.successful) return Some(parsedProgram.get.program)
    None
  }

  def readFile(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }

  def doTheThing(input:String): ParseResult[ProgramFactory] = parseAll(program,input)

/*  def tokenize(program: String): Option[TreeMap[String,Expression]] = {
    val token = new StringTokenizer(program,"\n")
    //TODO
    None
  }*/
}
