package engine.parser


import java.util.StringTokenizer

import engine.parser.expressions.Expression

import scala.util.parsing.combinator._
import scala.collection.immutable.TreeMap

/**
  * Created by et on 10.03.17.
  *
  * This is the main class (it's actually an object, i know) for the LARS syntax (aka magenta) parser.
  * The way i imagine it the .lars files will be read here, and all the instances of wrapper and helper
  * classes will be created here.
  *
  * This class is a scala object, because i don't think there is any need for more than one instance of a parser.
  */
object LarsParser extends RegexParsers {

  def apply(inputPath: String): Boolean = {
    val program = readFile(inputPath)
    doTheThing(program)
  }

  def readFile(inputPath: String): String = {
    //TODO
    ""
  }

  def doTheThing(program:String): Boolean = {
    val tokens = tokenize(program)
    //TODO
    false
  }

  def tokenize(program: String): Option[TreeMap[String,Expression]] = {
    val token = new StringTokenizer(program,"\n")
    //TODO
    None
  }


  /* This is an example method for parsing strings. I took this from a tutorial as a starting point. */
  /* Grammatik: word -> [a-z]+ */
  /* By using three triple double-quotes we don't need to worry about escaping characters */
  def word: Parser[String]  = """[a-z]+""".r ^^ { _.toString }


}
