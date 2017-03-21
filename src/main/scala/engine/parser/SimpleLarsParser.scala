package engine.parser

import engine.parser.wrapper._

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class SimpleLarsParser extends JavaTokenParsers {

  /* Possibly remove comments from the input beforehand. See utils.Tokenizer for that. */

  override val skipWhitespace = false

  def program: Parser[Any] = rep(comment)~rep(importN)~rep(comment)~rule~rep(rule)~rep(comment)

  def importN: Parser[Any] = "import"~space~str~opt("("~optSpace~str~optSpace~")")~space~"as"~space~str~newline~rep(newline)

  def rule: Parser[Any] = rep(comment)~((opt(head)~optSpace~":-"~optSpace~body)|head)~"."~rep(comment)~rep(newline)

  def head: Parser[Any] = atAtom | atom

  def body: Parser[Any] = repsep(bodyAtom,",")

  def atom: Parser[AtomWrapper] = optSpace~>(lowChar~opt(str))~opt("("~>repsep(upperChar,",")<~")") ^^ {
    case pre~dicate~params => AtomWrapper(pre.toString+dicate.toString,params.get)
  }

  def atAtom: Parser[AtAtomWrapper] = atom~space~"at"~space~(number|(upperChar~rep(str))) ^^ {
    case atom~_~_~_~time => AtAtomWrapper(atom,time.toString)
  }

  def wAtom: Parser[Any] = head~opt(space~"always"~optSpace)~opt(space~"in"~space)~window

  def bodyAtom: Parser[Any] = opt(neg)~( wAtom | head | operation)

  def window: Parser[Any] = "["~str~opt(space~param~opt(","~param~opt(","~param)))~"]"

  def operand: Parser[Any] = optSpace~(upperChar | number)~optSpace

  def arithmetic: Parser[Any] = "+"|"-"|"/"|"*"

  def compare: Parser[Any] = "=="|">="|"<="|"!="

  def operation: Parser[Any] = repsep(operand,operator)~"="~repsep(operand,operator)

  def operator: Parser[Any] = arithmetic | compare

  def param: Parser[Any] = optSpace~number~opt(space~str)

  def neg: Parser[Any] = optSpace~"not"~space

  def number: Parser[Any] = floatingPointNumber

  def digit: Parser[Any] = """[0-9]""".r

  def newline: Parser[Any] = "\n" | "\r"

  def str: Parser[Any] = char~rep(char|digit)

  def char: Parser[Any] = lowChar | upperChar

  def lowChar: Parser[Any] = """[a-z]""".r

  def upperChar: Parser[Any] = """[A-Z]""".r

  def comment: Parser[Any] = lineComment | blockComment

  def lineComment: Parser[Any] = ("//" | "%")~optSpace~repsep(str,space)~newline

  def blockComment: Parser[Any] = ("/*"~optSpace~repsep(str,space)~"*/" | "%*"~optSpace~repsep(str,space)~"*%")~rep(newline)

  def optSpace: Parser[Any] = rep(" ")

  def space: Parser[Any] = "( )+".r
}