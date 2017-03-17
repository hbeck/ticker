package engine.parser

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class SimpleLarsParser extends JavaTokenParsers {

  override val skipWhitespace = false

  def program: Parser[Any] = rep(comment)~rep(importN)~rep(comment)~rule~rep(rule)~rep(comment)

  def importN: Parser[Any] = "import "~str~opt("("~optSpace~str~optSpace~")")~" as "~str
//  def importN: Parser[Any] = "import"~" "~str~" "~"as"~" "~str

  def rule: Parser[Any] = rep(comment)~((opt(head)~":-"~body)|head)~"."~rep(comment)

  def head: Parser[Any] = atom | atAtom

  def body: Parser[Any] = bodyAtom~repsep(bodyAtom,",")

  def atom: Parser[Any] = optSpace~lowChar~opt("("~upperChar~repsep(upperChar,",")~")")~optSpace //~opt("at"~(number|upperChar))~opt(opt("alsways")~" in "~window)

  def atAtom: Parser[Any] = atom~" at "~(number|(upperChar~rep(str)))

  def wAtom: Parser[Any] = head~opt(" always")~" in "~window

  def bodyAtom: Parser[Any] = opt(neg)~(head | wAtom | operation)

  def window: Parser[Any] = "["~str~opt(" "~param~opt(","~param~opt(","~param)))~"]"

  def arithmeticOp: Parser[Any] = operand~arithmetic~operand~rep(arithmetic~operand)

  def logicOp: Parser[Any] = operand|arithmeticOp~bool~operand|arithmeticOp

  def operand: Parser[Any] = upperChar | number

  def arithmetic: Parser[Any] = "+"~"-"~"/"~"*"

  def bool: Parser[Any] = "="~">="~"<="~"!="

  def operation: Parser[Any] = arithmetic | bool

  def param: Parser[Any] = optSpace~number~" "~opt(str)

  def neg: Parser[Any] = optSpace~"not "

  def number: Parser[Any] = floatingPointNumber //digit~rep(digit)

  def digit: Parser[Any] = """[0-9]""".r //"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"

  def newline: Parser[Any] = "\n" | "\r"

  def str: Parser[Any] = /*"""[a-z][a-z0-9]*""".r*/ char~rep(char|digit)

  def char: Parser[Any] = /*"""[a-z]""".r | """[A-Z]""".r*/ lowChar | upperChar

  def lowChar: Parser[Any] = """[a-z]""".r /*"a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|
     "j"|"k"|"l"|"m"|"n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|"w"|"x"|"y"|"z"*/

  def upperChar: Parser[Any] = """[A-Z]""".r /*"A"|"B"|"C"|"D"|"E"|"F"|"G"|"H"|"I"|
     "J"|"K"|"L"|"M"|"N"|"O"|"P"|"Q"|"R"|"S"|"T"|"U"|"V"|"W"|"X"|"Y"|"Z"*/

  def comment: Parser[Any] = lineComment | blockComment

  def lineComment: Parser[Any] = "//" | "%"~str~newline

  def blockComment: Parser[Any] = "/*"~str~"*/" | "%*"~str~"*%"

  def optSpace: Parser[Any] = rep(" ")

}
