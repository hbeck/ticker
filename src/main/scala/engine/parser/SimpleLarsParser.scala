package engine.parser

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class SimpleLarsParser extends JavaTokenParsers {

  /* Possibly remove comments beforehand. See utils.Tokenizer for that. */

  override val skipWhitespace = false

  def program: Parser[Any] = rep(comment)~rep(importN)~rep(comment)~rule~rep(rule)~rep(comment)

  def importN: Parser[Any] = "import"~space~str~opt("("~optSpace~str~optSpace~")")~space~"as"~space~str~newline~rep(newline)
//  def importN: Parser[Any] = "import"~" "~str~" "~"as"~" "~str

  def rule: Parser[Any] = rep(comment)~((opt(head)~optSpace~":-"~optSpace~body)|head)~"."~rep(comment)~rep(newline)

  def head: Parser[Any] = atAtom | atom

  def body: Parser[Any] = repsep(bodyAtom,",")

  def atom: Parser[Any] = optSpace~lowChar~opt("("~repsep(upperChar,",")~")")//~optSpace //~opt("at"~(number|upperChar))~opt(opt("alsways")~" in "~window)

  def atAtom: Parser[Any] = atom~space~"at"~space~(number|(upperChar~rep(str)))

  def wAtom: Parser[Any] = head~opt(" always")~space~"in"~space~window

  def bodyAtom: Parser[Any] = opt(neg)~( wAtom | head | operation)

  def window: Parser[Any] = "["~str~opt(space~param~opt(","~param~opt(","~param)))~"]"

//  def arithmeticOp: Parser[Any] = operand~arithmetic~operand~rep(arithmetic~operand)
//
//  def logicOp: Parser[Any] = operand|arithmeticOp~bool~operand|arithmeticOp

  def operand: Parser[Any] = optSpace~(upperChar | number)~optSpace

  def arithmetic: Parser[Any] = "+"|"-"|"/"|"*"

  def compare: Parser[Any] = "=="|">="|"<="|"!="

//  def operation: Parser[Any] = arithmeticOp | logicOp
  def operation: Parser[Any] = repsep(operand,operator)~"="~repsep(operand,operator)

  def operator: Parser[Any] = arithmetic | compare

  def param: Parser[Any] = optSpace~number~opt(space~str)

  def neg: Parser[Any] = optSpace~"not"~space

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

  def lineComment: Parser[Any] = ("//" | "%")~optSpace~repsep(str,space)~newline

  def blockComment: Parser[Any] = ("/*"~optSpace~repsep(str,space)~"*/" | "%*"~optSpace~repsep(str,space)~"*%")~rep(newline)

  def optSpace: Parser[Any] = rep(" ")

  def space: Parser[Any] = "( )+".r
}