package engine.parser

import core.lars._
import engine.parser.factory._

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class SimpleLarsParser extends JavaTokenParsers {

  /* Possibly remove comments from the input beforehand. See utils.Tokenizer for that. */

  override val skipWhitespace = false

  def program: Parser[ProgramFactory] = rep(comment) ~> rep(importN) ~ rep(comment) ~ rule ~ rep(rule) <~ rep(comment) ^^ {
    case imp ~_ ~ r ~ lr => ProgramFactory(imp,List(r)++lr)
  }

  def importN: Parser[ImportFactory] = "import"~>space ~ str ~ opt("("~>optSpace ~ str ~ optSpace <~ ")") ~ space ~ "as" ~ space ~ str ~ newline <~ rep(newline) ^^ {
    case _ ~ str1 ~ None ~ _ ~ _ ~ _ ~ str2 ~ _ => {
      ImportFactory(str1,None,str2)
    }
    case _ ~ str1 ~ params ~ _ ~ _ ~ _ ~ str2 ~ _ => {
      val parameter = params.get._2 + params.get._1._1 + params.get._1._2
      ImportFactory(str1,Some(parameter.trim),str2)
    }
  }

  def rule: Parser[RuleFactory] = rep(comment) ~> ruleBase ~ "." <~ rep(comment) ~> rep(newline) ^^ {case r ~ _ => r}

  def ruleBase: Parser[RuleFactory] = (opt(head) ~ optSpace ~ ":-" ~ optSpace ~ body ^^ {case h ~ _ ~ _ ~ _ ~ b => RuleFactory(h,Some(b))}
    | head ^^ (h => RuleFactory(Some(h), None)))

  def head: Parser[AtomTrait] = atAtom | atom

  def body: Parser[BodyFactory] = repsep(bodyElement,",") ^^ BodyFactory

  def bodyElement: Parser[BodyTrait] = wAtom | head | operation

  def atom: Parser[AtomFactory] = opt(neg) ~ optSpace ~ predicate ~ opt("(" ~> repsep(upperChar,",") <~ ")") ^^ {
    case not ~ _ ~ pred ~ params => AtomFactory(not, pred, params.get)
  }

  def predicate: Parser[String] = lowChar ~ opt(str) ^^ { case l ~ r => ""+l+r }

  def atAtom: Parser[AtAtomFactory] = atom ~ space ~ opt(neg) ~ "at" ~ space ~ (number|(upperChar ~ rep(str))) ^^ {
    case atom ~ _ ~ not ~ _ ~ _ ~ time => AtAtomFactory(not,atom,time.toString)
  }

  def wAtom: Parser[WAtomFactory] = atom ~ opt(space ~> "always" <~ optSpace) ~ opt(space ~ "in" ~ space) ~ window ^^ {
    case atom ~ None ~ _ ~ win => WAtomFactory(atom,Some(Diamond),win)
    case atom ~ _ ~ _ ~ win => WAtomFactory(atom,Some(Box),win)
  } | atAtom ~ opt(space ~ "in" ~ space) ~ window ^^ {
    case atAtom ~ _ ~ win => WAtomFactory(atAtom,None,win)
  }

  //TODO can we abstract this and plug in?
  def window: Parser[WindowFactory] = "[" ~> str ~ opt(space ~> param ~ opt("," ~> param ~ opt("," ~> param))) <~ "]" ^^ {
    case wType ~ None                                   => WindowFactory(wType)
    case wType ~ params if params.get._2.isEmpty        => WindowFactory(wType,Some(params.get._1))
    case wType ~ params if params.get._2.get._2.isEmpty => WindowFactory(wType,Some(params.get._1),Some(params.get._2.get._1))
    case wType ~ params                                 => WindowFactory(wType,Some(params.get._1),Some(params.get._2.get._1),Some(params.get._2.get._2.get))
  }

  def operand: Parser[OperandFactory] = optSpace ~> (upperChar ^^ { o: Char => OperandFactory(o) }
                                                    | number ^^ { o: Double => OperandFactory(o) }) <~ optSpace

  def arithmetic: Parser[String] = "+"|"-"|"/"|"*"

  def compare: Parser[String] = "="|">="|"<="|"!="|"<"|">"

  //TODO do not allow arbitrary 'calculations', only single 'assignments' (like T = A + B, A + B = T) and (binary) relations
  def arithOperation: Parser[List[(String, OperandFactory)]] = operand ~ rep(arithmetic ~ operand) ^^ {
    case o ~ l => List(("", o)) ++ l.flatten
  }

  def operation: Parser[OperationFactory] = arithOperation ~ compare ~ arithOperation ^^ {
    case l1 ~ op ~ l2 => OperationFactory(l1,op,l2)
  }

  def operator: Parser[String] = arithmetic | compare

  def param: Parser[ParamFactory] = optSpace ~> number ~ opt(space ~> str) ^^ {
    case num ~ str => ParamFactory(num,str)
  }

  def neg: Parser[Any] = optSpace ~ "not" ~ space

  def number: Parser[Double] = floatingPointNumber ^^ (_.toDouble)

  def digit: Parser[Int] = """[0-9]""".r ^^ (_.toInt)

  def newline: Parser[String] = "\n" | "\r"

  def str: Parser[String] = char ~ rep(char|digit) ^^ {
    case c ~ str => c.toString+str.toString()
  }

  def char: Parser[Char] = lowChar | upperChar

  def lowChar: Parser[Char] = """[a-z]""".r ^^ (_.head)

  def upperChar: Parser[Char] = """[A-Z]""".r ^^ (_.head)

  def comment: Parser[Any] = lineComment | blockComment

  def lineComment: Parser[Any] = ("//" | "%") ~ optSpace ~ repsep(str,space) ~ newline

  def blockComment: Parser[Any] = ("/*" ~ optSpace ~ repsep(str,space) ~ "*/" | "%*" ~ optSpace ~ repsep(str,space) ~ "*%") ~ rep(newline)

  def optSpace: Parser[String] = rep(" ") ^^ (_.toString)

  def space: Parser[String] = "( )+".r
}
