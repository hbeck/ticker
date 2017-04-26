package engine.parser

import core.lars._
import engine.parser.factory._
import engine.parser.wrapper.{OperationWrapper, ParamWrapper}

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class SimpleLarsParser extends JavaTokenParsers {

  /* Possibly remove comments from the input beforehand. See fromScratch.utils.Tokenizer for that. */

  override val skipWhitespace = false

  def line: Parser[Any] = importN | rule

  def program: Parser[ProgramFactory] = rep(comment) ~> rep(importN) ~ rep(comment) ~ rep1(rule) <~ rep(comment) ^^ {
    case imp ~_ ~ rl => ProgramFactory(imp,rl)
  }

  def importN: Parser[ImportFactory] = "import" ~> space ~ fqdn ~ opt("("~> rep1sep(param,",") ~ optSpace <~ ")") ~ space ~ "as" ~ space ~ str <~ rep1(newline,newline) ^^ {
    case _ ~ str1 ~ None ~ _ ~ _ ~ _ ~ str2 => ImportFactory(str1,List(),str2)
    case _ ~ str1 ~ params ~ _ ~ _ ~ _ ~ str2 => ImportFactory(str1,params.get._1,str2)
  }

  def fqdn: Parser[String] = filepath | classpath

  def classpath: Parser[String] = rep1(str,"."|str) ^^ (str => str.mkString)

  def filepath: Parser[String] = "/" ~ rep1(str,"/"|str) ^^ {case root ~ str => root+""+str.mkString}

  def rule: Parser[RuleFactory] = rep(comment) ~> ruleBase <~ "." <~ optSpace <~ rep(comment) <~ rep(newline)

  def ruleBase: Parser[RuleFactory] = (head ~ optSpace ~ ":-" ~ optSpace ~ body <~ optSpace ^^ {
    case h ~ _ ~ _ ~ _ ~ b => RuleFactory(h,b)
  }
    | head ^^ (h => RuleFactory(h, List())))

  def head: Parser[AtomTrait] = atAtom | atom

  def body: Parser[List[BodyTrait]] = repsep(bodyElement,",")

  def bodyElement: Parser[BodyTrait] = operation | wAtom | head

  def atom: Parser[AtomFactory] = neg ~ optSpace ~ predicate ~ opt("(" ~> repsep(variable|float,",") <~ ")") ^^ {
    case not ~ _ ~ pred ~ None => AtomFactory(not,pred,List())
    case not ~ _ ~ pred ~ params => AtomFactory(not, pred, params.get)
  }

  def predicate: Parser[String] = lowChar ~ opt(str) ^^ {
    case l ~ None => l.toString
    case l ~ r => l+""+r.mkString
  }

  def atAtom: Parser[AtAtomFactory] = atom ~ space ~ neg ~ "at" ~ space ~ (float ^^ (f => f.toString)
    |upperChar ~ opt(str) ^^ {
    case c ~ None => c.toString
    case c ~ str => c+""+str.get
  }) ^^ {
      case atom ~ _ ~ not ~ _ ~ _ ~ time => AtAtomFactory(not,atom,time)
  }

  def wAtom: Parser[WAtomFactory] = boxWAtom | diamWAtom | atWAtom

  def boxWAtom: Parser[WAtomFactory] = atom ~ space ~ "always" ~ optNotIn ~ window ^^ {
    case atom ~ _ ~ _ ~ not ~ window => WAtomFactory(not,atom,Box,window)
  }

  def diamWAtom: Parser[WAtomFactory] = atom ~ optNotIn ~ window ^^ {
    case atom ~ not ~ window => WAtomFactory(not,atom,Diamond,window)
  }

  def atWAtom: Parser[WAtomFactory] = atAtom ~ optNotIn ~ window ^^ {
    case atAtom ~ not ~ window => WAtomFactory(not,atAtom,At(atAtom.atom.time),window)
  }

  def optNotIn: Parser[Boolean] = space ~ "not" ~ in ^^ (_ => true) |
                                space ~ "not" ~ space ^^ (_ => true) |
                                              opt(in) ^^ (_ => false)

  def in: Parser[Any] = space ~ "in" ~ space

  def window: Parser[WindowFactory] = optSpace ~> "[" ~> str ~ opt(space ~> repsep(param,",")) <~ "]" ^^ {
    case wType ~ None => WindowFactory(wType,List())
    case wType ~ lst  => WindowFactory(wType,lst.get)
  }

  def operand: Parser[ArgumentFactory] = optSpace ~> (variable ^^ { o: String => ArgumentFactory(o) }
                                                    | float ^^ { o: Double => ArgumentFactory(o) }) <~ optSpace

  def arithmetic: Parser[String] = optSpace ~> ("+"|"-"|"/"|"*"|"%"|"^") <~ optSpace

  def compare: Parser[String] = optSpace ~> ("="|">="|"<="|"!="|"<"|">") <~ optSpace

  def arithOperation: Parser[OperationWrapper] = operand ~ arithmetic ~ operand ^^ {
    case l ~ op ~ r => OperationWrapper(l,Some(op),Some(r))
  }

  def leftOperation: Parser[OperationFactory] = (arithOperation | operand ^^ (o => OperationWrapper(o,None,None))) ~ compare ~ operand ^^ {
    case ao ~ op ~ o => OperationFactory(ao,op,o)
  }

  def rightOperation: Parser[OperationFactory] = operand ~ compare ~ (arithOperation | operand ^^ (o => OperationWrapper(o,None,None))) ^^ {
    case o ~ op ~ ao => OperationFactory(o,op,ao)
  }

  def operation: Parser[OperationFactory] = rightOperation | leftOperation

  def param: Parser[ParamWrapper] = optSpace ~> float ~ opt(space ~ opt(str <~ optSpace)) ^^ {
    case num ~ None => ParamWrapper(num,None)
    case num ~ str => ParamWrapper(num,str.get._2)
  }
//(optSpace ^^ (_ => None) | opt(space ~> str <~ optSpace))
  def variable: Parser[String] = upperChar ~ opt(str|intStr) ^^ {
    case c ~ None => c.toString
    case c ~ str => c+""+str.mkString
  }

  def neg: Parser[Boolean] = optSpace ~> opt("not" <~ space) ^^ {
    case None => false
    case _ => true
  }

  def float: Parser[Double] = rep1(digit) ~ opt("." ~ intStr) ^^ {
    case num ~ None => num.mkString.toDouble
    case num ~ dec => (num.mkString + dec.get._1 + dec.get._2).toDouble
  }

  def intStr: Parser[String] = rep1(digit) ^^ (_.mkString)

  def digit: Parser[Int] = """[0-9]""".r ^^ (_.toInt)

  def newline: Parser[String] = "\n" | "\r"

  def str: Parser[String] = rep1(char) ^^ (str => str.mkString)

  def char: Parser[Char] = """[^\n\r\t +-/*%^<>=!,.:\[\]\{\}\(\)]""".r ^^ (_.head)

  def lowChar: Parser[Char] = """[a-z]""".r ^^ (_.head)

  def upperChar: Parser[Char] = """[A-Z]""".r ^^ (_.head)

  def comment: Parser[String] = lineComment | blockComment | """[\n\r]""".r

  def lineComment: Parser[String] = """(\/\/|%).*?(\n|\r)+?""".r

  def blockComment: Parser[String] = """(((\/\*)|%\*)(.|\n|\r)*?((\*\/)|\*%))""".r

  def optSpace: Parser[String] = """ *""".r //(_.toString)

  def space: Parser[String] = rep1(" ") ^^ (_.toString)
}
