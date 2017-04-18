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

  def program: Parser[ProgramFactory] = rep(comment) ~> rep(importN) ~ rep(comment) ~ rep1(rule) <~ rep(comment) ^^ {
    case imp ~_ ~ rl => ProgramFactory(imp,rl)
  }

  def importN: Parser[ImportFactory] = "import"~>space ~ fqdn ~ opt("("~> rep1sep(param,",") ~ optSpace <~ ")") ~ space ~ "as" ~ space ~ str <~ rep1(newline,newline) ^^ {
    case _ ~ str1 ~ None ~ _ ~ _ ~ _ ~ str2 => ImportFactory(str1,List(),str2)
    case _ ~ str1 ~ params ~ _ ~ _ ~ _ ~ str2 => ImportFactory(str1,params.get._1,str2)
  }

  def fqdn: Parser[String] = rep1(str,"."|str) ^^ (str => str.mkString)

  def rule: Parser[RuleFactory] = rep(comment) ~> ruleBase <~ "." <~ optSpace <~ rep(comment) <~ rep(newline)

  def ruleBase: Parser[RuleFactory] = (opt(head) ~ optSpace ~ ":-" ~ optSpace ~ body <~ optSpace ^^ {
    case h ~ _ ~ _ ~ _ ~ b => RuleFactory(h,b)
  }
    | head ^^ (h => RuleFactory(Some(h), List())))

  def head: Parser[AtomTrait] = atAtom | atom

  def body: Parser[List[BodyTrait]] = repsep(bodyElement,",")

  def bodyElement: Parser[BodyTrait] = operation | wAtom | head

  def atom: Parser[AtomFactory] = opt(neg) ~ optSpace ~ predicate ~ opt("(" ~> repsep(variable|number,",") <~ ")") ^^ {
    case not ~ _ ~ pred ~ None => AtomFactory(not.getOrElse(false),pred,List())
    case not ~ _ ~ pred ~ params => AtomFactory(not.getOrElse(false), pred, params.get)
  }

  def predicate: Parser[String] = lowChar ~ opt(str) ^^ {
    case l ~ None => l.toString
    case l ~ r => ""+l+r
  }

  def atAtom: Parser[AtAtomFactory] = atom ~ space ~ opt(neg) ~ "at" ~ space ~ (number|rep1(upperChar, str)) ^^ {
    case atom ~ _ ~ not ~ _ ~ _ ~ time => AtAtomFactory(not.getOrElse(false),atom,time.toString)
  }

  def wAtom: Parser[WAtomFactory] = boxWAtom | diamWAtom | atWAtom

  def boxWAtom: Parser[WAtomFactory] = atom ~ space ~ "always" ~ optIn ~ window ^^ {
    case atom ~ _ ~ _ ~ _ ~ window => WAtomFactory(atom,Some(Box),window)
  }

  def diamWAtom: Parser[WAtomFactory] = atom ~ optIn ~ window ^^ {
    case atom ~ _ ~ window => WAtomFactory(atom,Some(Diamond),window)
  }

  def atWAtom: Parser[WAtomFactory] = atAtom ~ optIn ~ window ^^ {
    case atAtom ~ _ ~ window => WAtomFactory(atAtom,None,window)
  }

  def optIn: Parser[Option[Any]] = opt(space ~ "in" ~ space)

  def window: Parser[WindowFactory] = optSpace ~ "[" ~> anyStr ~ opt(space ~> repsep(param,",")) <~ "]" ^^ {
    case wType ~ None => WindowFactory(wType,List())
    case wType ~ lst  => WindowFactory(wType,lst.get)
  }

  def operand: Parser[OperandFactory] = optSpace ~> (variable ^^ { o: String => OperandFactory(o) }
                                                    | number ^^ { o: Double => OperandFactory(o) }) <~ optSpace

  def arithmetic: Parser[String] = optSpace ~> ("+"|"-"|"/"|"*") <~ optSpace

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

  def param: Parser[ParamWrapper] = optSpace ~> number ~ opt(space ~> str) ^^ {
    case num ~ str => ParamWrapper(num,str)
  }

  def variable: Parser[String] = upperChar ~ opt(str|number) ^^ {
    case c ~ None => c.toString
    case c ~ str => c+""+str.mkString
  }

  def neg: Parser[Boolean] = optSpace ~> "not" <~ space ^^ (_ => true)

  def number: Parser[Double] = rep1(digit) ~ opt("." ~ rep1(digit)) ^^ {
    case integ ~ None => integ.mkString.toDouble
    case integ ~ dec => (integ.mkString + dec.mkString).toDouble
  }

  def digit: Parser[Int] = """[0-9]""".r ^^ (_.toInt)

  def newline: Parser[String] = "\n" | "\r"

  def str: Parser[String] = rep1(char, char | digit) ^^ (str => str.mkString)

  def anyStr: Parser[String] = rep1(anyChar) ^^ (str => str.mkString)

  def anyChar: Parser[Char] = """\S""".r ^^ (_.head)

  def char: Parser[Char] = lowChar | upperChar | '_'

  def lowChar: Parser[Char] = """[a-z]""".r ^^ (_.head)

  def upperChar: Parser[Char] = """[A-Z]""".r ^^ (_.head)

  def comment: Parser[Any] = lineComment | blockComment

  def lineComment: Parser[Any] = """((\/\/|%).*?(\n|\r))""".r

//  def lineComment: Parser[Any] = ("//" | "%") ~ optSpace ~ repsep(str,space) ~ newline

  def blockComment: Parser[Any] = """(((\/\*)|%\*)(.|\n|\r)*?((\*\/)|\*%))""".r

//  def blockComment: Parser[Any] = ("/*" ~ optSpace ~ repsep(repsep(str,space),"""\s+""") ~ "*/" | "%*" ~ optSpace ~ repsep(repsep(str,space),"""\s+""".r) ~ "*%") ~ rep(newline)

  def optSpace: Parser[String] = rep(" ") ^^ (_.toString)

  def space: Parser[String] = rep1(" ") ^^ (_.toString)
}
