package reasoner.parser

import core.lars._
import reasoner.parser.factories._
import reasoner.parser.wrapper.{OperationWrapper, ParamWrapper}

import scala.util.parsing.combinator.JavaTokenParsers
/**
   * Created by et on 16.03.17.
   */
class LarsLexer extends JavaTokenParsers {

  /* Possibly remove comments from the input beforehand. See fromScratch.utils.Tokenizer for that. */

  override val skipWhitespace = false

  def program: Parser[ProgramFactory] = rep(comment) ~> rep1(rule) <~ rep(comment) ^^ (rl => ProgramFactory(rl))

  def rule: Parser[RuleFactory] = rep(comment) ~> ruleBase <~ "." <~ optSpace <~ rep(comment) <~ rep(newline)

  def ruleBase: Parser[RuleFactory] = (head ~ optSpace ~ ":-" ~ optSpace ~ body <~ optSpace ^^ {
    case h ~ _ ~ _ ~ _ ~ b => RuleFactory(h,b)
  }
    | head ^^ (h => RuleFactory(h, List())))

  def head: Parser[AtomTrait] = atAtom | atom

  def body: Parser[List[BodyTrait]] = repsep(bodyElement,",")

  def bodyElement: Parser[BodyTrait] = operation | wAtom | head

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

  def window: Parser[WindowFactory] = optSpace ~> "[" ~> optSpace ~> integer ~ opt(space ~> str <~ optSpace) ~ "]" ^^ {
    case num ~ None ~ _ => WindowFactory("t",List(ParamWrapper(num)))
    case num ~ unit ~ _ => unit.get match {
      case "#" => WindowFactory(unit.get,List(ParamWrapper(num)))
      case _ => WindowFactory("t",List(ParamWrapper(num,unit.get)))}
  }

  def atAtom: Parser[AtAtomFactory] = atom ~ space ~ neg ~ "at" ~ space ~ (float ^^ (f => f.toString)
    |upperChar ~ opt(str) ^^ {
    case c ~ None => c.toString
    case c ~ str => c+""+str.get
  }) ^^ {
    case atom ~ _ ~ not ~ _ ~ _ ~ time => AtAtomFactory(not,atom,time)
  }

  def atom: Parser[AtomFactory] = neg ~ optSpace ~ predicate ~ opt("(" ~> repsep(variable|float,",") <~ ")") ^^ {
    case not ~ _ ~ pred ~ None => AtomFactory(not,pred,List())
    case not ~ _ ~ pred ~ params => AtomFactory(not, pred, params.get)
  }

  def predicate: Parser[String] = lowChar ~ opt(str) ^^ {
    case l ~ None => l.toString
    case l ~ r => l+""+r.mkString
  }

  def neg: Parser[Boolean] = optSpace ~> opt("not" <~ space) ^^ {
    case None => false
    case _ => true
  }

  def operation: Parser[OperationFactory] = rightOperation | leftOperation

  def leftOperation: Parser[OperationFactory] = (arithOperation | operand ^^ (o => OperationWrapper(o,None,None))) ~ compare ~ operand ^^ {
    case ao ~ op ~ o => OperationFactory(ao,op,o)
  }

  def rightOperation: Parser[OperationFactory] = operand ~ compare ~ (arithOperation | operand ^^ (o => OperationWrapper(o,None,None))) ^^ {
    case o ~ op ~ ao => OperationFactory(o,op,ao)
  }

  def arithOperation: Parser[OperationWrapper] = operand ~ arithmetic ~ operand ^^ {
    case l ~ op ~ r => OperationWrapper(l,Some(op),Some(r))
  }

  def operand: Parser[ArgumentFactory] = optSpace ~> (variable ^^ { o: String => ArgumentFactory(o) }
    | float ^^ { o: Double => ArgumentFactory(o) }) <~ optSpace

  def arithmetic: Parser[String] = optSpace ~> ("+"|"-"|"/"|"*"|"%"|"^") <~ optSpace

  def compare: Parser[String] = optSpace ~> ("="|">="|"<="|"!="|"<"|">") <~ optSpace

  def variable: Parser[String] = upperChar ~ opt(str|integer) ^^ {
    case c ~ None => c.toString
    case c ~ str => c+""+str.mkString
  }

  def str: Parser[String] = rep1(char) ^^ (str => str.mkString)

  def char: Parser[Char] = """[a-zA-Z0-9_#@]""".r ^^ (_.head)

  def upperChar: Parser[Char] = """[A-Z]""".r ^^ (_.head)

  def lowChar: Parser[Char] = """[a-z]""".r ^^ (_.head)

  def float: Parser[Double] = integer ~ opt("." ~ integer) ^^ {
    case num ~ None => num.mkString.toDouble
    case num ~ dec => (num.mkString + dec.get._1 + dec.get._2).toDouble
  }

  def integer: Parser[String] = rep1(digit) ^^ (_.mkString)

  def digit: Parser[String] = """[0-9]""".r

  def comment: Parser[String] = lineComment | blockComment | """[\n\r]""".r

  def lineComment: Parser[String] = """(\/\/|%).*?(\n|\r)+?""".r

  def blockComment: Parser[String] = """(((\/\*)|%\*)(.|\n|\r)*?((\*\/)|\*%))""".r

  def optSpace: Parser[String] = """ *""".r

  def space: Parser[String] = rep1(" ") ^^ (_.toString)

  def newline: Parser[String] = "\n" | "\r"
}
