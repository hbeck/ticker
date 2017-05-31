package engine.parser.fromScratch.expressions

import engine.parser.InvalidSyntaxException
import engine.parser.fromScratch.Tokenizer

import scala.util.matching.Regex

/**
  * Created by et on 11.03.17.
  */
@deprecated
sealed trait Expression {

  val importStmt = "import"

  //TODO

  override def toString: String = "This is an expression."
}

@deprecated
case class ImportExpression(importp: String) extends Expression {
  private val stripped = importp.substring(importStmt.length())
  private val split = stripped.split(" as ")

  val windowOperator: String = split(0)
  val windowName: String = split(1)
}

@deprecated
case class ProgramExpression(rows: List[String]) extends Expression {
  private val importList = rows.filter(_.startsWith(importStmt))
  val imports:List[ImportExpression] = createImports(importList)

  private def createImports(lst: List[String]): List[ImportExpression] = lst match {
    case Nil => Nil
    case x::xs => ImportExpression(x) :: createImports(xs)
  }

  private val ruleList = rows.filterNot(_.startsWith(importStmt))
  val rules:List[RuleExpression] = createRules(ruleList)

  private def createRules(lst: List[String]): List[RuleExpression] = lst match {
    case Nil => Nil
    case x::xs => RuleExpression(x) :: createRules(xs)
  }

}

@deprecated
case class RuleExpression(rule: String) extends Expression {
  private val msg = String.format("%s\n%s","Cannot find end of rule.",rule)
  if(!rule.last.equals('.')) throw new InvalidSyntaxException(msg)

  private val ruleExp = createExp(rule.split(":-").toList)

  val head: HeadExpression = ruleExp._1
  val body: BodyExpression = ruleExp._2

  @throws[InvalidSyntaxException]
  private def createExp(parts: List[String]): (HeadExpression,BodyExpression) = parts match {
    case Nil  => throw new InvalidSyntaxException("Empty rule.")
    case x::xs => (createHead(x),createBody(xs))
  }

  def createHead(atom: String): HeadExpression = HeadExpression(atom)

  def createBody(atoms: List[String]): BodyExpression = atoms match {
    case Nil => BodyExpression("")
    case x::_ => BodyExpression(x)
  }
}

@deprecated
case class HeadExpression(headAtom: String) extends Expression {
  val head:Expression = createHeadAtomExp(headAtom, checkAtAtom(headAtom))

  private def checkAtAtom(head: String): Option[AtAtomExpression] = {
    if(head.contains(" at ")) return Option(AtAtomExpression(head))
    None
  }

  private def createHeadAtomExp(head: String, atatom: Option[AtAtomExpression]): Expression = atatom match {
    case None => AtomExpression(head)
    case atomOpt => atomOpt.get
  }
}

@deprecated
case class BodyExpression(body: String) extends Expression {
  private val atoms = body.split(',').toList

  def findAtoms(atoms: List[String]): List[AtomExpression] = atoms match {
    case Nil => Nil
    case x::xs => checkAtom(x) ++ findAtoms(xs)
  }

  def checkAtom(atom: String): List[AtomExpression] = {
    val regex = new Regex("""([A-Za-z]\w+)\([A-Z]+\)""")
    if(regex.findFirstIn(atom).isDefined) List(AtomExpression(atom))
    Nil
  }

  def checkAtAtom(atom: String) = {
    val regex = new Regex("""([A-Za-z]\w+)\([A-Z]+\) at ([A-Z]+|[0-9]+)""")
  }

//  def findAtAtoms(atoms: List[String]): List[AtAtomExpression] = atoms match {
//    case Nil => Nil
//    case x::xs => checkAtAtom(x) ++ findAtoms(xs)
//  }

  def findWAtoms(atoms: List[String]): List[WindowAtomExpression] = {Nil}

//  private var result:(List[String],List[Expression]) = findWAtoms(atoms)

  val wAtomLst: List[WindowAtomExpression] = findWAtoms(atoms)
//  val atAtomLst: List[AtAtomExpression] = findAtAtoms(atoms)
  val atomLst: List[AtomExpression] = findAtoms(atoms)
  //TODO
}

@deprecated
case class AtomExpression(atom: String) extends Expression {
  //TODO
}

@deprecated
case class AtAtomExpression(atom: String) extends Expression {

  //TODO
}

@deprecated
case class WindowAtomExpression(atoms: String) extends Expression {
//  val atom = AtomExpression()
//  val window = WindowExpression()
  //TODO

}

@deprecated
case class WindowExpression() extends Expression {
  val wtype = ""
  val params = WindowParamExpression()
  //TODO
}

@deprecated
case class WindowParamExpression() extends Expression {
  val params:Map[String,Int] = Map()
  //TODO
}

@deprecated
case class ArithmeticOperationExpression() extends Expression {
  //TODO
}

//add more as needed