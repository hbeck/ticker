package engine.parser.expressions

import java.util.concurrent.atomic.AtomicStampedReference

import engine.parser.utils.Tokenizer

/**
  * Created by et on 11.03.17.
  */
sealed trait Expression {

  val importStmt = "import"

  //TODO

  override def toString: String = "This is an expression."
}

case class ImportExpression(importp: String) extends Expression {
  private val stripped = importp.substring(importStmt.length())
  private val split = stripped.split(" as ")

  val windowOperator: String = split(0)
  val windowName: String = split(1)
}

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

case class RuleExpression(rule: String) extends Expression {
  private val msg = String.format("%s\n%s","Cannot find end of rule.",rule)
  if(!rule.last.equals('.')) throw new SyntaxException(msg)

  private val ruleExp = createExp(rule.split(":-").toList)

  val head: HeadExpression = ruleExp._1
  val body: BodyExpression = ruleExp._2

//  private var tmpHead = HeadExpression("")
//  private var tmpBody = BodyExpression("")
//
//  split.length match {
//    case 2 => tmpHead = HeadExpression(split(0)); tmpBody = BodyExpression(split(1))
//    case 1 => tmpHead = HeadExpression(split(0))
//    case _ => throw new SyntaxException(String.format("%s\n%s","Not a valid rule:",rule))
//  }
  private def createExp(parts: List[String]): (HeadExpression,BodyExpression) = parts match {
    case Nil  => throw new SyntaxException("Empty rule.")
    case x::xs => (createHead(x),createBody(xs))
  }

  def createHead(atom: String): HeadExpression = HeadExpression(atom)

  def createBody(atoms: List[String]): BodyExpression = atoms match {
    case Nil => BodyExpression("")
    case x::_ => BodyExpression(x)
  }
}

case class HeadExpression(head: String) extends Expression {
  val atom = AtomExpression()
  //TODO
}

case class BodyExpression(body: String) extends Expression {
  //TODO
}

case class AtomExpression() extends Expression {
  //TODO
}

case class AtAtomExpression() extends Expression {

  //TODO
}

case class WindowAtomExpression() extends Expression {
  val atom = AtomExpression()
  val window = WindowExpression()
  //TODO

}

case class WindowExpression() extends Expression {
  val wtype = ""
  val params = WindowParamExpression()
  //TODO
}

case class WindowParamExpression() extends Expression {
  val params:Map[String,Int] = Map()
  //TODO
}

case class ArithmeticOperationExpression() extends Expression {
  //TODO
}

//add more as needed
