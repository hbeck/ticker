package engine.parser.utils

import engine.parser.expressions.ProgramExpression

import scala.util.matching.Regex

/**
  * Created by et on 12.03.17.
  */
object Tokenizer {

  def apply(input: String): Unit = {
    val noCommentInput = removeComments(input)
    val rows = noCommentInput.split("\r|\n").toList
    val program = ProgramExpression(rows)
  }

  def removeComments(input: String): String = removeCommentsRegex(input)

  /** Third attempt to remove comments from string
    * This looks surprisingly functional
    * */
  private def removeCommentsRegex(input: String): String = {
    /* The regex below matches c++-style block and line comments and also %* *% as block comment and % as line comment.
     * Maybe add carriage return in addition to line feed. - done?*/
    val regex = new Regex("""(((\/\*)|%\*)(.|\n|\r)*?((\*\/)|\*%))|((\/\/|%).*?(\n|\r))""")
    val comments = regex.findAllIn(input).toList
    remCom(input,comments)
  }

  private def remCom(input: String, comments: List[String]): String = comments match {
    case Nil => input
    case x::xs => remCom(input.replace(x,""),xs)
  }

  /* First attempt to remove comments from string */
//  def removeCommentsNaive(input: String): String = {
//    var out = ""
//    var flag = false
//    val iter = input.iterator
//    while(iter.hasNext) {
//      val c = iter.next
//      if (c.equals('/')) {
//        if(iter.hasNext){
//          val c2 = iter.next
//          if(c2.equals('*')) {
//            flag = true
//          }
//        }
//      } else if(c.equals('*')) {
//        if(iter.hasNext){
//          val c2 = iter.next
//          if(c2.equals('/')) {
//            flag = false
//          }
//      } else if(!flag) {
//         out += iter.next
//        }
//      }
//    }
//    ""
//  }
//
///* Second try to remove Comments from String */
//  def checkCommentEnd(tail: List[Char]): List[Char] = tail.head match {
//    case '/' =>  checkComment(tail)
//    case _ => tail.head :: findEnd(tail)
//  }
//
//  def findEnd(tail: List[Char]): List[Char] = {
//    if (tail.isEmpty) return Nil
//    tail.head match {
//      case '*' => checkCommentEnd(tail)
//      case _ => tail.head :: findEnd(tail.tail)
//    }
//  }
//
//  def checkComment(tail: List[Char]): List[Char] = tail.head match {
//    case '*' => findEnd(tail.tail)
//    case _ =>
//  }
//
//  def rmCm(in: List[Char]): List[Char] = {
//    if(in.isEmpty) return Nil
//    in.head match  {
//      case '/' =>  checkComment(in)
//      case _ => in.head :: rmCm(in.tail)
//    }
//  }
//
//  def removeCommentsRecurse(input: String): String = rmCm(input.toList).toString

}
