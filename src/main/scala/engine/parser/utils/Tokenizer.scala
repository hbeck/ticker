package engine.parser.utils

import engine.parser.expressions.ProgramExpression

import scala.util.matching.Regex

/**
  * Created by et on 12.03.17.
  */
object Tokenizer {

  def apply(input: String): Unit = {
    val noBlockCommentInput = removeCommentsRegex(input)
    val rows = noBlockCommentInput.split("\r|\n").toList
    val program = ProgramExpression(rows)
  }

  /** Third attempt to remove comments from string
    * This looks surprisingly functional
    * */

  def remCom(input: String, comments: List[String]): String = {
    if(comments.isEmpty) return input
    remCom(input.replace(comments.head,""),comments.tail)
  }

  def removeCommentsRegex(input: String): String = {
    /* The regex below matches c++-style block and line comments and also %* *% as block comment and % as line comment.
     * Maybe add carriage return in addition to line feed.*/
    val regex = new Regex("""(((\/\*)|%\*)(.|\n)*?((\*\/)|\*%))|((\/\/|%).*?\n)""")
    val comments = regex.findAllIn(input).toList
    remCom(input,comments)
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
