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

    /* This is shorter than the lines below and the associated method */
    regex.replaceAllIn(input,"")
  }
}
