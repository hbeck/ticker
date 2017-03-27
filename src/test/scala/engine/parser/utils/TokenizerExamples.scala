package engine.parser.utils

import org.scalatest.FlatSpec

/**
  * Created by et on 15.03.17.
  */
class TokenizerExamples extends FlatSpec {

    val input = "import bla as foo\n" +
      "import bar as rab\n\n" +
      "/* This is a block comment on one line*/\n" +
      "/* This is a block comment\n over multiple\n lines */\n" +
      "h :- f.\n" +
      "//This is a line comment\n" +
      "%This is an alternative line comment\n" +
      "%* This is an alternative block comment (using %*),\r" +
      "with a carriage return in between*%\n\n" +
      "a :- b.\n" +
      "b.\n" +
      "c :- d.\n" +
      "c :- a.\n"

    val output = Tokenizer.removeComments(input)
    println(output)
}
