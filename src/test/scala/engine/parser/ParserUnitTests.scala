package engine.parser

import org.scalatest.FlatSpec

/**
  * Created by et on 18.04.17.
  */
class ParserUnitTests extends FlatSpec {

  private val parser = LarsParser

  "program" should "be accepted" in {}
  "program" should "be rejected" in {}
  "importN" should "be accepted" in {}
  "importN" should "be rejected" in {}
  "fqdn" should "be accepted" in {}
  "fqdn" should "be rejected" in {}
  "rule" should "be accepted" in {}
  "rule" should "be rejected" in {}
  "ruleBase" should "be accepted" in {}
  "ruleBase" should "be rejected" in {}
  "head" should "be accepted" in {}
  "head" should "be rejected" in {}
  "body" should "be accepted" in {}
  "body" should "be rejected" in {}
  "bodyElement" should "be accepted" in {}
  "bodyElement" should "be rejected" in {}
  "atom" should "be accepted" in {}
  "atom" should "be rejected" in {}
  "predicate" should "be accepted" in {}
  "predicate" should "be rejected" in {}
  "atAtom" should "be accepted" in {}
  "atAtom" should "be rejected" in {}
  "wAtom" should "be accepted" in {}
  "wAtom" should "be rejected" in {}
  "boxWAtom" should "be accepted" in {}
  "boxWAtom" should "be rejected" in {}
  "diamWAtom" should "be accepted" in {}
  "diamWAtom" should "be rejected" in {}
  "atWAtom" should "be accepted" in {}
  "atWAtom" should "be rejected" in {}
  "optIn" should "be accepted" in {}
  "optIn" should "be rejected" in {}
  "window" should "be accepted" in {}
  "window" should "be rejected" in {}
  "operand" should "be accepted" in {}
  "operand" should "be rejected" in {}
  "arithmetic" should "be accepted" in {}
  "arithmetic" should "be rejected" in {}
  "compare" should "be accepted" in {}
  "compare" should "be rejected" in {}
  "arithOperation" should "be accepted" in {}
  "arithOperation" should "be rejected" in {}
  "leftOperation" should "be accepted" in {}
  "leftOperation" should "be rejected" in {}
  "rightOperation" should "be accepted" in {}
  "rightOperation" should "be rejected" in {}
  "operation" should "be accepted" in {}
  "operation" should "be rejected" in {}
  "param" should "be accepted" in {}
  "param" should "be rejected" in {}
  "variable" should "be accepted" in {}
  "variable" should "be rejected" in {}
//  "neg" should "be accepted" in {}
//  "neg" should "be rejected" in {}
//  "number" should "be accepted" in {}
//  "number" should "be rejected" in {}
//  "digit" should "be accepted" in {}
//  "digit" should "be rejected" in {}
//  "newline" should "be accepted" in {}
//  "newline" should "be rejected" in {}
//  "str" should "be accepted" in {}
//  "str" should "be rejected" in {}
//  "anyStr" should "be accepted" in {}
//  "anyStr" should "be rejected" in {}
  "anyChar" should "be accepted" in {
    assert(parser.parseAll(parser.anyChar,".").successful)
    assert(parser.parseAll(parser.anyChar,",").successful)
    assert(parser.parseAll(parser.anyChar,":").successful)
    assert(parser.parseAll(parser.anyChar,";").successful)
    assert(parser.parseAll(parser.anyChar,"_").successful)
    assert(parser.parseAll(parser.anyChar,"-").successful)
    assert(parser.parseAll(parser.anyChar,"!").successful)
    assert(parser.parseAll(parser.anyChar,"?").successful)
    assert(parser.parseAll(parser.anyChar,"$").successful)
    assert(parser.parseAll(parser.anyChar,"%").successful)
    assert(parser.parseAll(parser.anyChar,"&").successful)
    assert(parser.parseAll(parser.anyChar,"(").successful)
    assert(parser.parseAll(parser.anyChar,")").successful)
    assert(parser.parseAll(parser.anyChar,"[").successful)
    assert(parser.parseAll(parser.anyChar,"]").successful)
    assert(parser.parseAll(parser.anyChar,"{").successful)
    assert(parser.parseAll(parser.anyChar,"}").successful)
    assert(parser.parseAll(parser.anyChar,"/").successful)
    assert(parser.parseAll(parser.anyChar,"\\").successful)
    assert(parser.parseAll(parser.anyChar,"\"").successful)
    assert(parser.parseAll(parser.anyChar,"\'").successful)
    assert(parser.parseAll(parser.anyChar,"+").successful)
    assert(parser.parseAll(parser.anyChar,"*").successful)
    assert(parser.parseAll(parser.anyChar,"~").successful)
    assert(parser.parseAll(parser.anyChar,"=").successful)
    assert(parser.parseAll(parser.anyChar,"0").successful)
    assert(parser.parseAll(parser.anyChar,"1").successful)
    assert(parser.parseAll(parser.anyChar,"2").successful)
    assert(parser.parseAll(parser.anyChar,"3").successful)
    assert(parser.parseAll(parser.anyChar,"4").successful)
    assert(parser.parseAll(parser.anyChar,"5").successful)
    assert(parser.parseAll(parser.anyChar,"6").successful)
    assert(parser.parseAll(parser.anyChar,"7").successful)
    assert(parser.parseAll(parser.anyChar,"8").successful)
    assert(parser.parseAll(parser.anyChar,"9").successful)
    assert(parser.parseAll(parser.anyChar,"§").successful)
    assert(parser.parseAll(parser.anyChar,"#").successful)
    assert(parser.parseAll(parser.anyChar,"<").successful)
    assert(parser.parseAll(parser.anyChar,">").successful)
    assert(parser.parseAll(parser.anyChar,"@").successful)
  }
  "anyChar" should "be rejected" in {
    assert(!parser.parseAll(parser.anyChar," ").successful)
    assert(!parser.parseAll(parser.anyChar,"\r").successful)
    assert(!parser.parseAll(parser.anyChar,"\n").successful)
    assert(!parser.parseAll(parser.anyChar,"\t").successful)
  }
  "char" should "be accepted" in {
    assert(parser.parseAll(parser.char,"u").successful)
    assert(parser.parseAll(parser.char,"U").successful)
  }
  "char" should "be rejected" in {
    assert(!parser.parseAll(parser.char,"IUIUIU").successful)
    assert(!parser.parseAll(parser.char,"iuiuiu").successful)
  }
  "lowChar" should "be accepted" in {
    assert(!parser.parseAll(parser.lowChar,"u").successful)
  }
  "lowChar" should "be rejected" in {
    assert(!parser.parseAll(parser.lowChar,"U").successful)
  }
  "upperChar" should "be accepted" in {
    assert(!parser.parseAll(parser.upperChar,"U").successful)
  }
  "upperChar" should "be rejected" in {
    assert(!parser.parseAll(parser.upperChar,"u").successful)
  }
  "comment" should "be accepted" in {
    assert(parser.parseAll(parser.comment,"/*block comment\n * foo \n */").successful)
    assert(parser.parseAll(parser.comment,"// and a line comment\n").successful)
  }
//  "comment" should "be rejected" in {}
  "lineComment" should "accept c++-style comments" in {
    assert(parser.parseAll(parser.lineComment,"// foo and bar are words\n comment ends with line").successful)
  }
  "lineComment" should "accept latex-style comments" in {
    assert(parser.parseAll(parser.lineComment,"% foo and bar are words\n comment ends with line").successful)
  }
//  "lineComment" should "be rejected" in {}
  "newBlockComment" should "accept anything that is between /* */ or %* *% or any combination of the two" in {
    println(parser.parseAll(parser.blockComment,"/* foo\n * bar\n * next line\n *%"))
  }
  "blockComment" should "be accepted with c-style block comment" in {
    assert(parser.parseAll(parser.blockComment,"/* random text line one \n\r\r\r\n and in line n */").successful)
  }
  "blockComment" should "be accepted with %* ... *%" in {
    assert(parser.parseAll(parser.blockComment,"%* random text line one \n\r\r\r\n and in line n *%").successful)
  }
//  "blockComment" should "be rejected" in {}
  "optSpace" should "accept multiple space characters" in {
    assert(parser.parseAll(parser.optSpace,"       ").successful)
  }
  "optSpace" should "accept an empty string" in {
    assert(parser.parseAll(parser.optSpace,"").successful)
  }
  "optSpace" should "reject a string with characters other than space" in {
    assert(!parser.parseAll(parser.optSpace,"this is a string with non space characters").successful)
  }
  "A single or multiple space characters" should "be accepted" in {
    assert(parser.parseAll(parser.space,"       ").successful)
  }
  "Any non space character" should "be rejected" in {
    assert(!parser.parseAll(parser.optSpace,"this is a string with non space characters").successful)
  }
  "Empty string, i.e. zero space characters" should "be rejected" in {
    assert(!parser.parseAll(parser.space,"").successful)
  }
}