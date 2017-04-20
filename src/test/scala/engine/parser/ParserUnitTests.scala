package engine.parser

import org.scalatest.FlatSpec

/**
  * Created by et on 18.04.17.
  */
class ParserUnitTests extends FlatSpec {

  private val parser = LarsParser
  private val arith = List("+","-","/","*","%","^")
  private val comp = List("=",">=","<=","!=","<",">")

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
//  "head" should "be accepted" in {}
//  "head" should "be rejected" in {}
  "body" should "be accepted" in {}
  "body" should "be rejected" in {}
//  "bodyElement" should "be accepted" in {}
//  "bodyElement" should "be rejected" in {}
  "atom" should "be accepted" in {}
  "atom" should "be rejected" in {}
  "predicate" should "be accepted" in {}
  "predicate" should "be rejected" in {}
  "atAtom" should "be accepted" in {}
  "atAtom" should "be rejected" in {}
//  "wAtom" should "be accepted" in {}
//  "wAtom" should "be rejected" in {}
  "boxWAtom" should "be accepted" in {}
  "boxWAtom" should "be rejected" in {}
  "diamWAtom" should "be accepted" in {}
  "diamWAtom" should "be rejected" in {}
  "atWAtom" should "be accepted" in {
    assert(parser.parseAll(parser.atWAtom,"a at T in [t]").successful)
    assert(parser.parseAll(parser.atWAtom,"a at T not in [t]").successful)
    assert(parser.parseAll(parser.atWAtom,"not a at T in [t]").successful)
    assert(parser.parseAll(parser.atWAtom,"not a at T not in [t]").successful)
    assert(parser.parseAll(parser.atWAtom,"a at T [t]").successful)
    assert(parser.parseAll(parser.atWAtom,"a at T [t 5]").successful)
    assert(parser.parseAll(parser.atWAtom,"a at 10 [t 5]").successful)
  }
  "atWAtom" should "be rejected" in {
    assert(!parser.parseAll(parser.atWAtom,"a at [t]").successful)
  }
  "optNotIn" should "be accepted" in {
    assert(parser.parseAll(parser.optNotIn," in ").successful)
    assert(parser.parseAll(parser.optNotIn,"                       in ").successful)
    assert(parser.parseAll(parser.optNotIn," in                       ").successful)
    assert(parser.parseAll(parser.optNotIn," not in ").successful)
    assert(parser.parseAll(parser.optNotIn," not         in                       ").successful)
    assert(parser.parseAll(parser.optNotIn," not ").successful)
  }
  "optIn" should "be rejected" in {
    assert(!parser.parseAll(parser.optNotIn,"in ").successful)
    assert(!parser.parseAll(parser.optNotIn," in").successful)
    assert(!parser.parseAll(parser.optNotIn,"in").successful)
    assert(!parser.parseAll(parser.optNotIn,"not in").successful)
  }
  "window" should "be accepted" in {
    assert(parser.parseAll(parser.window," [t]").successful)
    assert(parser.parseAll(parser.window," [t 5]").successful)
    assert(parser.parseAll(parser.window," [t 5 sec]").successful)
    assert(parser.parseAll(parser.window," [t 5 sec,10 min,4]").successful)
    assert(parser.parseAll(parser.window," [t 5, 10 ,4]").successful)
  }
  "window" should "be rejected" in {
    assert(!parser.parseAll(parser.window," []").successful)
  }
  "operand" should "be accepted" in {
    assert(parser.parseAll(parser.operand,"Strings").successful)
    assert(parser.parseAll(parser.operand,"10.0").successful)
  }
  "operand" should "be rejected" in {
    val otherSymbols = comp ++ arith ++ List("\n\r ")
    otherSymbols.foreach { c => assert(!parser.parseAll(parser.operand,"String_with_"+c).successful) }
  }
  "arithmetic" should "be accepted" in {
    arith.foreach { c => assert(parser.parseAll(parser.arithmetic,c).successful) }
  }
//  "arithmetic" should "be rejected" in {}
  "compare" should "be accepted" in {
    comp.foreach { c => assert(parser.parseAll(parser.compare,c).successful) }
  }
//  "compare" should "be rejected" in {}
  "arithOperation" should "be accepted" in {
    arith.foreach { c => assert(parser.parseAll(parser.arithOperation,"1"+c+"1").successful) }
  }
  "arithOperation" should "be rejected" in {
    comp.foreach { c => assert(!parser.parseAll(parser.arithOperation,"1"+c+"1").successful) }
  }
  "leftOperation" should "be accepted" in {
    assert(parser.parseAll(parser.operation,"1+0=1").successful)
  }
  "leftOperation" should "be rejected" in {
    assert(!parser.parseAll(parser.leftOperation,"1=1+0").successful)
  }
  "rightOperation" should "be accepted" in {
    assert(parser.parseAll(parser.operation,"1=1+0").successful)
  }
  "rightOperation" should "be rejected" in {
    assert(!parser.parseAll(parser.rightOperation,"1+0=1").successful)
  }
  "operation" should "be accepted" in {
    assert(parser.parseAll(parser.operation,"1=1+0").successful)
    assert(parser.parseAll(parser.operation,"1+0=1").successful)
    assert(parser.parseAll(parser.operation,"A=B+C").successful)
    assert(parser.parseAll(parser.operation,"A+B=C").successful)
  }
  "operation" should "be rejected" in {
    assert(!parser.parseAll(parser.operation,"0+1=1+0").successful)
  }
  "param" should "be accepted" in {
    assert(parser.parseAll(parser.param,"1").successful)
    assert(parser.parseAll(parser.param,"1.0").successful)
    assert(parser.parseAll(parser.param,"1.0 Einheit").successful)
  }
  "param" should "be rejected" in {
    assert(!parser.parseAll(parser.param,"random words").successful)
  }
  "A variable starting with an upper case character" should "be accepted" in {
    assert(parser.parseAll(parser.variable,"V").successful)
//    println(parser.parseAll(parser.variable,"V4ri4_bl3"))
    assert(parser.parseAll(parser.variable,"V4ri4_bl3").successful)
  }
  "A variable starting with lower case character" should "be rejected" in {
    assert(!parser.parseAll(parser.variable,"v").successful)
    assert(!parser.parseAll(parser.variable,"1").successful)
    assert(!parser.parseAll(parser.variable,"_").successful)
  }
//  "neg" should "be accepted" in {}
//  "neg" should "be rejected" in {}
//  "number" should "be accepted" in {}
//  "number" should "be rejected" in {}
//  "digit" should "be accepted" in {}
//  "digit" should "be rejected" in {}
//  "newline" should "be accepted" in {}
//  "newline" should "be rejected" in {}
  "str" should "be accepted" in {
    assert(parser.parseAll(parser.str,"A_regular_string").successful)
    assert(parser.parseAll(parser.str,"4_str1ng_w1th_numb3rs").successful)
  }
  "str" should "be rejected" in {
    assert(!parser.parseAll(parser.str,"Strings do not contain whitespaces").successful)
  }
/*  "char" should "be accepted" in {
    assert(parser.parseAll(parser.char,"u").successful)
    assert(parser.parseAll(parser.char,"U").successful)
  }
  "char" should "be rejected" in {
    assert(!parser.parseAll(parser.char,"IUIUIU").successful)
    assert(!parser.parseAll(parser.char,"iuiuiu").successful)
  }*/
  "lowChar" should "be accepted" in {
    assert(parser.parseAll(parser.lowChar,"u").successful)
  }
  "lowChar" should "be rejected" in {
    assert(!parser.parseAll(parser.lowChar,"U").successful)
  }
  "upperChar" should "be accepted" in {
    assert(parser.parseAll(parser.upperChar,"U").successful)
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
    assert(parser.parse(parser.lineComment,"// foo and bar are words\n comment ends with line").successful)
  }
  "lineComment" should "accept latex-style comments" in {
    assert(parser.parse(parser.lineComment,"% foo and bar are words\n comment ends with line").successful)
  }
//  "lineComment" should "be rejected" in {}
  "newBlockComment" should "accept anything that is between /* */ or %* *% or any combination of the two" in {
    assert(parser.parseAll(parser.blockComment,"/* foo\n * bar\n * next line\n *%").successful)
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