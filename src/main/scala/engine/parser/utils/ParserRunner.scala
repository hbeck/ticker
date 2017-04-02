package engine.parser.utils

import engine.parser.AbstractLarsParser

/**
  * Created by et on 16.03.17.
  */
class ParserRunner() extends AbstractLarsParser {

  def parseProgram(input: String): ParseResult[Any] = parseAll(program,input)

  /* These have been implemented to test smaller portions of a lars program. Maybe remove them or comment out. */
  def parseImport(input: String): ParseResult[Any] = parseAll(importN,input)
  def parseRule(input: String): ParseResult[Any] = parseAll(rule,input)
  def parseHead(input: String): ParseResult[Any] = parseAll(head,input)
  def parseBody(input: String): ParseResult[Any] = parseAll(body,input)
  def parseAtom(input: String): ParseResult[Any] = parseAll(atom,input)
  def parseAtAtom(input: String): ParseResult[Any] = parseAll(atAtom,input)
  def parseWAtom(input: String): ParseResult[Any] = parseAll(wAtom,input)
  def parseBodyAtom(input: String): ParseResult[Any] = parseAll(bodyElement,input)
  def parseWindow(input: String): ParseResult[Any] = parseAll(window,input)
//  def parseArithmeticOp(input: String): ParseResult[Any] = parseAll(arithmeticOp,input)
//  def parseLogicOp(input: String): ParseResult[Any] = parseAll(logicOp,input)
  def parseOperand(input: String): ParseResult[Any] = parseAll(operand,input)
  def parseArithmetic(input: String): ParseResult[Any] = parseAll(arithmetic,input)
  def parseBool(input: String): ParseResult[Any] = parseAll(compare,input)
  def parseOperation(input: String): ParseResult[Any] = parseAll(operation,input)
  def parseParam(input: String): ParseResult[Any] = parseAll(param,input)
  def parseNeg(input: String): ParseResult[Any] = parseAll(neg,input)
  def parseComment(input: String): ParseResult[Any] = parseAll(comment,input)
  def parseStr(input: String): ParseResult[Any] = parseAll(str,input)
  def parseChar(input: String): ParseResult[Any] = parseAll(char,input)
}
