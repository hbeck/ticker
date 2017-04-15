package engine.parser.utils

import engine.parser.SimpleLarsParser
import engine.parser.factory._
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 16.03.17.
  */
class ParserRunner() extends SimpleLarsParser {

  def parseProgram(input: String): ParseResult[Any] = parseAll(program,input)

  /* These have been implemented to test smaller portions of a lars program. Maybe remove them or comment out. */
  def parseImport(input: String): ParseResult[ImportFactory] = parseAll(importN,input)
  def parseRule(input: String): ParseResult[RuleFactory] = parseAll(rule,input)
  def parseHead(input: String): ParseResult[AtomTrait] = parseAll(head,input)
  def parseBody(input: String): ParseResult[List[BodyTrait]] = parseAll(body,input)
  def parseAtom(input: String): ParseResult[AtomFactory] = parseAll(atom,input)
  def parseAtAtom(input: String): ParseResult[AtAtomFactory] = parseAll(atAtom,input)
  def parseWAtom(input: String): ParseResult[WAtomFactory] = parseAll(wAtom,input)
  def parseBodyAtom(input: String): ParseResult[BodyTrait] = parseAll(bodyElement,input)
  def parseWindow(input: String): ParseResult[WindowFactory] = parseAll(window,input)
  def parseOperand(input: String): ParseResult[OperandFactory] = parseAll(operand,input)
  def parseArithmetic(input: String): ParseResult[String] = parseAll(arithmetic,input)
  def parseBool(input: String): ParseResult[String] = parseAll(compare,input)
  def parseOperation(input: String): ParseResult[OperationFactory] = parseAll(operation,input)
  def parseParam(input: String): ParseResult[ParamWrapper] = parseAll(param,input)
  def parseNeg(input: String): ParseResult[Boolean] = parseAll(neg,input)
  def parseComment(input: String): ParseResult[Any] = parseAll(comment,input)
  def parseStr(input: String): ParseResult[String] = parseAll(str,input)
  def parseChar(input: String): ParseResult[Char] = parseAll(char,input)
}
