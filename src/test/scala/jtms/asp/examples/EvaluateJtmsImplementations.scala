package jtms.asp.examples

import clingo.ClingoEvaluation
import core.Evaluation
import jtms.evaluation.{JtmsExtendedEvaluation, JtmsBeierleEvaluation, JtmsRefactoredEvaluation}
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateJtmsImplementations {
  this: FlatSpec =>

  val asp = ClingoEvaluation()
  val jtmsBeierle = new JtmsBeierleEvaluation
  val jtmnRefactored = new JtmsRefactoredEvaluation
  val jtmnExtended = new JtmsExtendedEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The JtmsBeierle implementation" should behave like tests(jtmsBeierle)
    "The JtmsRefactored implementation" should behave like tests(jtmnRefactored)
    "The JtmsExtended implementation" should behave like tests(jtmnExtended)
  }
}
