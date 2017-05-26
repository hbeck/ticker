package jtms.asp.examples

import clingo.ClingoEvaluation
import core.Evaluation
import iclp.evaluation.{JtmsBeierleFixedEvaluation, JtmsDoyleEvaluation, JtmsGreedyEvaluation}
import org.scalatest.FlatSpec

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateJtmsImplementations {
  this: FlatSpec =>

  val asp = ClingoEvaluation()
  val jtmsBeierleFixed = new JtmsBeierleFixedEvaluation
  val jtmsDoyle = new JtmsDoyleEvaluation
  val jtmsGreedy = new JtmsGreedyEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The JtmsBeierleFixed implementation" should behave like tests(jtmsBeierleFixed)
    "The JtmsDoyle implementation" should behave like tests(jtmsDoyle)
    "The JtmsGreedy implementation" should behave like tests(jtmsGreedy)
  }
}
