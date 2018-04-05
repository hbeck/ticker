package reasoner.incremental.jtms.asp.examples

import core.Evaluation
import iclp.evaluation.{JtmsDoyleEvaluation, JtmsDoyleHeuristicsEvaluation, JtmsGreedyEvaluation}
import iclp2.evaluation2.{JtmsBeierleFixedEvaluation, JtmsDoyleEvaluation, JtmsDoyleHeuristicsEvaluation, JtmsGreedyEvaluation}
import org.scalatest.FlatSpec
import reasoner.asp.clingo.ClingoCall

/**
  * Created by FM on 25.02.16.
  */
trait EvaluateJtmsImplementations {
  this: FlatSpec =>

  val asp = ClingoCall()
  val jtmsBeierleFixed = new JtmsBeierleFixedEvaluation
  val jtmsDoyleHeuristics = new JtmsDoyleHeuristicsEvaluation
  val jtmsDoyle = new JtmsDoyleEvaluation
  val jtmsGreedy = new JtmsGreedyEvaluation

  def theSame(tests: => Evaluation => Unit) = {
    "The ASP implementation" should behave like tests(asp)
    "The JtmsBeierleFixed implementation" should behave like tests(jtmsBeierleFixed)
    "The JtmsDoyleHeuristics implementation" should behave like tests(jtmsDoyleHeuristics)
    "The JtmsDoyle implementation" should behave like tests(jtmsDoyle)
    "The JtmsGreedy implementation" should behave like tests(jtmsGreedy)
  }
}
