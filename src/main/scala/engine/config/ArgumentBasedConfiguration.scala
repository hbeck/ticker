package engine.config

import core.lars.LarsProgram
import engine.EvaluationEngine
import engine.asp.tms.policies.LazyRemovePolicy
import jtms.{JtmsDoyle, JtmsGreedy, JtmsLearn}

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
case class ArgumentBasedConfiguration(program: LarsProgram) {

  def build(evaluationType: String, evaluationModifier: String) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(evaluationType: String,
                  evaluationModifier: String,
                  random: Random = new Random(1)): Option[EvaluationEngine] = {
    // TODO: not nice

    if (evaluationType == "tms") {
      if (evaluationModifier == "greedy") {
        return Some(greedyTms(program, random))
      } else if (evaluationModifier == "doyle") {
        return Some(doyleTms(program, random))
      } else if (evaluationModifier == "learn") {
        return Some(learnTms(program, random))
      }
    } else if (evaluationType == "clingo") {
      if (evaluationModifier == "push") {
        return Some(clingoPush(program))
      } else if (evaluationModifier == "pull") {
        return Some(clingoPull(program))
      }
    }

    None
  }

  def greedyTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = JtmsGreedy(random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def doyleTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = JtmsDoyle(random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def learnTms(program: LarsProgram, random: Random = new Random(1)) = {
    val tms = new JtmsLearn(random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).useAsp().withTms().usingPolicy(LazyRemovePolicy(tms)).start()
  }

  def clingoPush(program: LarsProgram) = {
    BuildEngine.withProgram(program).useAsp().withClingo().use().usePush().start()
  }

  def clingoPull(program: LarsProgram) = {
    BuildEngine.withProgram(program).useAsp().withClingo().use().usePull().start()
  }


}
