package engine.config

import core.lars.LarsProgram
import engine.EvaluationEngine
import engine.asp.tms.policies.LazyRemovePolicy
import jtms.{OptimizedJtms, JtmsDoyle, JtmsGreedy, JtmsLearn}

import scala.util.Random

/**
  * Created by FM on 29.08.16.
  */
case class ArgumentBasedConfiguration(program: LarsProgram) {

  def build(evaluationType: String, evaluationModifier: String) = buildEngine(evaluationType, evaluationModifier)

  def buildEngine(evaluationType: String,
                  evaluationModifier: String,
                  jtms: OptimizedJtms = new OptimizedJtms(),
                  random: Random = new Random(1)): Option[EvaluationEngine] = {

    if (evaluationType == "tms") {
      if (evaluationModifier == "greedy") {
        return Some(greedyTms(program, jtms, random))
      } else if (evaluationModifier == "doyle") {
        return Some(doyleTms(program, jtms, random))
      } else if (evaluationModifier == "learn") {
        return Some(learnTms(program,jtms, random))
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

  def greedyTms(program: LarsProgram, jtms: OptimizedJtms = new OptimizedJtms(), random: Random = new Random(1)) = {
    val tms = JtmsGreedy(jtms, random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def doyleTms(program: LarsProgram, jtms: OptimizedJtms = new OptimizedJtms(), random: Random = new Random(1)) = {
    val tms = JtmsDoyle(jtms, random)
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def learnTms(program: LarsProgram, jtms: OptimizedJtms = new OptimizedJtms(), random: Random = new Random(1)) = {
    val tms = new JtmsLearn(jtms,random)
    tms.doConsistencyCheck = false
    tms.doJtmsSemanticsCheck = false
    tms.recordStatusSeq = false
    tms.recordChoiceSeq = false

    BuildEngine.withProgram(program).configure().withTms().withPolicy(LazyRemovePolicy(tms)).start()
  }

  def clingoPush(program: LarsProgram) = {
    BuildEngine.withProgram(program).configure().withClingo().use().usePush().start()
  }

  def clingoPull(program: LarsProgram) = {
    BuildEngine.withProgram(program).configure().withClingo().use().usePull().start()
  }


}
