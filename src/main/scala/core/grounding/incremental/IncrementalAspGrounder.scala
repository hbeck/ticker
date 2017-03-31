package core.grounding.incremental

import core.asp.{AspProgram, NormalRule}
import core.grounding.GrounderInstance

/**
  * Created by hb on 08.03.17.
  */
case class IncrementalAspGrounder() {

  var allRules: Set[NormalRule] = Set()

  var inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
  var grounder = GrounderInstance.incrementalAsp(inspection) //TODO val when incremental

  var rulesUpdated=true


  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  //var facts: Set[NormalRule] = Set()

  def add(rules: Seq[NormalRule]) {
    //rules foreach add //todo incremental
    allRules = allRules ++ rules
    rulesUpdated = true
  }

  def add(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules = allRules + rule
    rulesUpdated = true
  }

  def remove(rules: Seq[NormalRule]) {
    //rules foreach remove //TODO incremental
    allRules = allRules -- rules
    rulesUpdated = true
  }

  def remove(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules = allRules - rule
    rulesUpdated = true
  }

  def ground(rule: NormalRule): Set[NormalRule] = {
    if (rulesUpdated) {
      inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
      grounder = GrounderInstance.incrementalAsp(inspection)
      rulesUpdated = false
    }
    grounder.ground(rule)
  }

}
