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


  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  //var facts: Set[NormalRule] = Set()

  def add(rules: Seq[NormalRule]) {
    //rules foreach add //todo incremental
    allRules = allRules ++ rules
  }

  def add(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules = allRules + rule
  }

  def remove(rules: Seq[NormalRule]) {
    //rules foreach remove //TODO incremental
    allRules = allRules -- rules
  }

  def remove(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules = allRules - rule
  }

  def ground(rule: NormalRule): Set[NormalRule] = {
    inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
    grounder = GrounderInstance.incrementalAsp(inspection)
    grounder.ground(rule)
  }

}
