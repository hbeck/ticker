package core.grounding.incremental

import core.Atom
import core.asp.{AspProgram, NormalRule}
import core.grounding.{GrounderInstance, RuleGrounder, StaticProgramInspection}

/**
  * Incremental ASP grounded tailored for use in Incremental Engine based on LARS mapping.
  *
  * Created by hb on 08.03.17.
  */
case class TailoredIncrementalGrounder() {

  var staticGroundRules: Seq[NormalRule] = Seq()

  var allRules: Set[NormalRule] = Set()

  var inspection: StaticProgramInspection[NormalRule, Atom, Atom] = null
  var grounder: RuleGrounder[NormalRule, Atom, Atom] = null

  //TODO 0420 instantiate tick-depending rules with (0,0) in order to pre-ground other values?
  //TODO 0420 temporarily add potential values?
  def prepareStaticGroundRules(staticRules: Seq[NormalRule]): Unit = {
    val inspect = IncrementalProgramInspection.forAsp(AspProgram(staticRules.toList))
    val grounderInstance = GrounderInstance.incrementalAsp(inspect)
    staticGroundRules = staticRules flatMap (grounderInstance.ground(_))
    allRules = allRules ++ staticGroundRules
  }

  //
  //

  var rulesUpdated=true

  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  //var facts: Set[NormalRule] = Set()

//  def add(rules: Seq[NormalRule]) {
//    //rules foreach add //todo incremental
//    allRules = allRules ++ rules
//    rulesUpdated = true
//  }

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

//  def remove(rule: NormalRule) {
//    //TODO incremental call to inspect
//    allRules = allRules - rule
//    rulesUpdated = true
//  }

  def ground(rule: NormalRule): Set[NormalRule] = {
    if (rulesUpdated) {
      inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
      grounder = GrounderInstance.incrementalAsp(inspection)
      rulesUpdated = false
    }
    grounder.ground(rule)
  }

}
