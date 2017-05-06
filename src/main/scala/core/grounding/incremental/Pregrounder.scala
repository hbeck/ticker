package core.grounding.incremental

import core.Atom
import core.asp.NormalRule
import core.grounding.{GrounderInstance, RuleGrounder, StaticProgramInspection}

/**
  * Incremental ASP grounded tailored for use in Incremental Engine based on LARS mapping.
  *
  * Created by hb on 08.03.17.
  */
case class Pregrounder() {

  var allRules: List[NormalRule] = null
  var inspection: StaticProgramInspection[NormalRule, Atom, Atom] = null
  var grounder: RuleGrounder[NormalRule, Atom, Atom] = null

  var rulesUpdated=true

  def init(rules: Seq[NormalRule]): Unit = {
    allRules = rules.toList
    inspection = StaticProgramInspection[NormalRule, Atom, Atom](allRules)
    grounder = GrounderInstance.forAsp(inspection)
    rulesUpdated=true
  }

  def groundPartially(rule: NormalRule): Set[NormalRule] = {
    grounderCall(rule,false)
  }

  def groundFully(rule: NormalRule): Set[NormalRule] = {
    grounderCall(rule,true)
  }

  private def grounderCall(rule: NormalRule, ensureGroundResult: Boolean): Set[NormalRule] = {
    if (rulesUpdated) {
      inspection = StaticProgramInspection[NormalRule, Atom, Atom](allRules)
      grounder = GrounderInstance.forAsp(inspection)
      rulesUpdated = false
    }
    grounder.ground(rule, ensureGroundResult)
  }

}
