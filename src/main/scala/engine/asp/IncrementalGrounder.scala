package engine.asp

import core._
import core.asp.NormalRule
import core.lars.{LarsProgramInspection, RuleGrounder}

/**
  * Created by hb on 08.03.17.
  */
case class IncrementalGrounder(larsProgramEncoding: LarsProgramEncoding) {

  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  var facts: Set[NormalRule] = Set()

  def staticGroundRules() = larsProgramEncoding.groundBaseRules

  def add(rules: Seq[NormalRule]) {
    rules foreach add
  }

  def add(rule: NormalRule) {
    //TODO
  }

  def remove(rules: Seq[NormalRule]) {
    rules foreach remove
  }

  def remove(rule: NormalRule) {
    //TODO
  }

  def ground(rule: NormalRule): Set[NormalRule] = {
    val inspection = LarsProgramInspection.from(Seq(rule) ++ facts) //TODO incremental
    new RuleGrounder[NormalRule,Atom,Atom]().groundWith(inspection)(rule)
  }

}
