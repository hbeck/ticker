package core.grounding.incremental

import core.asp.{AspProgram, NormalRule}
import core.grounding.{GrounderInstance, StaticProgramInspection}

/**
  * Created by hb on 08.03.17.
  */
case class IncrementalAspGrounder(staticGroundRules: Seq[NormalRule]) {

  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  var facts: Set[NormalRule] = Set()

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
    val inspection = StaticProgramInspection.forAsp(AspProgram(List(rule) ++ facts)) //TODO incremental
    val grounder = GrounderInstance.oneShotAsp(inspection)
    grounder.ground(rule)
  }

}
