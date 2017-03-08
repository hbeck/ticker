package core.grounding.incremental

import core.asp.{AspProgram, NormalRule}
import core.grounding.GrounderInstance

import scala.collection.mutable.ListBuffer

/**
  * Created by hb on 08.03.17.
  */
case class IncrementalAspGrounder(staticGroundRules: Seq[NormalRule]) {

  val allRules: ListBuffer[NormalRule] = ListBuffer() ++ staticGroundRules

  var inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
  val grounder = GrounderInstance.incrementalAsp(inspection)


  //val entireStreamAsFacts: Set[NormalRule] = signalTracker.allTimePoints(networkTime).flatMap(asFacts).toSet

  //var facts: Set[NormalRule] = Set()

//  def add(rules: Seq[NormalRule]) {
//    rules foreach add //todo incremental
//  }

  def add(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules += rule
  }

  def remove(rules: Seq[NormalRule]) {
    rules foreach remove //TODO incremental
  }

  def remove(rule: NormalRule) {
    //TODO incremental call to inspect
    allRules -= rule
  }

  def ground(rule: NormalRule): Set[NormalRule] = {
    inspection = IncrementalProgramInspection.forAsp(AspProgram(allRules.toList)) //TODO incremental
    grounder.ground(rule)
  }

}
