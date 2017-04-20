package jtms.evaluation

import core.asp.{NormalRule, UserDefinedAspFact, UserDefinedAspRule}
import core.lars.{LarsRule, TimePoint, UserDefinedLarsRule}
import core.{Atom, Model, PinnedAtom}

/**
  * Created by hb on 04.04.17.
  */
trait StreamingTmsEvalInst extends LarsEvaluationInstance {

  val windowSize: Int

  //<manual TMS>
  def staticRules(): Seq[NormalRule]

  var addedFacts = Map[Int,Seq[NormalRule]]()

  def manualTmsFactsToAddAt(t: Int): Seq[NormalRule] = {
    val rules = generateFactsToAddAt(t)
    addedFacts = addedFacts + (t -> rules)
    rules
  }
  def manualTmsFactsToRemoveAt(t: Int): Seq[NormalRule] = {
    val u = t - windowSize - 1
    addedFacts.get(u) match {
      case Some(seq) => {
        addedFacts = addedFacts - u
        seq
      }
      case None => Seq()
    }
  }

  def manualTmsRulesToAddAt(t: Int) = immediatelyExpiringRulesFor(t) ++ rulesExpiringAfterWindow(t)
  def manualTmsRulesToRemoveAt(t: Int) = immediatelyExpiringRulesFor(t-1) ++ rulesExpiringAfterWindow(t - windowSize - 1)

  def immediatelyExpiringRulesFor(t: Int): Seq[NormalRule]
  def rulesExpiringAfterWindow(t: Int): Seq[NormalRule]

  def generateFactsToAddAt(t: Int): Seq[NormalRule] = generateSignalsToAddAt(t) map (pinnedFact(_,t))
  //</manual TMS>


  def contains(model: Model, t: Int, a: Atom) = {
    if (!model.contains(a)) {
      printModel(t,model)
      println(f"does not contain $a")
      assert(false)
    }
  }
  def notContains(model: Model, t: Int, a: Atom) = {
    if (model.contains(a)) {
      printModel(t,model)
      println(f"contains $a")
      assert(false)
    }
  }
  def containsSomeOf(model: Model, t: Int, ats: Seq[Atom]) = {
    if (!(ats.exists(model.contains(_)))) {
      printModel(t,model)
      println(f"contained none of $ats")
      assert(false)
    }
  }

  def printModel(t:Int, model: Set[Atom]): Unit = {
    println(f"\nt=$t")
    model foreach println
  }



  //
  //helper methods
  //

  def rule(head: Atom, posBody: Set[Atom], negBody: Set[Atom]): NormalRule = {
    UserDefinedAspRule[Atom](head, posBody, negBody)
  }

  def rule(head: Atom, posBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set())
  }

  def rule(head: Atom, posBody: Atom, negBody: Atom): NormalRule = {
    UserDefinedAspRule[Atom](head, Set(posBody), Set(negBody))
  }

  def fact(head: Atom): NormalRule = UserDefinedAspFact[Atom](head)

  def larsFact(head: Atom): LarsRule = UserDefinedLarsRule(head,Set(),Set())

  def pinnedFact(a: Atom, t: Int): NormalRule = fact(PinnedAtom.asPinnedAtAtom(a,TimePoint(t)))

}


