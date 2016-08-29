package evaluation.bit

import core.lars.Util._
import core.lars._

/**
  * Created by hb on 8/29/16.
  */
trait BitProgram {

  val nonGroundRules = Seq[LarsRule](
    rule("bit(L,1) :- level(L), not bit(L,0)"),
    rule("bit(L,0) :- level(L), not bit(L,1)"),
    rule("sum_at(0,B) :- bit(0,B)"),
    rule("sum_at(L,C) :- sum_at(L0,C0), sum(L0,1,L), bit(L,1), pow(2,L,X), sum(C0,X,C), int(X), int(C)"),
    rule("sum_at(L,C) :- sum_at(L0,C), sum(L0,1,L), bit(L,0), int(C)"),
    rule("id(C) :- max_level(M), sum_at(M,C)"),
    rule("xx1 :- id(C), mod(C,10,K), geq(K,8), int(K), not xx1"),
    rule("bit(L,1) :- level(L), w_d_20_signal(L)") //non-asp rule //TODO parsing order of args w_20_d
  )

  val highestExponent = 2 //2^X; prepared program has 2^7
  val maxLevel = highestExponent - 1

  val levels = Seq(fact(f"max_level($maxLevel)")) ++ ((0 to maxLevel) map (i => fact(f"level($i)")))
  val ints = (0 to Math.pow(2, highestExponent).toInt) map (i => fact(f"int($i)"))

  val facts = levels ++ ints

  val inputProgram = LarsProgram(nonGroundRules ++ facts)

  def groundLarsProgram() = {
    val grounder = Grounder(inputProgram)
    LarsProgram(grounder.groundRules)
  }
}
