package evaluation.reachBlocked

import core.lars.{Diamond, LarsProgram, LarsRule, W}
import core.{Predicate, Variable}

/**
  * Created by FM on 15.06.16.
  */
trait ReachBlockedProgram {
  val reach = Predicate("reach")
  val blocked = Predicate("blocked")
  val edge = Predicate("edge")
  val obstacle = Predicate("obstacle")

  val X: Variable = "X"
  val Y: Variable = "Y"
  val Z: Variable = "Z"

  val reach_X_Y: LarsRule = reach(X, Y) <= edge(X, Y) not blocked(X, Y)
  val reach_X_Z: LarsRule = reach(X, Z) <= reach(X, Y) and reach(Y, Z)
  val blocked_X_Y_1: LarsRule = blocked(X, Y) <= edge(X, Y) and W(5, Diamond, obstacle(X))
  val blocked_X_Y_2: LarsRule = blocked(X, Y) <= edge(X, Y) and W(5, Diamond, obstacle(Y))

  val baseProgram = LarsProgram.from(
    reach_X_Y,
    reach_X_Z,
    blocked_X_Y_1,
    blocked_X_Y_2
  )

  //TODO see "reach" test case in AspAddRemove.
}
