package evaluation.reachBlocked

import core.{Atom, Variable}
import core.lars.{Diamond, LarsProgram, W}

/**
  * Created by FM on 15.06.16.
  */
trait ReachBlockedProgram {
  val reach = Atom("reach")
  val blocked = Atom("blocked")
  val edge = Atom("edge")
  val obstacle = Atom("obstacle")

  val X: Variable = "X"
  val Y: Variable = "Y"
  val Z: Variable = "Z"

  val reach_X_Y = reach(X, Y) <= edge(X, Y) not blocked(X, Y)
  val reach_X_Z = reach(X, Y) <= reach(X, Y) and reach(Y, Z)
  val blocked_X_Y_1 = blocked(X, Y) <= edge(X, Y) and W(5, Diamond, obstacle(X))
  val blocked_X_Y_2 = blocked(X, Y) <= edge(X, Y) and W(5, Diamond, obstacle(Y))

  val baseProgram = LarsProgram.from(
    reach_X_Y,
    reach_X_Z,
    blocked_X_Y_1,
    blocked_X_Y_2
  )
}
