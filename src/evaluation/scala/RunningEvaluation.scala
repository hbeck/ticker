import core.lars._
import core.{Atom, Ground, Value, Variable}

/**
  * Created by FM on 23.08.16.
  */
object RunningEvaluation {
  def main(args: Array[String]): Unit = {
    val s = new RunningSample {}

    Console.println(s.groundedConstraints)
    Console.println(s.program.rules.length)
  }
}

trait RunningSample {

  def Fluent(atom: Atom) = WindowAtom(FluentWindow, Diamond, atom)

  val todo = Atom("todo")
  val task = Atom("task")
  val schedule = Atom("schedule")
  val done = Atom("done")
  val runs = Atom("runs")
  val machine = Atom("machine")
  val op = Atom("op")
  val assign = Atom("assign")
  val n_assign = Atom("n_assign")
  val timepoint = Atom("timepoint")
  val estim = Atom("estim")

  val T = Variable("T")
  val M = Variable("M")
  val P = Variable("P")

  val on = Value("on")

  val basicRules: Seq[LarsRule] = Seq(
    todo(T) <= task(T) and W(100, Diamond, schedule(T)) not W(100, Diamond, done(T)),
    runs(M) <= machine(M) and Fluent(op(M, on)),
    assign(M, T, P) <= runs(M) and todo(T) and timepoint(P) not n_assign(M, T, P),
    n_assign(M, T, P) <= runs(M) and todo(T) and timepoint(P) not assign(M, T, P)
  )

  val machines = 1 to 2 map (m => Value("m" + m))
  val tasks = 1 to 2 map (t => Value("task" + t))
  val timepoints = 0 to 99

  val estimatesLookup = tasks.zipWithIndex.toMap
  val estimates = estimatesLookup.map(e => estim(e._1, Value(e._2)))


  // fooling around with naive grounding
  // ._o_O //

  val groundedRules = {
    var grounded: Set[LarsRule] = Set()

    machines foreach { m =>
      tasks foreach { t1 =>
        timepoints foreach { p1 =>
          val ground = Ground(Map(M -> m, T -> t1, P -> Value(p1)))

          val groundedBasicRules = basicRules map ground.apply

          grounded = grounded ++ groundedBasicRules.toSet
        }
      }
    }

    grounded
  }


  val groundedConstraints = {
    val f = Atom("Falsum")
    val counter = Variable("counter")

    val T1 = Variable("T1")
    val T2 = Variable("T2")
    val P1 = Variable("P1")
    val P2 = Variable("P2")
    val E = Variable("e")

    val ungroundedRule: LarsRule = f(counter) <= assign(M, T1, P1) and assign(M, T2, P2) and estim(T1, E) not f(counter)
    var groundedRules: Set[LarsRule] = Set()

    var c = 0
    machines foreach { m =>
      tasks foreach { t1 =>
        timepoints foreach { p1 =>
          tasks foreach { t2 =>
            timepoints foreach { p2 =>
              val e = estimatesLookup(t1)
              timepoints foreach { z =>
                if ((t1 != t2) && (p1 <= p2) && (p1 + e == z) && (p2 < z)) {
                  c = c + 1

                  val grounding = Ground(Map(M -> m, T1 -> t1, T2 -> t2, P1 -> Value(p1), P2 -> Value(p2), E -> Value(e), counter -> Value(c)))

                  val groundedRule = grounding.apply(ungroundedRule)

                  groundedRules = groundedRules + groundedRule
                }
              }
            }
          }
        }
      }
    }

    groundedRules
  }

  val program = LarsProgram((groundedRules ++ groundedConstraints).toSeq)

}


