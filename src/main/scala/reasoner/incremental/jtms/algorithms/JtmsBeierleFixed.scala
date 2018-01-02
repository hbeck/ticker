package reasoner.incremental.jtms.algorithms

import core.Atom
import core.asp.{NormalProgram, NormalRule}
import reasoner.incremental.jtms.networks.TruthMaintenanceNetwork
import reasoner.incremental.jtms.unknown

import scala.util.Random

/**
  * Created by hb on 6/26/16.
  */
object JtmsBeierleFixed {

  def apply() = new JtmsBeierleFixed()

  def apply(P: NormalProgram): JtmsBeierleFixed = {
    val jtms = new JtmsBeierleFixed()
    P.rules foreach jtms.add
    jtms
  }

}

/**
  * justification-based truth maintenance network
  *
  * follows quite closely the presentation in the
  * book chapter from Beierle and Kern-Isberner,
  * but fixes some bugs.
  *
  */
class JtmsBeierleFixed(network: TruthMaintenanceNetwork = TruthMaintenanceNetwork(), random: Random = new Random()) extends JtmsBeierle(network, random) {

  override def update(L: Predef.Set[Atom]) {
    //try {
    updateImpl(L)
    //    } catch {
    //      case e:IncrementalUpdateFailureException => { //odd loop detection
    //        invalidateModel()
    //      }
    //    }
  }

  override def step2(rule: NormalRule) = false

  override def step3(atom: Atom): Unit = {
    setUnknown(atom)
  }

  //fix (choose) status
  override def step5a(atom: Atom): Unit = {
    if (network.status(atom) != unknown)
      return

    print(atom)

    network.justifications(atom) find unfoundedValid match {
      case Some(rule) => {
        if (!ACons(atom).isEmpty) {
          /*
          for (n <- ACons(atom) + atom) {
            status(n) = unknown //vs setUnknown [!]
            step5a(n) //vs first setting all unknown, and only then call 5a if still necessary [!] (see * below)
          }
          */
          val revisit = shuffleSeq(Seq[Atom]() ++ ACons(atom) :+ atom) //avoid infinite loop
          for (n <- revisit) {
            setUnknown(n)
          }
          for (n <- revisit) {
            step5a(n)
          }
        } else {
          setIn(rule)
          //diff to beierle: unknown atoms are left unknown. their support must be determined later.
          /*
          for (n <- rule.neg) {
            if (status(n) == unknown) {
              status(n) = out
            }
          }
          */
          //if (rule.neg exists (status(_) == in)) { //odd loop detection
          //  throw new IncrementalUpdateFailureException()
          //}
          for (u <- network.unknownCons(atom)) {
            //* here other variant is chosen. deliberately? [1]
            step5a(u)
          }
        }
      }
      case None => {
        //all justifications(atom) are unfounded invalid
        setOut(atom) //diff to beierle: findSpoiler allows unknown atoms!
        for (u <- network.unknownCons(atom)) {
          step5a(u)
        }
      }
    }
  }

  //in contrast to beierle/doyle, explicitly allow unknown atoms
  override def findSpoiler(rule: NormalRule): Option[Atom] = {
    val regular: Option[Atom] = super.findSpoiler(rule)
    if (regular.isDefined) return regular

    if (random.nextDouble() < 0.5) {
      rule.pos find (network.status(_) == unknown) match {
        case None => rule.neg find (network.status(_) == unknown)
        case opt => opt
      }
    } else {
      rule.neg find (network.status(_) == unknown) match {
        case None => rule.pos find (network.status(_) == unknown)
        case opt => opt
      }
    }
  }

}
