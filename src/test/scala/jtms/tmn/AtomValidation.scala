package jtms.tmn

import core.{Rule, Atom}
import jtms.{Status, TMN}
import org.scalatest.{GivenWhenThen, FlatSpec}

/**
  * Created by FM on 11.02.16.
  */

trait AtomValidation {
  this: FlatSpec =>

  def atomValidation(tmn: TMN, n: Atom): ((AtomValidator) => Any) => Any = {
    val nc = new AtomValidator(tmn, n)

    def atomCheckTestCallback(check: (AtomValidator) => Any) = {
      check(nc)
    }

    return atomCheckTestCallback
  }

  class AtomValidator(tmn: TMN, atom: Atom) {

    info(atom.toString)

    def state(status: Status) = {
      it should "have the state " + status in {
        assert(tmn.status(atom) == status)
      }
    }

    def Rules(rules: Rule*) = {
      val ruleSet = rules.toSet
      it should "have the rules" + ruleSet in {
        assert(tmn.Jn(atom) == ruleSet)
      }
    }

    def SJ(j: Option[Rule]) = {

      var text: String = ""
      if (j.isDefined)
        text = "have the supporting rule " + j;
      else
        text = "have no supporting rules";

      it should text in {
        assert(tmn.SJ(atom) == j)
      }
    }

    def Supp(atoms: Atom*) = {
      it should "have Supp " + atoms.toSet in {
        assert(tmn.Supp(atom) == atoms.toSet)
      }
    }

    def SuppTrans(atoms: Atom*) = {
      it should "have Supp*  " + atoms.toSet in {
        assert(tmn.SuppTrans(atom) == atoms.toSet)
      }
    }

    def Ant(atoms: Atom*) = {
      it should "have Ant " + atoms.toSet in {
        assert(tmn.Ant(atom) == atoms.toSet)
      }
    }

    def AntTrans(atoms: Atom*) = {
      it should "have Ant* " + atoms.toSet in {
        assert(tmn.AntTrans(atom) == atoms.toSet)
      }
    }

    def Cons(atoms: Atom*) = {
      it should "have Cons " + atoms.toSet in {
        assert(tmn.Cons(atom) == atoms.toSet)
      }
    }

    def ACons(atoms: Atom*) = {
      it should "have ACons " + atoms.toSet in {
        assert(tmn.ACons(atom) == atoms.toSet)
      }
    }

    def AConsTrans(atoms: Atom*) = {
      it should "have ACons* " + atoms.toSet in {
        assert(tmn.AConsTrans(atom) == atoms.toSet)
      }
    }
  }

}