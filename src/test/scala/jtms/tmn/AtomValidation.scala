package jtms.tmn

import core.Atom
import core.asp.NormalRule
import jtms.{JtmsDoyle, Status}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */

trait AtomValidation {
  this: FlatSpec =>

  //TODO
  def atomValidation(tmn: JtmsDoyle, n: Atom): ((AtomValidator) => Any) => Any = {
    val nc = new AtomValidator(tmn, n)

    def atomCheckTestCallback(check: (AtomValidator) => Any) = {
      check(nc)
    }

    return atomCheckTestCallback
  }

  class AtomValidator(tmn: JtmsDoyle, atom: Atom) {

    info(atom.toString)

    def state(status: Status) = {
      it should "have the state " + status in {
        assert(tmn.status(atom) == status)
      }
    }

    def Rules(rules: NormalRule*) = {
      val ruleSet = rules.toList
      it should "have the rules " + ruleSet in {
        assert(tmn.justifications(atom).toSet == ruleSet.toSet)
      }
    }

    def SJ(j: Option[NormalRule]) = {

      var text: String = ""
      if (j.isDefined)
        text = "have the supporting rule " + j;
      else
        text = "have no supporting rules";

      it should text in {
        assert(tmn.suppRule(atom) == j)
      }
    }

    def Supp(atoms: Atom*) = {
      it should "have Supp " + atoms.toSet in {
        assert(tmn.supp(atom) == atoms.toSet)
      }
    }

    def Ancestors(atoms: Atom*) = {
      it should "have Ancestors  " + atoms.toSet in {
        assert(tmn.ancestors(atom) == atoms.toSet)
      }
    }

    def Antecedents(atoms: Atom*) = {
      it should "have Antecedents " + atoms.toSet in {
        assert(tmn.antecedents(atom) == atoms.toSet)
      }
    }

    def Foundations(atoms: Atom*) = {
      it should "have Foundations* " + atoms.toSet in {
        assert(tmn.foundations(atom) == atoms.toSet)
      }
    }

    def Cons(atoms: Atom*) = {
      it should "have Cons " + atoms.toSet in {
        assert(tmn.cons(atom) == atoms.toSet)
      }
    }

    def ACons(atoms: Atom*) = {
      it should "have affected " + atoms.toSet in {
        assert(tmn.affected(atom) == atoms.toSet)
      }
    }

    def Repercussions(atoms: Atom*) = {
      it should "have repercussions* " + atoms.toSet in {
        assert(tmn.repercussions(atom) == atoms.toSet)
      }
    }
  }

}