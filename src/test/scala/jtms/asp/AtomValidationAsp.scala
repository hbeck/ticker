package jtms.asp

import core.Atom
import core.asp.NormalRule
import jtms.{JtmsGreedy, Status}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */

trait AtomValidationAsp {
  this: FlatSpec =>

  def atomValidation(net: JtmsGreedy, n: Atom): ((AtomValidatorAsp) => Any) => Any = {
    val nc = new AtomValidatorAsp(net, n)

    def atomCheckTestCallback(check: (AtomValidatorAsp) => Any) = {
      check(nc)
    }

    return atomCheckTestCallback
  }

  class AtomValidatorAsp(net: JtmsGreedy, atom: Atom) {

    info(atom.toString)

    def status(status: Status) = {
      it should "have the state " + status in {
        assert(net.status(atom) == status)
      }
    }

    def rules(rules: NormalRule*) = {
      val ruleSet = rules.toList
      it should "have the rules " + ruleSet in {
        assert(net.justifications(atom).toSet == ruleSet.toSet)
      }
    }

    def suppRule(rule: Option[NormalRule]) = {

      var text: String = ""
      if (rule.isDefined)
        text = "have the supporting rule " + rule;
      else
        text = "have no supporting rules";

      it should text in {
        assert(net.suppRule(atom) == rule)
      }
    }

    def supp(atoms: Atom*) = {
      it should "have Supp " + atoms.toSet in {
        assert(net.supp(atom) == atoms.toSet)
      }
    }

    def ancestors(atoms: Atom*) = {
      it should "have Ancestors  " + atoms.toSet in {
        assert(net.ancestors(atom) == atoms.toSet)
      }
    }

    def antecedents(atoms: Atom*) = {
      it should "have Antecedents " + atoms.toSet in {
        assert(net.antecedents(atom) == atoms.toSet)
      }
    }

    def foundations(atoms: Atom*) = {
      it should "have Foundations* " + atoms.toSet in {
        assert(net.foundations(atom) == atoms.toSet)
      }
    }

    def cons(atoms: Atom*) = {
      it should "have Cons " + atoms.toSet in {
        assert(net.cons(atom) == atoms.toSet)
      }
    }

    def aff(atoms: Atom*) = {
      it should "have ACons " + atoms.toSet in {
        assert(net.affected(atom) == atoms.toSet)
      }
    }

    def repercussions(atoms: Atom*) = {
      it should "have repercussions* " + atoms.toSet in {
        assert(net.repercussions(atom) == atoms.toSet)
      }
    }
  }

}