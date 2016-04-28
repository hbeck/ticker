package jtms.asp

import core.{Atom, Rule}
import jtms.{ExtendedJTMS, Status}
import org.scalatest.FlatSpec

/**
  * Created by FM on 11.02.16.
  */

trait AtomValidationASP {
  this: FlatSpec =>

  def atomValidation(net: ExtendedJTMS, n: Atom): ((AtomValidatorASP) => Any) => Any = {
    val nc = new AtomValidatorASP(net, n)

    def atomCheckTestCallback(check: (AtomValidatorASP) => Any) = {
      check(nc)
    }

    return atomCheckTestCallback
  }

  class AtomValidatorASP(net: ExtendedJTMS, atom: Atom) {

    info(atom.toString)

    def status(status: Status) = {
      it should "have the state " + status in {
        assert(net.status(atom) == status)
      }
    }

    def rules(rules: Rule*) = {
      val ruleSet = rules.toList
      it should "have the rules " + ruleSet in {
        assert(net.justifications(atom).toSet == ruleSet.toSet)
      }
    }

    def suppRule(rule: Option[Rule]) = {

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
        assert(net.aff(atom) == atoms.toSet)
      }
    }

    def repercussions(atoms: Atom*) = {
      it should "have repercussions* " + atoms.toSet in {
        assert(net.repercussions(atom) == atoms.toSet)
      }
    }
  }

}