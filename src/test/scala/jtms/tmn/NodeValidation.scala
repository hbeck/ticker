package jtms.tmn

import jtms.{Justification, Status, Node, TMN}
import org.scalatest.{GivenWhenThen, FlatSpec}

/**
  * Created by FM on 11.02.16.
  */

trait NodeValidation {
  this: FlatSpec =>

  def nodeValidation(tmn: TMN, n: Node): ((NodeValidator) => Any) => Any = {
    val nc = new NodeValidator(tmn, n)

    def nodeCheckTestCallback(nodeCheck: (NodeValidator) => Any) = {
      nodeCheck(nc)
    }

    return nodeCheckTestCallback
  }

  class NodeValidator(tmn: TMN, n: Node) {

    info(n.toString)

    def state(status: Status) = {
      it should "have the state " + status in {
        assert(tmn.status(n) == status)
      }
    }

    def Justifications(justifications: Justification*) = {
      val justificationSet = justifications.toSet
      it should "have the justifications" + justificationSet in {
        assert(tmn.Jn(n) == justificationSet)
      }
    }

    def SJ(j: Option[Justification]) = {

      var text: String = ""
      if (j.isDefined)
        text = "have the supporting justification " + j;
      else
        text = "have no supporting justifications";

      it should text in {
        assert(tmn.SJ(n) == j)
      }
    }

    def Supp(nodes: Node*) = {
      it should "have Supp " + nodes.toSet in {
        assert(tmn.Supp(n) == nodes.toSet)
      }
    }

    def SuppTrans(nodes: Node*) = {
      it should "have Supp*  " + nodes.toSet in {
        assert(tmn.SuppTrans(n) == nodes.toSet)
      }
    }

    def Ant(nodes: Node*) = {
      it should "have Ant " + nodes.toSet in {
        assert(tmn.Ant(n) == nodes.toSet)
      }
    }

    def AntTrans(nodes: Node*) = {
      it should "have Ant* " + nodes.toSet in {
        assert(tmn.AntTrans(n) == nodes.toSet)
      }
    }

    def Cons(nodes: Node*) = {
      it should "have Cons " + nodes.toSet in {
        assert(tmn.Cons(n) == nodes.toSet)
      }
    }

    def ACons(nodes: Node*) = {
      it should "have ACons " + nodes.toSet in {
        assert(tmn.ACons(n) == nodes.toSet)
      }
    }

    def AConsTrans(nodes: Node*) = {
      it should "have ACons* " + nodes.toSet in {
        assert(tmn.AConsTrans(n) == nodes.toSet)
      }
    }
  }

}

