package evaluation.diss.instances.analytic

import core.{Atom, IntValue, StringValue}
import evaluation.diss.Helpers._
import evaluation.diss.instances.traits.Instance
import evaluation.diss.programs.ContentProgramProvider
import reasoner.Result

/**
  * Created by hb on 18.05.18.
  *
  */
case class ContentTestInstance() extends Instance with ContentProgramProvider {

  val scale = 5
  val windowSize = 20
  val nrOfItems = 2
  val nrOfQualityLevels = 3

  implicit def i2IntValue(i: Int) = IntValue(i)
  implicit def s2StringValue(s: String) = StringValue(s)

  def generateSignalsToAddAt(t: Int): Seq[Atom] = {
    if (t == 1) {
      Seq(cache("i1","n1"))
    } else if (t == 3) {
      Seq(req("i1","n2"))
    } else if (t == 5) {
      Seq(cache("i1","n2"))
    } else if (t == 11) {
      Seq(cache("i2","n3"))
    } else if (t == 13) {
      Seq(req("i2","n4"))
    } else if (t == 15) {
      Seq(qual("n3",3))
    } else if (t == 17) {
      Seq(qual("n5",2))
    } else if (t == 19) {
      Seq(cache("i2","n5"))
    } else if (t == 21) {
      Seq(qual("n3",1))
    }
    else {
      Seq()
    }

  }

  override def verifyOutput(result: Result, t: Int): Unit = {
    if (t >= 1 && t <= 21) {
      mustHave(result.model,avail("i1","n1"),t)
    }
    if (t >= 3 && t < 5) {
      mustHave(result.model,need("i1","n2"),t)
      mustHave(result.model,src("i1","n2","n1"),t)
      mustHave(result.model,getFrom("i1","n2","n1"),t)
    }
    if (t >= 5 && t <= 23) {
      mustHave(result.model,avail("i1","n2"),t)
      mustHave(result.model,need("i1","n2"),t)
      mustNotHave(result.model,src("i1","n2","n1"),t)
      mustNotHave(result.model,getFrom("i1","n2","n1"),t)
    }
    if (t >= 11 && t <= 31) {
      mustHave(result.model,avail("i2","n3"),t)
    }
    if (t >= 13 && t <= 33) {
      mustHave(result.model,need("i2","n4"),t)
    }
    if (t >= 13 && t < 21) {
      mustHave(result.model,src("i2","n4","n3"),t)
      mustHave(result.model,getFrom("i2","n4","n3"),t)
    }
    if (t >= 15 && t < 21) {
      mustHave(result.model,minQ("n3",3),t)
    }
    if (t >= 17 && t < 21) {
      mustHave(result.model,minQ("n5",2),t)
      mustHave(result.model,worseThan("n5","n3"),t)
    }
    if (t >= 19 && t <= 39) {
      mustHave(result.model,avail("i2","n5"),t)
    }
    if (t >= 19 && t <= 33) {
      mustHave(result.model,src("i2","n4","n5"),t)
    }
    if (t >= 19 && t < 21) {
      mustHave(result.model,dism("i2","n4","n5"),t)
    }
    if (t >= 21 && t <= 35) {
      mustHave(result.model,nMinQ("n3",3),t)
      mustHave(result.model,worseThan("n3","n5"),t)
    }
    if (t >= 21 && t <= 31) {
      mustHave(result.model,minQ("n3",1),t)
      mustHave(result.model,dism("i2","n4","n3"),t)
    }
    if (t >= 21 && t <= 33) {
      mustHave(result.model,getFrom("i2","n4","n5"),t)
    }
  }
}
