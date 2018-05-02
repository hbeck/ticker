package evaluation.diss.instances.traits

import reasoner.Result

trait InstanceSansVerification extends Instance {

  override def verifyOutput(result: Result, t: Int): Unit = {}

}
