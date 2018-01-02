package reasoner

import core.Atom

/**
  * Created by hb on 02.01.18.
  */
package object asp {
  type PinnedModel = Set[Atom]
  type PinnedStream = Set[PinnedFact]
}
