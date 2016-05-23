package engine.asp


import core.PinnedAtom
import core.asp.{AspFact, AspRule, FixedAspProgram}

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {
  type PinnedAspFact = AspFact[PinnedAtom]
  type PinnedAspRule = AspRule[PinnedAtom]
  type PinnedAspProgram = FixedAspProgram[PinnedAtom, PinnedAspRule]
}
