package engine.asp


import core.PinnedAtom
import core.asp.{AspProgramT, AspRuleT, FixedAspProgram}

/**
  * Created by FM on 20.05.16.
  */
package object evaluation {
  type PinnedAspRule = AspRuleT[PinnedAtom]
  type PinnedAspProgram = FixedAspProgram[PinnedAtom, PinnedAspRule]
}
