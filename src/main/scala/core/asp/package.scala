package core

/**
  * Created by FM on 16.05.16.
  */
package object asp {
  type PlainAspRule = AspRuleT[Atom]
  type PlainAspProgram = AspProgramT[Atom, PlainAspRule]
}
