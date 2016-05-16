package core

/**
  * Created by FM on 16.05.16.
  */
package object asp {
  type AspRule = AspRuleT[Atom]
  type AspProgram = AspProgramT[Atom, AspRule]
}
