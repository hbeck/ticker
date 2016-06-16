package core

/**
  * Created by FM on 16.05.16.
  */
package object asp {
  type NormalRule = AspRule[Atom]
  type NormalFact = AspFact[Atom]
  type NormalProgram = AspProgram[Atom, NormalRule]
}
