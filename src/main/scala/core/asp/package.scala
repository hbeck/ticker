package core

/**
  * Created by FM on 16.05.16.
  */
package object asp {
  type PlainAspRule = AspRule[Atom]
  type PlainAspProgram = AspProgram[Atom, PlainAspRule]
}
