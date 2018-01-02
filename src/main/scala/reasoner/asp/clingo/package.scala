package reasoner.asp

/**
  * Created by FM on 27.04.16.
  */
package object clingo {
  type ClingoExpression = String
  type ClingoAtom = String
  type ClingoModel = Set[ClingoAtom]

}
