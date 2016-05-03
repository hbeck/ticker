package core.lars

/**
  * Created by FM on 01.05.16.
  */
trait ExtendedAtom

trait HeadAtom


object HeadAtom {
  implicit def headAtomToBuilder(atom: HeadAtom): LarsBuilderHead = new LarsBuilderHead(atom)

  implicit def headAtomToFact(atom: HeadAtom): Rule = Fact(atom)
}