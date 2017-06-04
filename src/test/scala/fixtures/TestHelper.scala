package fixtures

import core.{Argument, Atom, AtomModification, AtomWithArguments}

/**
  * Created by fm on 04/06/2017.
  */
object TestHelper {
  implicit def testHelperBuilder(atom: Atom): TestHelper = new TestHelper(atom)

}

case class TestHelper(atom: Atom) {

  // This was moved from core into testhelper to be able to keep tests
  def apply(arguments: Any*): AtomWithArguments = {
    val args: Seq[Argument] = arguments map (a => Argument.convertToArgument(a.toString))
    AtomModification(atom).appendArguments(args)
  }
}
