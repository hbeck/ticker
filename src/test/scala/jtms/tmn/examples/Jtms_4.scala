package jtms.tmn.examples

import jtms.tmn.AtomValidation
import jtms.{in, out}

/**
  * Created by FM on 05.02.16.
  */
class Jtms_4 extends JtmsSpec with AtomValidation {

  val jtms = {
    val jtms = JTMS()
    jtms.set(Set(e, b, d))
    jtms
  }

  "Atom A" must behave like atomValidation(jtms, a) { validator =>
    validator.state(out)
    validator.Rules(j1)
    validator.SJ(None)
    validator.Supp(c)
    validator.Ancestors(c, a)
    validator.Cons(b, c)
    validator.ACons(b, c)
    validator.Repercussions(b, c, d, a, f)
    validator.Antecedents()
    validator.Foundations()
  }

  "Atom B" must behave like atomValidation(jtms, b) { validator =>
    validator.state(in)
    validator.Rules(j2)
    validator.SJ(Some(j2))
    validator.Supp(a)
    validator.Ancestors(a, c)
    validator.Cons(d)
    validator.ACons(d)
    validator.Repercussions(d)
    validator.Antecedents(a)
    validator.Foundations(a)
  }

  "Atom C" must behave like atomValidation(jtms, c) { validator =>
    validator.state(out)
    validator.Rules(j3)
    validator.SJ(None)
    validator.Supp(a)
    validator.Ancestors(a, c)
    validator.Cons(a, d, f)
    validator.ACons(a, f)
    validator.Repercussions(a, f, b, c, d)
    validator.Antecedents()
    validator.Foundations()
  }

  "Atom D" must behave like atomValidation(jtms, d) { validator =>
    validator.state(in)
    validator.Rules(j4a,j4b)
    validator.SJ(Some(j4a))
    validator.Supp(b)
    validator.Ancestors(b, a, c)
    validator.Cons()
    validator.ACons()
    validator.Repercussions()
    validator.Antecedents(b)
    validator.Foundations(b, a)
  }

  "Atom E" must behave like atomValidation(jtms, e) { validator =>
    validator.state(in)
    validator.Rules(j5)
    validator.SJ(Some(j5))
    validator.Supp()
    validator.Ancestors()
    validator.Cons(f)
    validator.ACons()
    validator.Repercussions()
    validator.Antecedents()
    validator.Foundations()
  }

  "Atom F" must behave like atomValidation(jtms, f) { validator =>
    validator.state(out)
    validator.Rules(j6)
    validator.SJ(None)
    validator.Supp(c)
    validator.Ancestors(c, a)
    validator.Cons()
    validator.ACons()
    validator.Repercussions()
    validator.Antecedents()
    validator.Foundations()
  }
}
