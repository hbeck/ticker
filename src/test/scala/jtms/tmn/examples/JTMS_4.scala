package jtms.tmn.examples

import jtms.tmn.AtomValidation
import jtms.{in, out}

/**
  * Created by FM on 05.02.16.
  */
class JTMS_4 extends JTMSSpec with AtomValidation {

  val tmn = {
    val tmn = JTMS
    tmn.set(Set(E, B, D))
    tmn
  }

  "Atom A" must behave like   atomValidation(tmn, A) { validator =>
    validator.state(out)
    validator.Rules(j1)
    validator.SJ(None)
    validator.Supp(C)
    validator.SuppTrans(C, A)
    validator.Cons(B, C)
    validator.ACons(B, C)
    validator.AConsTrans(B, C, D, A, F)
    validator.antecedents()
    validator.foundations()
  }

  "Atom B" must behave like atomValidation(tmn, B) { validator =>
    validator.state(in)
    validator.Rules(j2)
    validator.SJ(Some(j2))
    validator.Supp(A)
    validator.SuppTrans(A, C)
    validator.Cons(D)
    validator.ACons(D)
    validator.AConsTrans(D)
    validator.antecedents(A)
    validator.foundations(A)
  }

  "Atom C" must behave like atomValidation(tmn, C) { validator =>
    validator.state(out)
    validator.Rules(j3)
    validator.SJ(None)
    validator.Supp(A)
    validator.SuppTrans(A, C)
    validator.Cons(A, D, F)
    validator.ACons(A, F)
    validator.AConsTrans(A, F, B, C, D)
    validator.antecedents()
    validator.foundations()
  }

  "Atom D" must behave like atomValidation(tmn, D) { validator =>
    validator.state(in)
    validator.Rules(j4a,j4b)
    validator.SJ(Some(j4a))
    validator.Supp(B)
    validator.SuppTrans(B, A, C)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.antecedents(B)
    validator.foundations(B, A)
  }

  "Atom E" must behave like atomValidation(tmn, E) { validator =>
    validator.state(in)
    validator.Rules(j5)
    validator.SJ(Some(j5))
    validator.Supp()
    validator.SuppTrans()
    validator.Cons(F)
    validator.ACons()
    validator.AConsTrans()
    validator.antecedents()
    validator.foundations()
  }

  "Atom F" must behave like atomValidation(tmn, F) { validator =>
    validator.state(out)
    validator.Rules(j6)
    validator.SJ(None)
    validator.Supp(C)
    validator.SuppTrans(C, A)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.antecedents()
    validator.foundations()
  }
}
