package jtms.tmn.examples

import jtms.tmn.AtomValidation
import jtms.{in, out}

/**
  * Created by FM on 05.02.16.
  */
class JTMS_4 extends JTMSSpec with AtomValidation {

  val tmn = {
    val tmn = JTMS
    tmn.set(Set(e, b, d))
    tmn
  }

  "Atom A" must behave like   atomValidation(tmn, a) { validator =>
    validator.state(out)
    validator.Rules(j1)
    validator.SJ(None)
    validator.Supp(c)
    validator.SuppTrans(c, a)
    validator.Cons(b, c)
    validator.ACons(b, c)
    validator.AConsTrans(b, c, d, a, f)
    validator.Ant()
    validator.AntTrans()
  }

  "Atom B" must behave like atomValidation(tmn, b) { validator =>
    validator.state(in)
    validator.Rules(j2)
    validator.SJ(Some(j2))
    validator.Supp(a)
    validator.SuppTrans(a, c)
    validator.Cons(d)
    validator.ACons(d)
    validator.AConsTrans(d)
    validator.Ant(a)
    validator.AntTrans(a)
  }

  "Atom C" must behave like atomValidation(tmn, c) { validator =>
    validator.state(out)
    validator.Rules(j3)
    validator.SJ(None)
    validator.Supp(a)
    validator.SuppTrans(a, c)
    validator.Cons(a, d, f)
    validator.ACons(a, f)
    validator.AConsTrans(a, f, b, c, d)
    validator.Ant()
    validator.AntTrans()
  }

  "Atom D" must behave like atomValidation(tmn, d) { validator =>
    validator.state(in)
    validator.Rules(j4a,j4b)
    validator.SJ(Some(j4a))
    validator.Supp(b)
    validator.SuppTrans(b, a, c)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.Ant(b)
    validator.AntTrans(b, a)
  }

  "Atom E" must behave like atomValidation(tmn, e) { validator =>
    validator.state(in)
    validator.Rules(j5)
    validator.SJ(Some(j5))
    validator.Supp()
    validator.SuppTrans()
    validator.Cons(f)
    validator.ACons()
    validator.AConsTrans()
    validator.Ant()
    validator.AntTrans()
  }

  "Atom F" must behave like atomValidation(tmn, f) { validator =>
    validator.state(out)
    validator.Rules(j6)
    validator.SJ(None)
    validator.Supp(c)
    validator.SuppTrans(c, a)
    validator.Cons()
    validator.ACons()
    validator.AConsTrans()
    validator.Ant()
    validator.AntTrans()
  }
}
