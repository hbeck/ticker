package core.lars

import core._
import org.scalatest.FunSuite

/**
  * Created by hb on 8/25/16.
  */
class RelationAtomTests extends FunSuite {

  implicit def toIntValue(i:Int) = IntValue(i)
  implicit def toStringValue(s:String) = StringValue(s)

  test("relation atom test 1") {

    assert(Neq("a","b").holds())
    assert(!Neq("b","b").holds())

    assert(Neq(1,2).holds())
    assert(!Neq(1,1).holds())

    assert(Leq(1,2).holds())
    assert(Leq(1,1).holds())
    assert(!Leq(2,1).holds())

    assert(Lt(1,2).holds())
    assert(!Lt(1,1).holds())
    assert(!Lt(2,1).holds())

    assert(!Geq(1,2).holds())
    assert(Geq(1,1).holds())
    assert(Geq(2,1).holds())

    assert(!Gt(1,2).holds())
    assert(!Gt(1,1).holds())
    assert(Gt(2,1).holds())

    assert(!Sum(1,1,1).holds())
    assert(Sum(1,1,2).holds())
    assert(!Sum(1,2,2).holds())

    assert(Product(1,1,1).holds())
    assert(!Product(1,2,1).holds())
    assert(Product(1,2,2).holds())

  }

}
