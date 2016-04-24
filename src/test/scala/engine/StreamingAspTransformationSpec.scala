package engine

import asp.AspExpression
import core.Atom
import engine.implementations.StreamingAspTransformation
import org.scalatest.FlatSpec

/**
  * Created by FM on 22.04.16.
  */
class StreamingAspTransformationSpec extends FlatSpec {
  val t1 = At.second(1)
  val t2 = At.second(2)
  val a = Atom("a")

  "A program with no atoms and only a time" should "be translated into the fact 'now(1000)'" in {
    assert(StreamingAspTransformation.transform(t1, Set()) == Set(AspExpression("now(1000).")))
  }

  "A program with one atom" should "be translated into the facts 'now(2000). a.'" in {
    assert(StreamingAspTransformation.transform(t2, Set(a)) == Set(AspExpression("now(2000)."), AspExpression("a.")))
  }

  "A program with two atoms with arity" should "be translated into the facts 'now(3000). a. b(1)." in {
    val b = Atom("b").apply("1")

    assert(StreamingAspTransformation.transform(At.second(3), Set(a, b)) == Set(AspExpression("now(3000)."), AspExpression("a."), AspExpression("b(1).")))
  }


}
