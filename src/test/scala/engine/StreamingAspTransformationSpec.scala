package engine

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
    assert(StreamingAspTransformation.transform(t1, Set()) == Set("now(1000)."))
  }

  "A program with one atom" should "be translated into the facts 'now(2000). a.'" in {
    assert(StreamingAspTransformation.transform(t2, Set(a)) == Set("now(2000).", "a."))
  }

}
