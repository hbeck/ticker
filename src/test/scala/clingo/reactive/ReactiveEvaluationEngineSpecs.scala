package clingo.reactive

import core.PinnedAtom
import core.lars._
import engine.asp.{PlainLarsToAspMapper, PlainLarsToReactiveMapper}
import engine.asp.reactive.ReactiveEvaluationEngine
import fixtures.AtomTestFixture
import org.scalatest.FlatSpec

/**
  * Created by fm on 11/02/2017.
  */
class ReactiveEvaluationEngineSpecs extends FlatSpec with AtomTestFixture{

  "a :- w^2 D b and {2 -> b}" should "contain a in the model at t = {2,3,4} " in {
    val p = LarsProgram.from(
      a <= WindowAtom(SlidingTimeWindow(2), Diamond, b)
    )
    val engine: ReactiveEvaluationEngine = buildEngine(p)


    try {

      engine.append(TimePoint(2))(b)

      /*
        a <- \window^2 \Diamond b

        Stream: 1 -> b
        t=3

        1 -> b        <-- answer stream
        3 -> a

        b_at(1)       <-- encoding of answer stream
        a_at(3) a

        a             <-- output stream (model)
     */

      def assertModel(timePoint: TimePoint)={
        val model = engine.evaluate(timePoint)
        assert(model.get.get contains (a))
      }


      assertModel(TimePoint(2))
      assertModel(TimePoint(3))
      assertModel(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }


  "With reactive engine" should "have running a :- w#^2 D b" in {
    val p = LarsProgram.from(
      a <= WindowAtom(SlidingTupleWindow(2), Diamond, b)
    )
    val engine: ReactiveEvaluationEngine = buildEngine(p)

    try {
      engine.append(TimePoint(1))(b)

      val model = engine.evaluate(TimePoint(1)).get.get

      print(model)
      assert(model contains a)
      assert(model contains b)

      engine.evaluate(TimePoint(3))
      engine.evaluate(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }

  "With reactive engine" should "have running a :- w^2 B b" in {
    val p = LarsProgram.from(
      a <= WindowAtom(SlidingTimeWindow(2), Box, b)
    )

    val engine: ReactiveEvaluationEngine = buildEngine(p)

    try {

      engine.append(TimePoint(1))(b)
      engine.append(TimePoint(2))(b)
      engine.append(TimePoint(3))(b)

      val model = engine.evaluate(TimePoint(3))


      assert(model.get.get contains a)

      engine.evaluate(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }

  "With reactive engine" should "have running a :- w#^2 B b" in {
    val p = LarsProgram.from(
      a <= WindowAtom(SlidingTupleWindow(2), Box, b)
    )
    val engine: ReactiveEvaluationEngine = buildEngine(p)


    try {

      engine.append(TimePoint(1))(b)
      engine.append(TimePoint(2))(b)
      engine.append(TimePoint(3))(b)

      val model = engine.evaluate(TimePoint(3))


      assert(model.get.get contains a)

      engine.evaluate(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }


  "With reactive engine" should "have running a_{T+1} :- w#^2 @_T b" in {

    val U = TimeVariableWithOffset("U")

    val p = LarsProgram.from(
      AtAtom(U + 1, a) <= WindowAtom(SlidingTupleWindow(2), At(U), b)
    )

    val engine: ReactiveEvaluationEngine = buildEngine(p)

    try {

      engine.append(TimePoint(1))(b)

      val model = engine.evaluate(TimePoint(2))

      assert(model.get.get contains a)


      engine.evaluate(TimePoint(3))
      engine.evaluate(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }

  "With reactive engine" should "have running a_{T+1} :- w^2 @_T b" in {

    val U = TimeVariableWithOffset("U")

    val p = LarsProgram.from(
      AtAtom(U + 1, a) <= WindowAtom(SlidingTimeWindow(2), At(U), b)
    )

    val engine: ReactiveEvaluationEngine = buildEngine(p)

    try {

      engine.append(TimePoint(1))(b)

      val model = engine.evaluate(TimePoint(2))

      assert(model.get.get contains a)

      engine.evaluate(TimePoint(3))
      engine.evaluate(TimePoint(4))
    }
    finally {
      engine.close()
    }

  }

  private def buildEngine(p: LarsProgram) = {
    val mapper = PlainLarsToReactiveMapper()

    val mappedProgram = mapper(p)

    val engine = ReactiveEvaluationEngine(mappedProgram)
    engine
  }
}
