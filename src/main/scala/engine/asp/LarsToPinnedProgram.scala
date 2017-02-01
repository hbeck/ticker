package engine.asp

import core._
import core.asp.AspRule
import core.lars._

import scala.concurrent.duration._

/**
  * Created by FM on 05.05.16.
  */

case class LarsToPinnedProgram(engineTick: EngineTick = 1 second) {
  //TODO hb: maybe better called "toAspAtom"
  def apply(extendedAtom: ExtendedAtom): AtomWithArgument = extendedAtom match {
    case AtAtom(t, a) => a(t)
    // TODO: discuss if this approach is correct: can an head-atom be already pinned?
    case VariablePinnedAtAtom(a, v: TimeVariableWithOffset) => a(v)
    case a: Atom => a(T)
    case a: WindowAtom => this.apply(a)
  }

  //TODO hb: maybe better called "asAspRules; change output type to NormalRule"
  def apply(rule: LarsRule): Set[PinnedRule] = {
    val derivedBodyRules = (rule.pos ++ rule.neg) flatMap derivation

    Set(this.transformRule(rule)) ++ derivedBodyRules
  }

  def apply(program: LarsProgram): PinnedProgramWithLars = {
    val rules: Seq[LarsRuleAsPinnedRules] = program.rules map (r => (r, this.apply(r)))
    PinnedProgramWithLars(rules, engineTick)
  }

  def apply(windowAtom: WindowAtom): PinnedAtom = {
    val basicAtom = atomFor(windowAtom)

    basicAtom(T)
  }

  //TODO hb: maybe better called "toAspRule"
  def transformRule(rule: LarsRule): PinnedRule = {
    AspRule(
      this.apply(rule.head),
      (rule.pos map this.apply) + now(T),
      rule.neg map this.apply
    )
  }

  def predicateFor(window: WindowAtom) = {
    val windowFunction = window.windowFunction match {
      case SlidingTimeWindow(size) => f"w_te_${size.ticks(engineTick)}"
      case SlidingTupleWindow(size) => f"w_tu_$size"
      case FluentWindow => f"w_fl"
    }
    val operator = window.temporalModality match {
      case Diamond => "d"
      case Box => "b"
      case a: At => f"at_${a.time}"
    }
    val atomName = window.atom match {
      case p: PinnedAtom => p.atom.predicate
      case _ => window.atom.predicate
    }
    f"${windowFunction}_${operator}_${atomName.toString}"
  }

  def derivation(extendedAtom: ExtendedAtom): Set[PinnedRule] = extendedAtom match {
    case w@WindowAtom(_, temporalOperator, _) => temporalOperator match {
      case a: At => rulesForAt(w)
      case Diamond => rulesForDiamond(w)
      case Box => rulesForBox(w)
    }
    case _ => Set()
  }

  def rulesForBox(windowAtom: WindowAtom): Set[PinnedRule] = {
    val generatedAtoms: Set[AtomWithArgument] = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, windowAtom.atom, T).toSet[AtomWithArgument] ++ Set(windowAtom.atom(T))
      case SlidingTupleWindow(size) => {
        val rAtom = tupleReference(windowAtom.atom) _
        0 until size.toInt map (rAtom(_)) toSet
      }
    }

    val posBody = generatedAtoms ++ Set(now(T))

    Set(AspRule(head(windowAtom), posBody, Set()))
  }

  def rulesForDiamond(windowAtom: WindowAtom): Set[PinnedRule] = {
    val h = head(windowAtom)

    val generatedAtoms = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, windowAtom.atom, T)
      case SlidingTupleWindow(size) => {
        val rAtom = tupleReference(windowAtom.atom) _
        (0 until size.toInt) map (rAtom(_))
      }
      case FluentWindow => Seq(windowAtom.atom.asFluentReference())

    }

    val rules = generatedAtoms map (a => AspRule(h, Set(now(T), a)))

    rules.toSet
  }


  def rulesForAt(windowAtom: WindowAtom): Set[PinnedRule] = {
    val at = windowAtom.temporalModality.asInstanceOf[At]

    at.time match {
      case t: TimePoint => rulesForAtTimePoint(windowAtom, t)
      case v: TimeVariableWithOffset => rulesForAtTimeVariable(windowAtom, v)
    }
  }

  def rulesForAtTimePoint(windowAtom: WindowAtom, timePoint: TimePoint): Set[PinnedRule] = {
    val h = atomFor(windowAtom)

    val atomAtTime = windowAtom.atom(timePoint)

    val nowAtoms = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => generateAtomsOfT(size, now, timePoint, (t, i) => t + i)
    }

    val rules = nowAtoms map (n => AspRule[AtomWithArgument, AtomWithArgument](h(n.time), Set(atomAtTime, n)))

    rules.toSet
  }

  def rulesForAtTimeVariable(windowAtom: WindowAtom, timeVariable: TimeVariableWithOffset): Set[PinnedRule] = {
    val h = Atom(predicateFor(windowAtom))

    val generatedRules = windowAtom.windowFunction match {
      case SlidingTimeWindow(size) => (0 to size.ticks(engineTick).toInt) map { t =>
        AspRule(
          h(T - t)(T).asInstanceOf[AtomWithArgument],
          Set[AtomWithArgument](
            windowAtom.atom(T - t),
            now(T)
          )
        )
      }
    }

    generatedRules.toSet
  }

  private def atomFor(windowAtom: WindowAtom) = {
    val atom = Atom(predicateFor(windowAtom))
    val previousArguments = windowAtom.atom match {
      case aa: AtomWithArgument => aa.arguments
      case a: Atom => Seq()
    }
    val filteredAtomArguments = windowAtom.windowFunction match {
      case FluentWindow => previousArguments take 1
      case _ => previousArguments
    }
    val atomsWithArguments = atom.apply(filteredAtomArguments: _*)

    windowAtom.temporalModality match {
      case At(v: TimeVariableWithOffset) => atomsWithArguments(v)
      case _ => atomsWithArguments
    }
  }

  def tupleReference(atom: Atom)(position: Long): GroundAtomWithArguments = atom.asTupleReference(position)

  def head(atom: WindowAtom): AtomWithArgument = atomFor(atom)(T)

  def generateAtomsOfT(windowSize: TimeWindowSize, atom: Atom, referenceTime: Time, calculateTime: (Time, Int) => Time = (t, i) => t - i): Set[PinnedAtom] = {
    val generateAtoms = (1 to windowSize.ticks(engineTick).toInt) map (calculateTime(referenceTime, _)) map (atom(_))
    (generateAtoms :+ atom(referenceTime)).toSet
  }
}

/*
 * we keep the original lars rule for potential later optimizations
 */
case class PinnedProgramWithLars(larsRulesAsPinnedRules: Seq[LarsRuleAsPinnedRules], tickSize: EngineTick) extends PinnedProgram with LarsBasedProgram {

  override val rules = larsRulesAsPinnedRules flatMap { case (_, pinned) => pinned }

  override val larsRules = larsRulesAsPinnedRules map { case (lars, _) => lars }
}
