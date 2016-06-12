package engine.asp.evaluation

import core._
import core.lars.TimePoint
import engine.asp.{MappedProgram, PinnedAspToIncrementalAsp}
import jtms.JtmsExtended

trait TmsPolicy {
  def initialize(groundRules: Seq[GroundedNormalRule])

  def add(rules: Seq[GroundedNormalRule])

  def getModel(): Option[Model]

  def remove(rules: Seq[GroundedNormalRule])
}

case class DirectAddRemovePolicy(extendedJtms: JtmsExtended = JtmsExtended()) extends TmsPolicy {

  override def initialize(groundRules: Seq[GroundedNormalRule]) = groundRules foreach extendedJtms.add

  override def add(rules: Seq[GroundedNormalRule]): Unit = rules foreach extendedJtms.add

  override def remove(rules: Seq[GroundedNormalRule]): Unit = rules foreach extendedJtms.remove

  override def getModel(): Option[Model] = extendedJtms.getModel()
}

/**
  * Created by FM on 18.05.16.
  */
case class TmsEvaluation(pinnedAspProgram: MappedProgram, tmsPolicy: TmsPolicy) extends StreamingAspInterpreter {
  val incrementalProgram = PinnedAspToIncrementalAsp(pinnedAspProgram)

  val (groundRules, nonGroundRules) = incrementalProgram.rules partition (_.isGround)

  tmsPolicy.initialize(groundRules.map(x => GroundedNormalRule(x)))

  def apply(timePoint: TimePoint, pinnedStream: PinnedStream): Option[PinnedModel] = {
    val pin = Pin(timePoint)

    val groundedRules = pin.ground(nonGroundRules)
    val groundedStream = pin.ground(pinnedStream) //TODO pinnedStream map (_.toNormal) oder so

    tmsPolicy.add(groundedRules ++ groundedStream)

    val resultingModel = tmsPolicy.getModel() match {
      case Some(model) => Some(asPinnedAtoms(model, timePoint))
      case None => None
    }

    tmsPolicy.remove(groundedRules ++ groundedStream)

    resultingModel
  }

  def asPinnedAtoms(model: Model, timePoint: TimePoint) = model map {
    case p: PinnedAtom => p

    // in incremental mode we assume that all (resulting) atoms are meant to be at T
    case a: Atom => a(timePoint)

    //    case a: Atom => throw new IllegalArgumentException(f"The atom $a is an invalid result (it cannot be converted into a PinnedAtom)")
  }

}
