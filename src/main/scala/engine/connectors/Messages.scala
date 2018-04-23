package engine.connectors

import core.Model
import core.lars.TimePoint
import engine.Engine

object Messages {

  def model(engine: Engine, timepoint: TimePoint, model: Model): String = {
    val outputTime = engine.convertToTime(timepoint)
    f"$outputTime${engine.clockTimeUnitWritten} (t=$timepoint): $model"
  }

  def noModel(engine: Engine, timepoint: TimePoint): String = {
    val outputTime = engine.convertToTime(timepoint)
    f"$outputTime${engine.clockTimeUnitWritten} (t=$timepoint): -"
  }

}
