package evaluation.diss.programs

import core.Model

trait NoVerification extends AnalyticProgramProvider {

  override def verify_time_at(model: Model, t: Int): Unit = {
    //
  }

  override def verify_time_diamond(model: Model, t: Int): Unit = {
    //
  }

  override def verify_time_box(model: Model, t: Int): Unit = {
    //
  }

  override def verify_count_at(model: Model, t: Int): Unit = {
    //
  }

  override def verify_count_diamond(model: Model, t: Int): Unit = {
    //
  }

  override def verify_count_box(model: Model, t: Int): Unit = {
    //
  }

}
