package jtms

import core.{Program, Evaluation}

/**
  * Created by FM on 25.02.16.
  */
class jTmn extends Evaluation {

  def apply(program: Program) = {
    val tmn = TMN(program)

    val singleModel = tmn.getModel.get

    Set(singleModel)
  }

}
