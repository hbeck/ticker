package evaluation.diss.programs

import core.lars.LarsProgram
import core.not
import evaluation.diss.Prepared.{a, b}
import evaluation.diss.programs.properties.AnalyticProgramProvider
import evaluation.diss.programs.properties.AnalyticProgramProvider.makeWindowAtom

// Created by hb on 02.05.18. negated version of basic.
trait NBasicProgramProvider extends AnalyticProgramProvider {

  def program(): LarsProgram = {
    val windowAtom = makeWindowAtom(winMod,windowSize,b)
    LarsProgram.from(a <= not(windowAtom))
  }

}
