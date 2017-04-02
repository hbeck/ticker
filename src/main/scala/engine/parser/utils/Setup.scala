package engine.parser.utils

import core.lars.WindowFunction
import engine.parser.factory.ImportFactory

/**
  * Created by et on 01.04.17.
  *
  * Use this object to setup the parser with your own window functions
  *
  */
object Setup {

  def addWindowFunction(name: String, wfn: WindowFunction): Unit = {
    ImportFactory.registerWinfowFunction(name,wfn)
  }

}
