package engine.parser.factory

import core.lars.WindowFunction

/**
  * Created by et on 01.04.17.
  */
abstract class WindowFactory(w: String) {

  val wfn: WindowFunction
}
