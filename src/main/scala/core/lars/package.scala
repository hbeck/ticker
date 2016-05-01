package core

import engine.{Time, Stream}

/**
  * Created by FM on 29.04.16.
  */
package object lars {
  type WindowSize = Int

  //  type WindowFunction = (WindowSize) => (Stream, Time) => Stream
  type WindowFunction = (Stream, Time) => Stream


}
