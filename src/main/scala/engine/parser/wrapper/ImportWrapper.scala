package engine.parser.wrapper

import core.lars.WindowFunction

/**
  * Created by et on 22.03.17.
  */
case class ImportWrapper(importClass: String, params: Option[String], name: String) {
  ImportWrapper(importClass,params,name)
}

object ImportWrapper {
  //TODO make val if possible
  private var wfnObjects: Map[String,WindowFunction] = Map()
  private var wfnClasses: Map[String,Class[WindowFunction]] = Map()

  def apply(importClass: String, params: Option[String], name: String): Unit = {}

//  def getWfnObject(wfn: String): Option[WindowFunction] = wfnObjects.get(wfn)
  def getWfnClass(wfn: String): Option[Class[WindowFunction]] = wfnClasses.get(wfn)
  //TODO create a method which takes up to three window parameters and creates an object of the specified window function

  def getWfnObject(wType: String,
                      past: Option[ParamWrapper] = Option(ParamWrapper(0,None)),
                      next: Option[ParamWrapper] = Option(ParamWrapper(0,None)),
                      step: Option[ParamWrapper] = Option(ParamWrapper(1,None))): WindowFunction = ???
//TODO implement getnewwfnobject and fill in useful defaults

  //  windowFunctions += ("t" -> classOf[SlidingTimeWindow])
//  windowFunctions += ("#" -> classOf[SlidingTupleWindow])
//
//    private val foo = classOf[SlidingTupleWindow]
//    private val bar = foo.newInstance()
//    private val tar = bar.
//
//    def window(importClass: String, params: Option[String]): = ???
}
