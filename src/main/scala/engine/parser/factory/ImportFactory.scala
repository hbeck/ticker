package engine.parser.factory

import core.lars.WindowFunction

/**
  * Created by et on 22.03.17.
  */
case class ImportFactory(importClass: String, params: Option[String], name: String) {
  ImportFactory(importClass,params,name)
}

object ImportFactory {
  //TODO make val if possible
  private var wfnObjects: Map[String,WindowFunction] = Map()
  private var wfnClasses: Map[String,Class[WindowFunction]] = Map()

  def apply(importClass: String, params: Option[String], name: String): Unit = {
    //TODO add new import to wfn maps
  }

//  def getWfnObject(wfn: String): Option[WindowFunction] = wfnObjects.get(wfn)
  def getWfnClass(wfn: String): Option[Class[WindowFunction]] = wfnClasses.get(wfn)
  //TODO create a method which takes up to three window parameters and creates an object of the specified window function

  def getWfnObject(wType: String,
                   past: Option[ParamFactory] = Option(ParamFactory(0,None)),
                   next: Option[ParamFactory] = Option(ParamFactory(0,None)),
                   step: Option[ParamFactory] = Option(ParamFactory(1,None))): WindowFunction = ???
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
