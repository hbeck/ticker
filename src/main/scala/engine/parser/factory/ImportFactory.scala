package engine.parser.factory

import core.lars.WindowFunction
import engine.parser.wrapper.ParamWrapper

/**
  * Created by et on 22.03.17.
  */
case class ImportFactory(importClass: String, params: Option[String], name: String)

object ImportFactory {


  private var wfnObjects: Map[String,WindowFactory] = Map()

  def apply(importClass: String, params: Option[String], name: String) = {
    if(params.isDefined) ??? //TODO implement this as soon as it can be actually useful

    if(!wfnObjects.contains(name)) {
      //This should work for classes with paramterless constructors
      wfnObjects += (name -> Class.forName(importClass).asInstanceOf[WindowFactory])
    }
  }

  def registerWinfowFunction(name: String, wfn: WindowFactory): Unit = wfnObjects += (name -> wfn)

  def getWindowFunction(name: String): Option[WindowFactory] = wfnObjects.get(name)
}
