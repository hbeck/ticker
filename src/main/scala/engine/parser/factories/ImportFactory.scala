package engine.parser.factories

import java.net.{URL, URLClassLoader}

import engine.parser.InvalidSyntaxException
import engine.parser.factories.slidingWindowFunctionFactory._
import engine.parser.wrapper.ParamWrapper


/**
  * Created by et on 22.03.17.
  */

case class ImportFactory(importClass: String, params: List[ParamWrapper], name: String) {
  ImportFactory.register(importClass,params,name)
}

object ImportFactory {

  private var importFactories: Map[String,WindowFunctionFactory] = defaultFactories

  @throws[ImportException]
  def register(importClass: String, params: List[ParamWrapper], name: String): Unit = {

    var clazz:Option[Class[_]] = None

    try {
      if (importClass.startsWith("/")) {
        val splits = importClass.splitAt(importClass.lastIndexOf("/") + 1)

        val url = new URL("file://" + splits._1)
        val loader = new URLClassLoader(Array(url))

        clazz = Some(loader.loadClass(splits._2))
      } else {
        clazz = Some(Class.forName(importClass))
      }
    } catch {
      case exception: ClassNotFoundException => throw new ImportException("Class "+importClass+" cannot be found. Make sure to specify the path to the .class file and check your compiler version.")
    }

    val constructor = clazz.get.getConstructor(classOf[List[WindowFunctionFactory]])

    constructor.newInstance(params) match {
      case factory: WindowFunctionFactory => importFactories += (name -> factory)
      case _ => throw new ImportException("The specified class is not a subtype of WindowFunctionFactory")
    }
  }

  @throws[InvalidSyntaxException]
  def getWinfowFunction(name: String): WindowFunctionFactory = {
    val wfn = importFactories.get(name)
    if(wfn.isDefined) return wfn.get
    throw new InvalidSyntaxException("A window function with name '"+name+"' has not been imported.")
  }

  private def defaultFactories: Map[String,WindowFunctionFactory] = {
    Map("t" -> SlidingTimeWindowFactory(),
        "#" -> SlidingTupleWindowFactory())
  }
}
