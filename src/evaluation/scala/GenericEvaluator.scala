import core.lars.LarsProgram


//import scala.reflect.runtime.universe
//import scala.reflect.runtime._

/**
  * Created by FM on 08.08.16.
  */
object GenericEvaluator {

  def main(args: Array[String]): Unit = {
//    val engine = Evaluator.buildEngineFromArguments(args, loadProgram)

    // feed data
//    engine.append(1)(Atom("a"))
  }


  def loadProgram(programIdentifier: String): LarsProgram = {

    //    var classLoader = new java.net.URLClassLoader(
    //      Array(new File("module.jar").toURI.toURL),
    //      this.getClass.getClassLoader)
    //    val mirror = universe.runtimeMirror(getClass.getClassLoader)
    //    val cls = mirror.classSymbol(Class.forName(programIdentifier))
    //    val module = cls.companion.asModule
    //    val i = mirror.reflectModule(module).instance
    //    val c = cm.classLoader.loadClass(programIdentifier).asInstanceOf[ProgramProvider]

    //    c.program
    //    i.asInstanceOf[ {val program: LarsProgram}].program
    return null
  }
}
