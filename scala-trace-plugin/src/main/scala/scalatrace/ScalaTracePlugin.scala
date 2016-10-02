package scalatrace

/**
 * Created by shiyu on 16/9/11.
 */

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class ScalaTracePlugin(val global: Global) extends Plugin {

  val name = "scalatrace"
  val description = "scalatrace code analyzing tool"
  val components = List[PluginComponent](WrapUp)

  override def processOptions(option: List[String], error: String => Unit): Unit = {
    if(!option.isEmpty) {
      try {
        val nameValue = option.head.split(":")
        nameValue(0) match {
          case "targetline" => WrapUp.targetLine = nameValue(1)
          case _ => throw new Exception
        }
      } catch {
        case e: Throwable => error("Option for scalatrace plugin failed. Use -P:scalatrace:help for help.")
      }
    }
  }

  override val optionsHelp = {
   Some(" -P:scalatrace:targetline:<line>\t\tSpecify the target line of the program. " +
     "This should include the relative path of the source file under 'src/' directory of a project. " +
     "For example -P:scalatrace:targetline:/library/scala/collection/Traversable.scala,line-21," +
     "where 'library' is a directory under 'src' folder of the Scala source code.")
  }

  private object WrapUp extends PluginComponent with WrapUpTypingTransformers {
    val global = ScalaTracePlugin.this.global
    val runsAfter = List[String]("typer")
    override val runsBefore = List[String]("patmat")
    override val description = "wrap up function calls at tail position"
    var targetLine = ""

    val phaseName = "wrapper"

    import global._

    def newPhase(_prev: Phase): Phase = new WrapUpPhase(_prev)

    class WrapUpPhase(prev: Phase) extends StdPhase(prev) {
      override def name = WrapUp.phaseName
      override def description = "wrap up function calls at tail position"
      def apply(unit: CompilationUnit): Unit = {
        unit.body = new WrapUpTypingTransformer(unit).transform(unit.body)
      }
    }
  }
}
