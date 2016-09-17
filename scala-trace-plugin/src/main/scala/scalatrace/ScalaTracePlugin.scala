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

  private object WrapUp extends PluginComponent with WrapUpTypingTransformers {
    val global = ScalaTracePlugin.this.global
    val runsAfter = List[String]("typer")
    override val runsBefore = List[String]("patmat")
    override val description = "wrap up function calls at tail position"
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
