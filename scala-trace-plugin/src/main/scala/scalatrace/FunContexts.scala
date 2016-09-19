package scalatrace

/**
 * Created by shiyu on 16/9/13.
 */

import java.util

import scala.tools.nsc.Global

trait FunContexts extends DataFlows {

  val global: Global

  import global._

  protected trait FunContext {
    var localTyper: analyzer.Typer
    var contexts: List[(Tree, Symbol)] = Nil
    var lazyBuffer: List[DataFlow] = Nil

    private def parsePos(pos: Position): String = {
      def leaveOutOffset(pos: Position): String = {
        val stringPos = pos.toString
        if(stringPos.contains("NoPosition")) "NoPosition"
        else stringPos.substring(stringPos.indexOf("/src/") + 4, stringPos.lastIndexOf(","))
      }
      /*if(pos.isRange) {
        "[" + leaveOutOffset(pos.focusStart) + "|" + leaveOutOffset(pos.focusEnd) + "]"
      } else*/ leaveOutOffset(pos.focus)
    }

    protected def makeLogTree(dataFlow: DataFlow): Tree = {
      localTyper.typed(Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
        Literal(Constant((dataFlow.froms map {_.getName()}).foldLeft("") {(froms, from) => if(froms == "") from else froms + " " + from})) ::
          Literal(Constant((dataFlow.tos map {_.getName()}).foldLeft(""){(tos, to) => if(tos == "") to else tos + " " + to})) ::
          Literal(Constant((dataFlow.poses map {parsePos(_)}).foldLeft(""){(poses, pos) => if(poses == "") pos else poses + " " + pos})) :: Nil))
    }

    private def makeWrapLogTree(tree: Tree, dataFlows: List[DataFlow]): Tree = {
      val logDataFlows: List[Tree] = (dataFlows map { dataFlow =>
        (List(
          (dataFlow.froms map {_.getName()}).foldLeft("") {(froms, from) => if(froms == "") from else froms + " " + from},
          (dataFlow.tos map {_.getName()}).foldLeft(""){(tos, to) => if(tos == "") to else tos + " " + to},
          (dataFlow.poses map { parsePos(_) }).foldLeft(""){(poses, pos) => if(poses == "") pos else poses + " " + pos})).
          reduce(_ + ":" + _)
      }) map {logDataFlow => Literal(Constant(logDataFlow))}

      localTyper.typed(Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("__wrapperBefore")),
        Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("multiLog")), logDataFlows)
          :: tree :: Nil))
    }

    protected def enter(tree: Tree, symbol: Symbol = contexts.head._2) {
      contexts = (tree, symbol) :: contexts
    }

    protected def exit(): Unit = {
      contexts = contexts.tail
    }

    protected def wrapWithPrint(tree: Tree, dataFlows: List[DataFlow]): Tree = {
      val (parentTree, symbol) = contexts.head

      if(dataFlows.isEmpty) tree
      else if(symbol.isLazy) {
        val returnTreeOfLazyDef = if (symbol.isLazy) {
          parentTree match {
            case DefDef(_, _, _, _, _, Block(_, expr)) => expr
            case _ => EmptyTree
          }
        } else EmptyTree

        tree match {
          case Select(_, _) if(tree == returnTreeOfLazyDef) =>
            localTyper.typed(Block(dataFlows filter {dataFlow => !dataFlow.froms.isEmpty && !dataFlow.tos.isEmpty} map {makeLogTree(_)}, tree))
          case _ =>
            val newTree = makeWrapLogTree (tree, lazyBuffer ::: dataFlows filter {dataFlow => !dataFlow.froms.isEmpty && !dataFlow.tos.isEmpty})
            lazyBuffer = Nil
            newTree
        }
      }
      else localTyper.typed(Block(dataFlows filter {dataFlow => !dataFlow.froms.isEmpty && !dataFlow.tos.isEmpty} map {makeLogTree(_)}, tree))
    }
  }
}


