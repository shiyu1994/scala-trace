package scalatrace

/**
 * Created by shiyu on 16/9/13.
 */

import scala.tools.nsc.Global

trait FunContexts {

  val global: Global

  import global._

  protected trait FunContext {

    var localTyper: analyzer.Typer
    var contexts: List[(Tree, Symbol)] = Nil

    @inline protected def makePrintTrees(msgs: List[(String, Position)]): List[Tree] = {
      msgs map { msg =>
        val (dataFlow, pos) = msg
        val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
          Literal(Constant(dataFlow)):: Literal(Constant(pos.focus.toString)) :: Nil)
        localTyper.typed(newTree)
      }
    }

    @inline protected def makePrintTree(msg: (String, Position)): Tree = {
        val (dataFlow, pos) = msg
        val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
          Literal(Constant(dataFlow)):: Literal(Constant(pos.focus.toString)) :: Nil)
        localTyper.typed(newTree)
    }


    @inline protected def makePrintTree(msg: String, pos: Position): Tree = {
        val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
          Literal(Constant(msg)):: Literal(Constant(pos.focus.toString)) :: Nil)
        localTyper.typed(newTree)
    }

    @inline protected def makePrintWrapTree(tree: Tree, msg: List[(String, Position)]): Tree = {
      val logTrees: List[Tree] = msg map { a =>
        val (msg, pos) = a
        Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
        Literal(Constant(msg)):: Literal(Constant(pos.focus.toString)) :: Nil) }
      val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("__wrapperAfter")),
         tree :: logTrees ::: Nil)
      localTyper.typed(newTree)
    }

    @inline protected def makePrintCall(name: String, msg: String, pos: Position): Tree = {
      localTyper.typed(Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
        Literal(Constant(msg)):: Literal(Constant(pos.focus.toString)) :: Nil))
    }

    def enter(tree: Tree, symbol: Symbol = contexts.head._2) {
      contexts = (tree, symbol) :: contexts
    }

    def exit(): Unit = {
      contexts = contexts.tail
    }

    def wrapWithPrint(tree: Tree, msg: List[(String, Position)]): Tree =
    if(msg.isEmpty)
      tree
    else
        localTyper.typed(Block(makePrintTrees(msg), tree))
    }

}

