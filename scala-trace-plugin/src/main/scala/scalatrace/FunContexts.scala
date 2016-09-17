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

    @inline private def makePrintTree(msg: List[(String, Position)]): List[Tree] =
    {
      msg map { msg =>
        val (dataFlow, pos) = msg
        val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
          Literal(Constant(dataFlow)):: Literal(Constant(pos.focus.toString)) :: Nil)
        localTyper.typed(newTree)
      }
    }

    @inline protected def makePrintCall(name: String, msg: String, pos: Position): Tree = {
      localTyper.typed(Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("log")),
        Literal(Constant(msg)):: Literal(Constant(pos.focus.toString)) :: Nil))
    }

    @inline private def makeWrapperAfterTree(tree: Tree, msg: List[(String, Position)]): Tree =
    {
      val newTree = Apply(Select(Select(Ident("scalatrace"), newTermName("ScalaTrace")), newTermName("__wrapperAfter")), tree :: makePrintTree(msg))
      localTyper.typed(newTree)
    }

    def enter(tree: Tree, symbol: Symbol = contexts.head._2) {
      contexts = (tree, symbol) :: contexts
    }

    def exit(): Unit = {
      contexts = contexts.tail
    }

    def wrapWithPrint(tree: Tree, isReturnValue: Boolean, msg: List[(String, Position)]): Tree =
      if (isReturnValue) {
        printAfter(tree, msg)
      } else {
        treeCopy.Block(tree, makePrintTree(msg), tree)
      }


    def tailCall(symbol: Symbol, tree: Tree): Boolean = tree match {

      case Apply(fun, args) => ((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
        (!args.isEmpty && (args map { tailCall(symbol, _) } reduce {_||_}))

      case UnApply(fun, args) => ((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
        (!args.isEmpty && (args map { tailCall(symbol, _) } reduce {_||_}))

      case ApplyDynamic(fun, args) => ((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
        (!args.isEmpty && (args map { tailCall(symbol, _) } reduce {_||_}))

      case Block(_, expr) => tailCall(symbol, expr)

      case If(_, thenp, elsep) => tailCall(symbol, thenp) || tailCall(symbol, elsep)

      case Match(_, cases) => !cases.isEmpty && (cases map { tailCall(symbol, _) } reduce {_||_})

      case Return(expr) => tailCall(symbol, expr)

      case Try(block, _, _) => tailCall(symbol, block) //TODO

      case CaseDef(_, _, body) => tailCall(symbol, body)

      case ArrayValue(_, elems) => !elems.isEmpty && (elems map { tailCall(symbol, _) } reduce {_||_})

      case _ => false
    }

    def printAfter(tree: Tree, msg: List[(String, Position)]): Tree = {
      tree match {
        case Apply(fun, args)  =>
          val (parentTree, symbol) = contexts.head
          if(parentTree.toString().startsWith("@scala.annotation.tailrec") &&
            (((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
              (!args.isEmpty && ((args map { tailCall(symbol, _) } ) reduce {_||_}))))
            treeCopy.Block(tree, makePrintTree(msg), tree)
          else
            makeWrapperAfterTree(tree, msg)

        case UnApply(fun, args)  =>
          val (parentTree, symbol) = contexts.head
          if(parentTree.toString().startsWith("@scala.annotation.tailrec") &&
            (((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
              (!args.isEmpty && ((args map { tailCall(symbol, _) } ) reduce {_||_}))))
            treeCopy.Block(tree, makePrintTree(msg), tree)
          else
            makeWrapperAfterTree(tree, msg)

        case ApplyDynamic(fun, args)  =>
          val (parentTree, symbol) = contexts.head
          if(parentTree.toString().startsWith("@scala.annotation.tailrec") &&
            (((fun.symbol ne null) && fun.symbol.name == symbol.name) ||
              (!args.isEmpty && ((args map { tailCall(symbol, _) } ) reduce {_||_}))))
            treeCopy.Block(tree, makePrintTree(msg), tree)
          else
            makeWrapperAfterTree(tree, msg)

        case Select(qualifier, selector) =>
          val (parentTree, symbol) = contexts.head

          val returnTreeOfLazyDef = if(symbol.isLazy) {
            parentTree match {
              case DefDef(_, _, _, _, _, Block(_, expr)) => expr
              case _ => EmptyTree
            }
          } else EmptyTree

          if((parentTree.toString().startsWith("@scala.annotation.tailrec") &&
            (((tree.symbol ne null) && tree.symbol.name == symbol.name))) || (tree == returnTreeOfLazyDef))
            treeCopy.Block(tree, makePrintTree(msg), tree)
          else
            makeWrapperAfterTree(tree, msg)

        case Ident(_) => val (parentTree, symbol) = contexts.head
          if(parentTree.toString().startsWith("@scala.annotation.tailrec") &&
            (((tree.symbol ne null) && tree.symbol.name == symbol.name) ))
            treeCopy.Block(tree, makePrintTree(msg), tree)
          else
            makeWrapperAfterTree(tree, msg)

        case Typed(expr, tpt) => treeCopy.Typed(tree, printAfter(expr, msg), tpt)

        case Block(stats, expr) => treeCopy.Block(tree, stats, printAfter(expr, msg))

        case If(cond, thenp, elsep) => treeCopy.If(tree, cond, printAfter(thenp, msg), printAfter(elsep, msg))

        case Match(selector, cases) => treeCopy.Match(tree, selector,
          cases map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard, printAfter(_case.body, msg)) })

        case Return(expr) => treeCopy.Return(tree, printAfter(expr, msg))

        case Try(block, catches, finalizer) => treeCopy.Try(tree, printAfter(block, msg), catches, finalizer) //TODO

        case Throw(expr) => treeCopy.Throw(tree, printAfter(expr, msg))

        case CaseDef(pat, guard, body) => treeCopy.CaseDef(tree, pat, guard, printAfter(body, msg))

        case ArrayValue(elemtpt, elems) if(!elems.isEmpty) => //TODO
          treeCopy.ArrayValue(tree, elemtpt, (elems dropRight 1):::List(printAfter(elems last, msg)) )

        case _ => treeCopy.Block(tree, makePrintTree(msg), tree)
      }
    }
  }
}


