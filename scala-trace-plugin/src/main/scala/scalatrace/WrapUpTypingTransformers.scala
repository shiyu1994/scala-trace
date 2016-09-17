package scalatrace

/**
 * Created by shiyu on 16/9/11.
 */

import scala.tools.nsc.transform.TypingTransformers

trait WrapUpTypingTransformers extends TypingTransformers with FunContexts with NameTrees {

  import global._
  import ReadWriteTree._

  protected class WrapUpTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) with FunContext {

    val global = WrapUpTypingTransformers.this.global

    override def transform(tree: Tree): Tree = {
      val newtree = wrapUp(tree)
      newtree
    }

    def flattenBlock(tree: Tree): Tree = {
      def extractStats(tree: Tree): List[Tree] = tree match {
        case Block(stats, expr) => (stats map { extractStats(_) } flatten):::extractStats(expr)
        case _=> tree::Nil
      }

      tree match {
        case Block(stats, expr) =>
          val extractedExpr = extractStats(expr)
          treeCopy.Block(tree, (stats map { extractStats(_) } flatten):::(extractStats(expr) dropRight 1), extractedExpr last)
        case _ => tree
      }
    }

    def getConstructorArgs(implBody: List[Tree]): String = {
      var args: String = ""
      for(tree <- implBody) {
        tree match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if(tree.symbol.isConstructor) =>
            vparamss foreach {vparams => vparams foreach {vparam => args = (args + " " + vparam.name)}}
          case _ =>
        }
      }
      return args
    }

    def getValsAndParents(implBody: List[Tree], symbol: Symbol): String = {
      var args: String = ""
      implBody foreach { tree =>
        tree match {
          case ValDef(mods, name, tpt, rhs) => args = args + " " + tree.name
          case _ => ""
        }
      }
      symbol.parentSymbols foreach { sym =>
        args = args + " " + sym.name
      }
      return args
    }

    def insertIntoConstructor(implBody: List[Tree], passArgs: Tree, returnObj: Tree): List[Tree] = {
      implBody map {  tree => tree match
        {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (tree.symbol.isConstructor)  => {
            val newRhs = rhs match {
              case Block(stats, expr) =>
               treeCopy.Block(rhs, stats ::: List(passArgs), returnObj)
              case _ => rhs
            }
            treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, newRhs)
          }

          case _ => tree
        }
      }
    }

    def wrapUp(tree: Tree, isReturnValue: Boolean = false, msg: List[(String, Position)] = Nil): Tree = {

      tree match {

        case  PackageDef(pid, stats) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.PackageDef(tree, pid, stats.map { wrapUp(_, false) } )
          exit()
          newTree

        case  ClassDef(mods, name, tparams, impl) =>
          enter(tree, tree.symbol)
          //println(tree.symbol.parentSymbols)
          val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id,
            tree.name + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ClassDef(tree, mods, name, tparams,
            treeCopy.Template(impl, impl.parents, impl.self,
              insertIntoConstructor(impl.body.map { wrapUp(_, false) }, passConstructorArgs, returnObj)  ))
          exit()
          newTree

        case  ModuleDef(mods, name, impl) =>
          enter(tree, tree.symbol)
          //println(tree.symbol.parentSymbols)
          val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id, tree.name + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ModuleDef(tree, mods, name,
            treeCopy.Template(impl, impl.parents, impl.self, passConstructorArgs :: impl.body.map { wrapUp(_, false) } ::: List(returnObj) ))
          exit()
          newTree

        case  ValDef(mods, name, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.ValDef(tree, mods, name, tpt, wrapUp(rhs, true, (rhs.name + " $$$ " + tree.name, tree.pos)::Nil))
          exit()
          newTree

        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = if(!name.toString.contains("<init>"))
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, true, (rhs.name + " $$$ " + tree.name, tree.pos)::Nil))))
          else tree
          exit()
          newTree

        //TODO
        case LabelDef(name, params, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.LabelDef(tree, name, params,
            wrapUp(rhs, false, (rhs.name + " $$$ " + tree.name, tree.pos)::Nil))
          exit()
          newTree

        case  Template(parents, self, body) =>
          treeCopy.Template(tree, parents, self, body map { wrapUp(_, false) })

        case  Block(stats, expr) =>
          treeCopy.Block(tree, stats map { wrapUp(_, false) },
            wrapUp(expr, isReturnValue, (expr.name + " $$$ " + tree.name + "\n" + msg, tree.pos)::Nil))

        //TODO
        case  CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, guard,
            wrapUp(body, isReturnValue, (body.name + " $$$ " + tree.name + "\n" + msg, tree.pos)::Nil))

        //TODO
        case  Alternative(trees) =>
          treeCopy.Alternative(tree, trees map { wrapUp(_, isReturnValue) })

        //TODO
        case  Bind(name, body) =>
          treeCopy.Bind(tree, name,
            wrapUp(body, isReturnValue, (body.name + " $$$ " + tree.name, tree.pos)::Nil))

        case Apply(fun, args) =>
          val selectList = fun.allNames filter(_ != fun.name)
          val read =
          if(args.length > 0)
            (args map { _ name } reduce { _ + " " + _ }) + " " + {if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}}
          else if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}


          def wrapUpFun(fun: Tree): Tree = {
            fun match {
              case Select(qualifier, selector) => treeCopy.Select(fun, wrapUp(qualifier, true, ("say something", fun.pos)::Nil), selector)
              case TypeApply(_fun, args) => treeCopy.TypeApply(fun, wrapUpFun(_fun), args)
              case Apply(fun, args) => wrapUp(fun, true, ("say something", fun.pos)::Nil)
              case _ => fun
            }
          }

          if(args.length >= 1)
            wrapWithPrint(treeCopy.Apply(tree, fun,
              ( args dropRight 1 map { wrapUp(_) } ) :::
                List(wrapWithPrint((wrapUp(args last, false)), true, (read + " $$$ " + fun.name, tree.pos)::Nil)) ), isReturnValue, msg)
          else
            wrapWithPrint(treeCopy.Apply(tree, fun, Nil), isReturnValue, (read + " *** " + fun.name, tree.pos)::msg)

        //TODO
        case  UnApply(fun, args) =>
          val selectList = fun.allNames filter(_ != fun.name)
          val read =
            if(args.length > 0)
              (args map { _ name } reduce { _ + " " + _ }) + " " + {if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}}
            else if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}

          if(args.length >= 1)
            wrapWithPrint(treeCopy.UnApply(tree, fun,
              ( args dropRight 1 map { wrapUp(_) } ) :::
                List(wrapWithPrint((wrapUp(args last, false)), true, (read + " $$$ " + fun.name, tree.pos)::Nil)) ) , isReturnValue, msg)
          else
            wrapWithPrint(treeCopy.UnApply(tree, fun, Nil), isReturnValue, (read + " *** " + fun.name, tree.pos)::msg)

        case ApplyDynamic(qual, args) =>
          val selectList = qual.allNames filter(_ != qual.name)
          val read =
            if(args.length > 0)
              (args map { _ name } reduce { _ + " " + _ }) + " " + {if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}}
            else if(selectList.isEmpty) "" else selectList reduce {_ + " " + _}

          if(args.length >= 1)
            wrapWithPrint(treeCopy.ApplyDynamic(tree, qual,
              ( args dropRight 1 map { wrapUp(_) } ) :::
                List(wrapWithPrint((wrapUp(args last, false)), true, (read + " $$$ " + qual.name, tree.pos)::Nil)) ), isReturnValue, msg)
          else
            wrapWithPrint(treeCopy.ApplyDynamic(tree, qual, Nil), isReturnValue, msg)

        case Function(vparams, body) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.Function(tree, vparams,
            wrapUp(body, true, (body.name + " $$$ " + tree.name, tree.pos)::Nil))
          exit()
          newTree

        case Assign(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.Assign(tree, lhs,
            wrapUp(rhs, true, (rhs.name + " $$$ " + lhs.name, tree.pos)::Nil))
          exit()
          newTree

        case AssignOrNamedArg(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.AssignOrNamedArg(tree, lhs,
            wrapUp(rhs, true, (rhs.name + " $$$ " + lhs.name, tree.pos)::Nil))
          exit()
          newTree

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, wrapUp(cond, false),
            wrapUp(thenp, isReturnValue, (cond.name + " " + thenp.name + " $$$ " + tree.name + "\n", tree.pos) :: msg),
            wrapUp(elsep, isReturnValue, (cond.name + " " + elsep.name + " $$$ " + tree.name + "\n", tree.pos) :: msg))

        case Match(selector, cases) =>
          treeCopy.Match(tree, wrapUp(selector, false),
            cases map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard,
              wrapUp(_case.body, isReturnValue, (selector.name + " " + _case.body.name + " $$$ " + tree.name + "\n", tree.pos) :: msg)) })

        case Return(expr) =>
          treeCopy.Return(tree, wrapUp(expr, true, msg))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, wrapUp(block, isReturnValue, (block.name + " $$$ " + tree.name + "\n", tree.pos) :: msg),
            catches map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard, wrapUp(_case.body, isReturnValue)) },
            wrapUp(finalizer, isReturnValue))

        case Throw(expr) =>
          treeCopy.Throw(tree, wrapUp(expr, false, msg))

        case New(tpt) =>
          treeCopy.New(tree, wrapUp(tpt, isReturnValue, msg))

        case ArrayValue(elemtpt, trees) =>
          println("======================= ArrayValue " + tree)
          println(tree.pos)
          treeCopy.ArrayValue(tree, elemtpt, trees map { wrapUp(_, false) })

        case Typed(expr, tpt) =>
          treeCopy.Typed(tree, wrapUp(expr, isReturnValue, msg), tpt)

        case TypeApply(fun, args) =>
          treeCopy.TypeApply(tree, fun, args map { wrapUp(_, isReturnValue, msg) })

        case Super(qual, mix) =>
          println("======================= Super " + tree)
          println(tree.pos)
          treeCopy.Super(tree, qual, mix)

        case This(qual) => wrapWithPrint(tree, isReturnValue, msg)

        case Ident(name) => if(tree.symbol.isValue) {
          wrapWithPrint(tree, isReturnValue, msg)
        } else {
          tree
        }

        case Literal(value) => wrapWithPrint(tree, isReturnValue, msg)

        case Select(qualifier, selector) => if(qualifier.symbol == null || qualifier.symbol.isValue) {
          wrapWithPrint(treeCopy.Select(tree, wrapUp(qualifier), selector), isReturnValue, msg)
        } else {
          wrapWithPrint(tree, isReturnValue, msg)
        }

        case RefTree(qualifier, selector) =>
          println("======================= RefTree " + tree)
          println(tree.pos)
          tree

        case ReferenceToBoxed(idt) =>
          println("======================= ReferenceToBoxed " + tree)
          println(tree.pos)
          tree

        case TypeTree() => tree

        case Annotated(annot, arg) =>
          println("======================= Annotated " + tree)
          println(tree.pos)
          tree

        case SingletonTypeTree(ref) =>
          println("======================= SingletonTypeTree " + tree)
          println(tree.pos)
          tree

        case SelectFromTypeTree(qualifier, selector) =>
          println("======================= SelectFromTypeTree " + tree)
          println(tree.pos)
          tree

        case CompoundTypeTree(templ) =>
          println("======================= CompoundTypeTree " + tree)
          println(tree.pos)
          tree

        case AppliedTypeTree(tpt, args) =>
          println("======================= AppliedTypeTree " + tree)
          println(tree.pos)
          tree

        case TypeBoundsTree(lo, hi) =>
          println("======================= TypeBoundsTree " + tree)
          println(tree.pos)
          tree

        case ExistentialTypeTree(tpt, whereClauses) =>
          println("======================= ExistentialTypeTree " + tree)
          println(tree.pos)
          tree

        case Import(expr, selectors) => tree

        case TypeDef(mods, name, tparams, rhs) => tree

        case Star(elem) =>
          println("======================= Star " + tree)
          println(tree.pos)
          tree

        case EmptyTree => tree
      }
    }
  }
}
