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

    def parseFunQualifier(tree: Tree): (Tree, List[String]) = {
      tree match {
        case Select(qualifier, selector) =>
          val (qualTree, qualString) = parseFunQualifier(qualifier)
          if(tree.symbol.isMethod) {
            (treeCopy.Select(tree, qualTree, selector), tree.downName :: qualString)
          } else {
            (treeCopy.Select(tree, qualTree, selector), tree.upName :: qualString)
          }
        case Apply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          (treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }), fun.downName::qualString)
        case Block(stats, expr) =>
          (treeCopy.Block(tree, stats map { wrapUp(_) }, wrapUp(expr, Some(tree.name))), tree.downName :: Nil)
        case This(qual) =>
          (tree, Nil)
        case Ident(name) =>
          if(tree.symbol.isMethod)
            (tree, tree.downName :: Nil)
          else
            (tree, tree.upName :: Nil)
        case New(tpt) =>
          (tree, tree.downName :: Nil)
        case Literal(value) =>
          (tree, Nil)
        case Typed(expr, tpt) =>
          val (qualTree, qualString) = parseFunQualifier(expr)
          (treeCopy.Typed(tree, qualTree, tpt), qualString)
        case TypeApply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          (treeCopy.TypeApply(tree, qualTree, args), fun.downName :: qualString)
        case Function(vparams, body) =>
          (wrapUp(tree), tree.downName :: Nil)
        case Match(selector, cases) =>
          (wrapUp(tree), tree.downName :: Nil)
        case Super(qual, mix) =>
          (tree, Nil)
        case If(cond, thenp, elsep) =>
          (wrapUp(tree), tree.downName :: Nil)
        case Try(block, catches, finalizer) =>
          (wrapUp(tree), tree.downName :: Nil)
      }
    }

    def parseFun(tree: Tree): (Tree, List[String]) = {
      tree match {
        case Select(qualifier, selector) =>
          val (qualTree, qualString) = parseFunQualifier(qualifier)
          (treeCopy.Select(tree, qualTree, selector), qualString)
        case TypeApply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          (treeCopy.TypeApply(tree, qualTree, args), qualString)
        case Apply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          (treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }), qualString)
        case Ident(name) => (tree, Nil)
        case New(tpt) => (tree, Nil)
        case This(qual) => (tree, Nil)
      }
    }

    def wrapUp(tree: Tree, beDependentBy: Option[String] = None, toBePrint: List[(String, Position)] = Nil): Tree = {

      tree match {

        case  PackageDef(pid, stats) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.PackageDef(tree, pid, stats.map { wrapUp(_) } )
          exit()
          newTree
//TODO ClassDef
        case  ClassDef(mods, name, tparams, impl) =>
          enter(tree, tree.symbol)
          //val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id,
          //  tree.name + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          //val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ClassDef(tree, mods, name, tparams,
            treeCopy.Template(impl, impl.parents, impl.self,
              impl.body.map { wrapUp(_) }  ))
          exit()
          newTree
//TODO ModuleDef
        case  ModuleDef(mods, name, impl) =>
          enter(tree, tree.symbol)
          //println(tree.symbol.parentSymbols)
          //val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id, tree.name + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          //val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ModuleDef(tree, mods, name,
            treeCopy.Template(impl, impl.parents, impl.self, impl.body.map { wrapUp(_) }  ))
          exit()
          newTree

        case  ValDef(mods, name, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.ValDef(tree, mods, name, tpt, wrapUp(rhs, Some(tree.name)))
          exit()
          newTree

        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = if(!name.toString.contains("<init>"))
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(tree.name)))))
          else tree
          exit()
          newTree


        case LabelDef(name, params, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.LabelDef(tree, name, params,
            wrapUp(rhs, Some(tree.name)))
          exit()
          newTree

        case  Template(parents, self, body) =>
          treeCopy.Template(tree, parents, self, body map { wrapUp(_) })

        case  Block(stats, expr) =>
         // println(tree + " " + tree.pos)
          treeCopy.Block(tree,   (stats map { wrapUp(_) } ),
            wrapUp(expr, Some(tree.name), (toBePrint ::: List((tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))))
          tree

        case  CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, guard,
            wrapUp(body, None, toBePrint ::: List((guard.upName + " " +  body.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos))))

        //TODO
        case  Alternative(trees) =>
          treeCopy.Alternative(tree, trees map { wrapUp(_) })

        //TODO
        case  Bind(name, body) =>
          treeCopy.Bind(tree, name,
            wrapUp(body))

        case Apply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          val allDeps: List[String] = ((args map {_.name}) ::: qualString)
          val allDependces: String = if(allDeps.length > 0) allDeps reduce {_ + " " + _} else ""
          wrapWithPrint(treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }),
            toBePrint ::: List((allDependces + " " + tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))


        //TODO
        case  UnApply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          val allDeps = ((args map {_.name}) ::: qualString)
          val allDependces = if(allDeps.length > 0) allDeps reduce {_ + " " + _} else ""
          wrapWithPrint(treeCopy.ApplyDynamic(tree, qualTree, args map { wrapUp(_) }),
            toBePrint ::: List((allDependces + " " + tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case ApplyDynamic(qual, args) =>
          val (qualTree, qualString) = parseFun(qual)
          val allDeps = ((args map {_.name}) ::: qualString)
          val allDependces = if(allDeps.length > 0) allDeps reduce {_ + " " + _} else ""
          wrapWithPrint(treeCopy.ApplyDynamic(tree, qualTree, args map { wrapUp(_) }),
            toBePrint ::: List((allDependces + " " + tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Function(vparams, body) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.Function(tree, vparams,
            wrapUp(body, Some(tree.name)))
          exit()
          wrapWithPrint(newTree, toBePrint ::: List((tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Assign(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.Assign(tree, lhs,
            wrapUp(rhs, Some(lhs.name)))
          wrapWithPrint(newTree, toBePrint)
          exit()
          newTree

        case AssignOrNamedArg(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.AssignOrNamedArg(tree, lhs,
            wrapUp(rhs, Some(lhs.name)))
          wrapWithPrint(newTree, toBePrint)
          exit()
          newTree

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, wrapWithPrint(wrapUp(cond), toBePrint),
            wrapUp(thenp, None, (cond.upName + " " + thenp.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos) :: Nil),
            wrapUp(elsep, None, (cond.upName + " " + elsep.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos) :: Nil))

        case Match(selector, cases) =>
          treeCopy.Match(tree, wrapWithPrint(wrapUp(selector), toBePrint),
            cases map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard,
              wrapUp(_case.body, None, (selector.upName + " " + _case.body.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos) :: Nil)) })

        case Return(expr) =>
          treeCopy.Return(tree, wrapUp(expr, beDependentBy, toBePrint))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, wrapUp(block, beDependentBy, toBePrint),
            catches map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard, wrapUp(_case.body)) },
            wrapUp(finalizer))

        case Throw(expr) =>
          treeCopy.Throw(tree, wrapUp(expr))

        case New(tpt) =>
          treeCopy.New(tree, wrapUp(tpt))

          //TODO
        case ArrayValue(elemtpt, trees) =>
          println("======================= ArrayValue " + tree)
          println(tree.pos)
          treeCopy.ArrayValue(tree, elemtpt, trees map { wrapUp(_) })

        case Typed(expr, tpt) =>
          treeCopy.Typed(tree, wrapUp(expr, beDependentBy, toBePrint), tpt)

        case TypeApply(fun, args) =>
          treeCopy.TypeApply(tree, fun, args)

        case Super(qual, mix) =>
          println("======================= Super " + tree)
          println(tree.pos)
          treeCopy.Super(tree, qual, mix)

        case This(qual) => wrapWithPrint(tree, toBePrint ::: List((tree.upName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Ident(name) =>
         wrapWithPrint(tree, toBePrint ::: List((tree.upName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Literal(value) => wrapWithPrint(tree, toBePrint ::: List((tree.name + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Select(qualifier, selector) =>
          val (qualTree, qualString) = parseFunQualifier(qualifier)
          val allDependces = if(qualString.length > 0) qualString reduce {_ + " " + _} else ""
          if(tree.symbol.isMethod)
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              toBePrint ::: List((allDependces + " " + tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))
          else
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              toBePrint ::: List((allDependces + " " + tree.upName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

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
