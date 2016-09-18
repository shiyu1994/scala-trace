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
          case ValDef(mods, name, tpt, rhs) => args = args + " " + tree.upName
          case _ => ""
        }
      }
      symbol.parentSymbols foreach { sym =>
        args = args + " " + sym.name + "^"
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
        case TypeTree() => (tree, Nil) //TODO
      }
    }

    def parsePattern(tree: Tree): List[String] = {
      tree match {

        case Alternative(trees) =>
          //Assumption: No name bindings in Alternative patterns
          Nil

        case Bind(name, body) =>
          name.toString :: parsePattern(body)

        case Ident(name) =>
          tree.name :: Nil

        case  UnApply(fun, args) =>
          (args map {parsePattern(_)}).foldLeft(List[String]()) { (names: List[String], argNames: List[String]) =>
            names ::: argNames
          }

        case Apply(fun, args) =>
          (args map {parsePattern(_)}).foldLeft(List[String]()) { (names: List[String], argNames: List[String]) =>
            names ::: argNames
          }

        case Literal(value) =>
          Nil

        case Typed(expr, args) =>
          parsePattern(expr)

        case Select(qualifier, selector) =>
          //Assumption: No name bindings in Select patterns
          Nil

        case Star(elem) =>
          //This case can only be reached by recursive calls in UnApply and Apply cases
          //Star(elem) is not a pattern itself
          //Assumption: No name bindings in Star
          Nil
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
          val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id,
            tree.upName + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ClassDef(tree, mods, name, tparams,
            treeCopy.Template(impl, impl.parents, impl.self,
              insertIntoConstructor(impl.body.map { wrapUp(_) }, passConstructorArgs, returnObj)  ))
          exit()
          newTree

//TODO ModuleDef
        case  ModuleDef(mods, name, impl) =>
          enter(tree, tree.symbol)
          val passConstructorArgs = makePrintCall("a_" + tree.symbol.name + "_" + tree.id, tree.upName + " $$$ " + getConstructorArgs(impl.body), tree.pos)
          val returnObj = makePrintCall("b_" + tree.symbol.name + "_" + tree.id, getValsAndParents(impl.body, tree.symbol) + " $$$ " + tree.name, tree.pos)
          val newTree = treeCopy.ModuleDef(tree, mods, name,
            treeCopy.Template(impl, impl.parents, impl.self, insertIntoConstructor(impl.body.map { wrapUp(_) }, passConstructorArgs, returnObj) ))
          exit()
          newTree

        case  ValDef(mods, name, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.ValDef(tree, mods, name, tpt, wrapUp(rhs, Some(tree.name)))
          exit()
          newTree

        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          enter(tree, tree.symbol)
          val allParamName = vparamss map { vparams => vparams map {_.name} } flatten
          val newTree = if(!name.toString.contains("<init>"))
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(tree.name), (tree.name + "@" + " $$$ " + allParamName , tree.pos)::Nil))))
          else tree
          exit()
          newTree


        case LabelDef(name, params, rhs) =>
          val allParamName =  params map {_.name}
          enter(tree, tree.symbol)
          val newTree = treeCopy.LabelDef(tree, name, params,
            wrapUp(rhs, Some(tree.name), (tree.name + "@" + " $$$ " + allParamName, tree.pos)::Nil))
          exit()
          newTree


        case  Block(stats, expr) =>
          treeCopy.Block(tree, makePrintTrees(toBePrint) ::: (stats map { wrapUp(_) } ),
            wrapUp(expr, Some(tree.name),  beDependentBy.map{x => (tree.downName + " $$$ " + x, tree.pos)}.toList))

        case Apply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          val allArgName = (args map {_.downName})
          val allDeps: List[String] = (allArgName ::: qualString)
          val allDependces: String = if(allDeps.length > 0) allDeps reduce {_ + " " + _} else ""
          wrapWithPrint(treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }),
            toBePrint :::
              beDependentBy.map{x => (allDependces + " " + tree.downName + " $$$ " + x, tree.pos)}.toList :::
              List((allArgName + " $$$ " + fun.name + "@", tree.pos)))

        case Function(vparams, body) =>
          enter(tree, tree.symbol)
          val allParamName = vparams map {_.name}
          val newTree = treeCopy.Function(tree, vparams,
            wrapUp(body, Some(tree.name), (tree.name + "@" + " $$$ " + allParamName, tree.pos)::Nil))
          exit()
          wrapWithPrint(newTree, toBePrint ::: List((tree.downName + " $$$ " + beDependentBy.getOrElse(""), tree.pos)))

        case Assign(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.Assign(tree, lhs,
            wrapUp(rhs, Some(lhs.name)))
          wrapWithPrint(newTree, toBePrint)
          exit()
          newTree

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, wrapWithPrint(wrapUp(cond), toBePrint),
            wrapUp(thenp, Some(tree.name), beDependentBy.map {x => (cond.upName + " " + tree.downName + " $$$ " + x, tree.pos)}.toList),
            wrapUp(elsep, Some(tree.name), beDependentBy.map {x => (cond.upName + " " + tree.downName + " $$$ " + x, tree.pos)}.toList))

        case Match(selector, cases) =>
          treeCopy.Match(tree, wrapWithPrint(wrapUp(selector), toBePrint),
            cases map { _case =>
              val patNames = parsePattern(_case.pat)

              treeCopy.CaseDef(_case, _case.pat, wrapUp(_case.guard),
                wrapUp(_case.body, Some(tree.name),
                beDependentBy.map { x => (selector.upName + " " + tree.downName + " $$$ " + x, tree.pos) }.toList :::
                List((selector.upName + " $$$ " + patNames, tree.pos))))
            })

        case Return(expr) =>
          treeCopy.Return(tree, wrapUp(expr, beDependentBy, toBePrint))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, wrapUp(block, Some(tree.name), toBePrint ::: beDependentBy.map {x => (tree.downName + " $$$ " + x, tree.pos)}.toList),
            catches map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard, wrapUp(_case.body)) },
            wrapUp(finalizer))

        case Throw(expr) =>
          treeCopy.Throw(tree, wrapUp(expr))

        case Typed(expr, tpt) =>
          treeCopy.Typed(tree, wrapUp(expr, beDependentBy, toBePrint), tpt)

        case TypeApply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          wrapWithPrint(treeCopy.TypeApply(tree, qualTree, args),
            toBePrint ::: beDependentBy.map { x => (qualString + " " + tree.name + " $$$ " + x, tree.pos) }.toList)

        case This(qual) =>
          wrapWithPrint(tree, toBePrint :::
            List((tree.upName + " $$$ " + tree.name, tree.pos)) :::
            beDependentBy.map{ x => (tree.upName + " $$$ " + x, tree.pos)}.toList)

        case Ident(name) =>
          if(tree.symbol.isMethod)
            wrapWithPrint(tree, toBePrint :::
              List((tree.downName + " $$$ " + tree.name, tree.pos)) :::
              beDependentBy.map {x => (tree.downName + " $$$ " + x, tree.pos)}.toList)
          else
            wrapWithPrint(tree, toBePrint :::
              List((tree.upName + " $$$ " + tree.name, tree.pos)) :::
              beDependentBy.map {x => (tree.upName + " $$$ " + x, tree.pos)}.toList)

        case Literal(value) => wrapWithPrint(tree, toBePrint :::
          beDependentBy.map { x => (tree.name + " $$$ " + x, tree.pos)}.toList)

        case Select(qualifier, selector) =>
          val (qualTree, qualString) = parseFunQualifier(qualifier)
          val allDependces = if(qualString.length > 0) qualString reduce {_ + " " + _} else ""
          if(tree.symbol.isMethod)
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              toBePrint :::
                List((tree.downName + " $$$ " + tree.name, tree.pos)) :::
                beDependentBy.map {x => (allDependces + " " + tree.downName + " $$$ " + x, tree.pos)}.toList)
          else
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              toBePrint :::
                List((tree.upName + " $$$ " + tree.name, tree.pos)) :::
                beDependentBy.map {x => (allDependces + " " + tree.upName + " $$$ " + x, tree.pos)}.toList)

        case Import(expr, selectors) => tree

        case TypeDef(mods, name, tparams, rhs) => tree

        case EmptyTree => tree

        case _ => tree

       /* case  Template(parents, self, body) =>
          treeCopy.Template(tree, parents, self, body map { wrapUp(_) })
*/
       /* case  CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, wrapUp(guard),
            wrapUp(body, Some(tree.name), toBePrint :::
              beDependentBy.map { x => (guard.upName + " " +  body.downName + " $$$ " + x, tree.pos) }.toList))
*/
       /* case ApplyDynamic(qual, args) =>
          println("==================== ApplyDynamic")
          println(tree.pos)
          println(tree)
          val (qualTree, qualString) = parseFun(qual)
          val allDeps = ((args map {_.name}) ::: qualString)
          val allDependces = if(allDeps.length > 0) allDeps reduce {_ + " " + _} else ""
          wrapWithPrint(treeCopy.ApplyDynamic(tree, qualTree, args map { wrapUp(_) }),
            toBePrint ::: beDependentBy.map{x => (allDependces + " " + tree.downName + " $$$ " + x, tree.pos)}.toList)*/


     /*   case AssignOrNamedArg(lhs, rhs) =>
          println("==================== AssignOrNamedArg")
          println(tree.pos)
          println(tree)
          enter(tree)
          val newTree = treeCopy.AssignOrNamedArg(tree, lhs,
            wrapUp(rhs, Some(lhs.name)))
          wrapWithPrint(newTree, toBePrint)
          exit()
          newTree*/

 /*       case New(tpt) =>
          println("================ NEW")
          treeCopy.New(tree, wrapUp(tpt))

          //TODO
        case ArrayValue(elemtpt, trees) =>
          println("======================= ArrayValue " + tree)
          println(tree.pos)
          treeCopy.ArrayValue(tree, elemtpt, trees map { wrapUp(_) })
*/

   /*     case Super(qual, mix) =>
          println("======================= Super " + tree)
          println(tree.pos)
          treeCopy.Super(tree, qual, mix)
*/

/*        case RefTree(qualifier, selector) =>
          println("======================= RefTree " + tree)
          println(tree.pos)
          tree

        case ReferenceToBoxed(idt) =>
          println("======================= ReferenceToBoxed " + tree)
          println(tree.pos)
          tree
*/
 /*       case TypeTree() => {
          println("========================= TypeTree")
          tree
        }
*/
  /*      case Annotated(annot, arg) =>
          println("======================= Annotated " + tree)
          println(tree.pos)
          tree
*/
  /*      case SingletonTypeTree(ref) =>
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
*/

   /*     case Star(elem) =>
          println("======================= Star " + tree)
          println(tree.pos)
          tree
*/
      }
    }
  }
}
