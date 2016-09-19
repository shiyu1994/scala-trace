package scalatrace

/**
 * Created by shiyu on 16/9/11.
 */

import scala.tools.nsc.transform.TypingTransformers

trait WrapUpTypingTransformers extends TypingTransformers with FunContexts {

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

    def getConstructorArgs(implBody: List[Tree]): List[RawName] = {
      var args: List[RawName] = Nil
      for(tree <- implBody) {
        tree match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if(tree.symbol.isConstructor) =>
            vparamss foreach {vparams => vparams foreach {vparam => args = (args ::: List(vparam.rawName))}}
          case _ =>
        }
      }
      return args
    }

    def getValsAndParents(implBody: List[Tree], symbol: Symbol): List[RawName] = {
      var args: List[RawName] = Nil
      implBody foreach { tree =>
        tree match {
          case ValDef(mods, name, tpt, rhs) => args = args ::: List(tree.upName)
          case _ =>
        }
      }
      symbol.parentSymbols filter { parentSymbol => parentSymbol.name.toString != "Object" &&
        parentSymbol.name.toString != "AnyRef" &&
        parentSymbol.name.toString != "AnyVal" &&
        parentSymbol.name.toString != "Any"} foreach { sym =>
        args = args ::: List(UpName(sym.fullName.toString))
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

    def parseFunQualifier(tree: Tree): (Tree, List[RawName]) = {
      tree match {
        case Select(qualifier, selector) =>
          val (qualTree, qualNames) = parseFunQualifier(qualifier)
          if(tree.symbol.isMethod) {
            (treeCopy.Select(tree, qualTree, selector), tree.downName :: qualNames)
          } else {
            (treeCopy.Select(tree, qualTree, selector), tree.upName :: qualNames)
          }
        case Apply(fun, args) =>
          val (qualTree, qualNames) = parseFun(fun)
          (treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }), fun.downName::qualNames)
        case Block(stats, expr) =>
          (treeCopy.Block(tree, stats map { wrapUp(_) }, wrapUp(expr, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)))), tree.downName :: Nil)
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
          val (qualTree, qualNames) = parseFunQualifier(expr)
          (treeCopy.Typed(tree, qualTree, tpt), qualNames)
        case TypeApply(fun, args) =>
          val (qualTree, qualNames) = parseFun(fun)
          (treeCopy.TypeApply(tree, qualTree, args), fun.downName :: qualNames)
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

    def parseFun(tree: Tree): (Tree, List[RawName]) = {
      tree match {
        case Select(qualifier, selector) =>
          val (qualTree, qualNames) = parseFunQualifier(qualifier)
          (treeCopy.Select(tree, qualTree, selector), qualNames)
        case TypeApply(fun, args) =>
          val (qualTree, qualNames) = parseFun(fun)
          (treeCopy.TypeApply(tree, qualTree, args), qualNames)
        case Apply(fun, args) =>
          val (qualTree, qualNames) = parseFun(fun)
          (treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }), qualNames)
        case Ident(name) => (tree, Nil)
        case New(tpt) => (tree, Nil)
        case This(qual) => (tree, Nil)
        case TypeTree() => (tree, Nil) //TODO
      }
    }

    def parsePattern(tree: Tree): List[RawName] = {
      tree match {

        case Alternative(trees) =>
          //Assumption: No name bindings in Alternative patterns
          Nil

        case Bind(name, body) =>
          tree.rawName :: parsePattern(body)

        case Ident(name) =>
          tree.rawName :: Nil

        case  UnApply(fun, args) =>
          (args map {parsePattern(_)}).foldLeft(List[RawName]()) { (names: List[RawName], argNames: List[RawName]) =>
            names ::: argNames
          }

        case Apply(fun, args) =>
          (args map {parsePattern(_)}).foldLeft(List[RawName]()) { (names: List[RawName], argNames: List[RawName]) =>
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

    def wrapUp(tree: Tree, toDataFlow: Option[DataFlow] = None, dataFlows: List[DataFlow] = Nil): Tree = {

      tree match {

        case  PackageDef(pid, stats) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.PackageDef(tree, pid, stats.map { wrapUp(_) } )
          exit()
          newTree

//TODO ClassDef
        case  ClassDef(mods, name, tparams, impl) =>
          enter(tree, tree.symbol)
          val passConstructorArgs = makeLogTree(DataFlow().addFrom(tree.passFuncName).addTo(getConstructorArgs(impl.body)).addPos(tree.pos))
          val returnObj = makeLogTree(DataFlow().addFrom(getValsAndParents(impl.body, tree.symbol)).addTo(tree.rawName).addPos(tree.pos))
          val newTree = treeCopy.ClassDef(tree, mods, name, tparams,
            treeCopy.Template(impl, impl.parents, impl.self,
              insertIntoConstructor(impl.body.map { wrapUp(_) }, passConstructorArgs, returnObj)  ))
          exit()
          newTree

//TODO ModuleDef
        case  ModuleDef(mods, name, impl) =>
          enter(tree, tree.symbol)
          val passConstructorArgs = makeLogTree(DataFlow().addFrom(tree.passFuncName).addTo(getConstructorArgs(impl.body)).addPos(tree.pos))
          val returnObj = makeLogTree(DataFlow().addFrom(getValsAndParents(impl.body, tree.symbol)).addTo(tree.rawName).addPos(tree.pos))
          val newTree = treeCopy.ModuleDef(tree, mods, name,
            treeCopy.Template(impl, impl.parents, impl.self, insertIntoConstructor(impl.body.map { wrapUp(_) }, passConstructorArgs, returnObj) ))
          exit()
          newTree

        case  ValDef(mods, name, tpt, rhs) =>
          enter(tree, tree.symbol)
          val newTree = treeCopy.ValDef(tree, mods, name, tpt, wrapUp(rhs, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos))))
          exit()
          newTree

        case  DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          enter(tree, tree.symbol)
          val vparamNames = vparamss map { vparams => vparams map {_.rawName} } flatten
          val dataFlow = DataFlow().addFrom(tree.passFuncName).addTo(vparamNames).addPos(tree.pos)
          val newTree =
            if(!name.toString.contains("<init>") && !tree.symbol.isLazy)
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)), dataFlow :: Nil))))
            else if(tree.symbol.isLazy) {
              lazyBuffer = lazyBuffer ::: List(dataFlow)
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos))))))
            }
            else tree
          exit()
          newTree


        case LabelDef(name, params, rhs) =>
          val paramNames =  params map {_.rawName}
          enter(tree, tree.symbol)
          val newTree = treeCopy.LabelDef(tree, name, params, wrapUp(rhs, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
            DataFlow().addFrom(tree.passFuncName).addTo(paramNames).addPos(tree.pos) :: Nil))
          exit()
          newTree


        case  Block(stats, expr) =>
          treeCopy.Block(tree, (dataFlows map {makeLogTree(_)}) ::: (stats map { wrapUp(_) } ),
            wrapUp(expr, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
              toDataFlow.map{x => x.addFrom(tree.downName)}.toList))

        case Apply(fun, args) =>
          val (qualTree, qualString) = parseFun(fun)
          val argNames = (args map {_.downName})
          val allDeps: List[RawName] = (argNames ::: qualString)
          wrapWithPrint(treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }),
            dataFlows ::: toDataFlow.map {x => x.addFrom(allDeps)}.toList :::
              List(DataFlow().addFrom(argNames).addTo(tree.passFuncName).addPos(tree.pos)))

        case Function(vparams, body) =>
          enter(tree, tree.symbol)
          val paramNames = vparams map {_.rawName}
          val newTree =
            treeCopy.Function(tree, vparams,
              wrapUp(body, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
                DataFlow().addFrom(tree.passFuncName).addTo(paramNames).addPos(tree.pos) :: Nil))
          exit()
          wrapWithPrint(newTree, dataFlows ::: toDataFlow.map {x => x.addFrom(tree.downName)}.toList)

        case Assign(lhs, rhs) =>
          enter(tree)
          val newTree = treeCopy.Assign(tree, lhs, wrapUp(rhs, Some(DataFlow().addTo(lhs.rawName).addPos(lhs.pos))))
          wrapWithPrint(newTree, dataFlows)
          exit()
          newTree

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, wrapWithPrint(wrapUp(cond), dataFlows),
            wrapUp(thenp, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
              toDataFlow.map {x => x.addFrom(cond.upName, tree.downName)}.toList),
            wrapUp(elsep, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
              toDataFlow.map {x => x.addFrom(cond.upName, tree.downName)}.toList))

        case Match(selector, cases) =>
          treeCopy.Match(tree, wrapWithPrint(wrapUp(selector), dataFlows),
            cases map { _case =>
              val patNames = parsePattern(_case.pat)
              treeCopy.CaseDef(_case, _case.pat, wrapUp(_case.guard),
                wrapUp(_case.body, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
                toDataFlow.map { x => x.addFrom(selector.upName, tree.downName) }.toList :::
                  List(DataFlow().addFrom(selector.upName).addTo(patNames).addPos(_case.pat.pos))))
            })

        case Return(expr) =>
          treeCopy.Return(tree, wrapUp(expr, toDataFlow, dataFlows))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, wrapUp(block, Some(DataFlow().addTo(tree.rawName).addPos(tree.pos)),
            dataFlows ::: toDataFlow.map {x => x.addFrom(tree.downName)}.toList),
            catches map { _case => treeCopy.CaseDef(_case, _case.pat, _case.guard, wrapUp(_case.body)) },
            wrapUp(finalizer))

        case Throw(expr) =>
          treeCopy.Throw(tree, wrapUp(expr))

        case Typed(expr, tpt) =>
          treeCopy.Typed(tree, wrapUp(expr, toDataFlow, dataFlows), tpt)

        case TypeApply(fun, args) =>
          val (qualTree, qualNames) = parseFun(fun)
          wrapWithPrint(treeCopy.TypeApply(tree, qualTree, args),
            dataFlows ::: toDataFlow.map { x => x.addFrom(tree.downName :: qualNames) }.toList)

        case This(qual) =>
          wrapWithPrint(tree, dataFlows :::
            List(DataFlow().addFrom(tree.upName).addTo(tree.rawName).addPos(tree.pos)) :::
            toDataFlow.map{ x => x.addFrom(tree.upName)}.toList)

        case Ident(name) =>
          if(tree.symbol.isMethod)
            wrapWithPrint(tree, dataFlows :::
              List(DataFlow().addFrom(tree.downName).addTo(tree.rawName).addPos(tree.pos)) :::
              toDataFlow.map {x => x.addFrom(tree.downName)}.toList)
          else
            wrapWithPrint(tree, dataFlows :::
              List(DataFlow().addFrom(tree.upName).addTo(tree.rawName).addPos(tree.pos)) :::
              toDataFlow.map {x => x.addFrom(tree.upName)}.toList)

        case Literal(value) => wrapWithPrint(tree, dataFlows ::: toDataFlow.map { x => x.addFrom(tree.rawName)}.toList)

        case Select(qualifier, selector) =>
          val (qualTree, qualNames) = parseFunQualifier(qualifier)
          if(tree.symbol.isMethod)
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              dataFlows :::
                List(DataFlow().addFrom(tree.downName).addTo(tree.rawName).addPos(tree.pos)) :::
                toDataFlow.map {x => x.addFrom(tree.downName :: qualNames)}.toList)
          else
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              dataFlows :::
                List(DataFlow().addFrom(tree.upName).addTo(tree.rawName).addPos(tree.pos)) :::
                toDataFlow.map {x => x.addFrom(tree.upName :: qualNames)}.toList)

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
