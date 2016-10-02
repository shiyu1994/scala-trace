package scalatrace

/**
 * Created by shiyu on 16/9/11.
 */

import scala.tools.nsc.transform.TypingTransformers
import DataFlowExceptions._

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

    def getNameAsArg(tree: Tree): RawName = tree match {

      case  ValDef(mods, name, tpt, rhs) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  PackageDef(pid, stats) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  ClassDef(mods, name, tparams, impl) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  ModuleDef(mods, name, impl) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  DefDef(mods, name, tparams, vparamss, tpt, rhs) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case LabelDef(name, params, rhs) => tree.downName

      case  Template(parents, self, body) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  Block(stats, expr) => tree.downName

      case  CaseDef(pat, guard, body) => tree.downName

      case  Alternative(trees) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case  Bind(name, body) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case Apply(fun, args) => tree.downName

      case  UnApply(fun, args) => tree.downName

      case ApplyDynamic(qual, args) => tree.downName

      case Function(vparams, body) => tree.downName

      case Assign(lhs, rhs) => lhs.downName

      case AssignOrNamedArg(lhs, rhs) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case If(cond, thenp, elsep) => tree.downName

      case Match(selector, cases) => tree.downName

      case Return(expr) => getNameAsArg(expr)

      case Try(block, catches, finalizer) => tree.downName

      case Throw(expr) => getNameAsArg(expr)

      case New(tpt) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case ArrayValue(elemtpt, trees) => tree.downName

      case Typed(expr, tpt) => getNameAsArg(expr)

      case TypeApply(fun, args) => tree.upName

      case Super(qual, mix) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case This(qual) => tree.upName

      case Ident(name) => if(tree.symbol.isMethod) tree.downName else tree.upName

      case Literal(value) => tree.upName

      case Select(qualifier, selector) => if(tree.symbol.isMethod) tree.downName else tree.upName

      case RefTree(qualifier, selector) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case ReferenceToBoxed(idt) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case TypeTree() => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case Annotated(annot, arg) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case SingletonTypeTree(ref) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case SelectFromTypeTree(qualifier, selector) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case CompoundTypeTree(templ) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case AppliedTypeTree(tpt, args) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case TypeBoundsTree(lo, hi) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case ExistentialTypeTree(tpt, whereClauses) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case Import(expr, selectors) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case TypeDef(mods, name, tparams, rhs) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case Star(elem) => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)

      case EmptyTree => throw new CannotBeArgument(tree.toString() + " " + tree.pos + " " + tree.getClass)
    }

    def getValsAndParents(implBody: List[Tree], symbol: Symbol): List[RawName] = {
      var args: List[RawName] = Nil
      implBody foreach { tree =>
        tree match {
          case ValDef(mods, name, tpt, rhs) => args = args ::: List(tree.upName)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if(tree.symbol.isConstructor) =>
            rhs match {
              case Block(stats, expr) => stats foreach { stat =>
                if(treeInfo.isSelfOrSuperConstrCall(stat) && !(stat.rawName.toString.contains(".Object.") ||
                  stat.rawName.toString.contains(".AnyRef.") ||
                  stat.rawName.toString.contains(".AnyVal.") ||
                  stat.rawName.toString.contains(".Any."))) {
                  args = args ::: List(stat.upName)
                }
              }
            }
          case _ =>
        }
      }
      return args
    }

    def insertIntoConstructor(implBody: List[Tree], passArgs: Tree, callsInConstructor: List[Tree], returnObj: Tree): List[Tree] = {
      implBody map {  tree => tree match {
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) if (tree.symbol.isConstructor)  => {
            val newRhs = rhs match {
              case Block(stats, expr) =>
               treeCopy.Block(rhs, stats ::: (passArgs :: callsInConstructor), returnObj)
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

        case  ClassDef(mods, name, tparams, impl) =>
          enter(tree, tree.symbol)
          enterClass(getValsAndParents(impl.body, tree.symbol))
          val newTree = treeCopy.ClassDef(tree, mods, name, tparams,
            treeCopy.Template(impl, impl.parents, impl.self, impl.body.map { wrapUp(_) }))
          exitClass()
          exit()
          newTree

        case  ModuleDef(mods, name, impl) =>
          enter(tree, tree.symbol)
          enterClass(getValsAndParents(impl.body, tree.symbol))
          val newTree = treeCopy.ModuleDef(tree, mods, name,
            treeCopy.Template(impl, impl.parents, impl.self, impl.body.map { wrapUp(_) }))
          exitClass()
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
          val _toDataFlow = if(tree.symbol.isConstructor) {
            DataFlow().addTo(tree.rawName).addPos(tree.pos).addFrom(superClassesAndMembers)
          } else {
            DataFlow().addTo(tree.rawName).addPos(tree.pos)
          }
          val newTree =
            if(!tree.symbol.isConstructor && !tree.symbol.isLazy)
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(_toDataFlow), dataFlow :: Nil))))
            else if(tree.symbol.isLazy) {
              lazyBuffer = lazyBuffer ::: List(dataFlow)
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                (flattenBlock(wrapUp(rhs, Some(_toDataFlow)))))
            } else {
              constructorBuffer = constructorBuffer ::: List(dataFlow)
              treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, (flattenBlock(wrapUp(rhs, Some(_toDataFlow)))))
            }
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
          val (qualTree, qualNames) = parseFun(fun)
          val argNames = (args map {getNameAsArg(_)})
          val allDeps: List[RawName] = (argNames ::: qualNames)

          if(treeInfo.isSelfOrSuperConstrCall(tree)) {
            constructorBuffer = constructorBuffer ::: dataFlows ::: toDataFlow.map {x => x.addFrom(tree.downName :: allDeps)}.toList :::
              List(DataFlow().addFrom(allDeps).addTo(tree.passFuncName).addPos(tree.pos))
            wrapWithPrint(treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }), Nil)
          } else {
            wrapWithPrint(treeCopy.Apply(tree, qualTree, args map { wrapUp(_) }),
              dataFlows ::: toDataFlow.map {x => x.addFrom(tree.downName :: allDeps)}.toList :::
                List(DataFlow().addFrom(allDeps).addTo(tree.passFuncName).addPos(tree.pos)))
          }

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
            wrapWithPrint(tree, dataFlows ::: toDataFlow.map {x => x.addFrom(tree.upName)}.toList)

        case Literal(value) => wrapWithPrint(tree, dataFlows ::: toDataFlow.map { x => x.addFrom(tree.rawName)}.toList)

        case Select(qualifier, selector) =>
          val (qualTree, qualNames) = parseFunQualifier(qualifier)
          if(tree.symbol.isMethod)
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              dataFlows :::
                List(DataFlow().addFrom(tree.downName :: qualNames).addTo(tree.rawName).addPos(tree.pos)) :::
                toDataFlow.map {x => x.addFrom(tree.downName :: qualNames)}.toList)
          else
            wrapWithPrint(treeCopy.Select(tree, qualTree, selector),
              dataFlows ::: toDataFlow.map {x => x.addFrom(tree.upName :: qualNames)}.toList)

        case Import(expr, selectors) => tree

        case TypeDef(mods, name, tparams, rhs) => tree

        case EmptyTree => tree

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
