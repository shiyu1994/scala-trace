package scalatrace

/**
 * Created by shiyu on 16/9/14.
 */

import scala.tools.nsc.Global

trait NameTrees {
  val global: Global
  import global._

  protected abstract class NameTree {
    val name: String
    val allNames: List[String]
  }

  object ReadWriteTree {

    def getName(tree: Tree): String = tree match {

      case  PackageDef(pid, stats) => tree.symbol.name.toString

      case  ClassDef(mods, name, tparams, impl) => tree.symbol.name.toString

      case  ModuleDef(mods, name, impl) => tree.symbol.name.toString

      case  ValDef(mods, name, tpt, rhs) => name.toString

      case  DefDef(mods, name, tparams, vparamss, tpt, rhs) => name.toString

      case LabelDef(name, params, rhs) => name.toString

      case  Template(parents, self, body) => ""

      case  Block(stats, expr) => "Block" + tree.id

      case  CaseDef(pat, guard, body) => "CaseDef" + tree.id

      case  Alternative(trees) => ""

      case  Bind(name, body) => name.toString

      case Apply(fun, args) => getName(fun)

      case  UnApply(fun, args) => getName(fun)

      case ApplyDynamic(qual, args) => getName(qual)

      case Function(vparams, body) => "Function" + tree.id

      case Assign(lhs, rhs) => ""

      case AssignOrNamedArg(lhs, rhs) => ""

      case If(cond, thenp, elsep) => "If" + tree.id

      case Match(selector, cases) => "Match" + tree.id

      case Return(expr) => getName(expr)

      case Try(block, catches, finalizer) => "Try" + tree.id

      case Throw(expr) => getName(expr)

      case New(tpt) => getName(tpt)

      case ArrayValue(elemtpt, trees) => ""

      case Typed(expr, tpt) => getName(expr)

      case TypeApply(fun, args) => getName(fun)

      case Super(qual, mix) => ""

      case This(qual) => tree.symbol.name.toString

      case Ident(name) => name.toString

      case Literal(value) => "#"

      case Select(qualifier, selector) => selector.toString

      case RefTree(qualifier, selector) => ""

      case ReferenceToBoxed(idt) => ""

      case TypeTree() => ""

      case Annotated(annot, arg) => ""

      case SingletonTypeTree(ref) => ""

      case SelectFromTypeTree(qualifier, selector) => ""

      case CompoundTypeTree(templ) => ""

      case AppliedTypeTree(tpt, args) => ""

      case TypeBoundsTree(lo, hi) => ""

      case ExistentialTypeTree(tpt, whereClauses) => ""

      case Import(expr, selectors) => ""

      case TypeDef(mods, name, tparams, rhs) => ""

      case Star(elem) => ""

      case EmptyTree => ""
    }

    def getAllNames(tree: Tree): List[String] = tree match {

      case  PackageDef(pid, stats) => getName(tree) :: Nil

      case  ClassDef(mods, name, tparams, impl) => getName(tree) :: Nil

      case  ModuleDef(mods, name, impl) => getName(tree) :: Nil

      case  ValDef(mods, name, tpt, rhs) => getName(tree) :: Nil

      case  DefDef(mods, name, tparams, vparamss, tpt, rhs) => getName(tree) :: Nil

      case LabelDef(name, params, rhs) => getName(tree) :: Nil

      case  Template(parents, self, body) => Nil

      case  Block(stats, expr) => "Block" + tree.id :: Nil

      case  CaseDef(pat, guard, body) => "CaseDef" + tree.id :: Nil

      case  Alternative(trees) => Nil

      case  Bind(name, body) => getName(tree) :: Nil

      case Apply(fun, args) => getAllNames(fun)

      case  UnApply(fun, args) => getAllNames(fun)

      case ApplyDynamic(qual, args) => getAllNames(qual)

      case Function(vparams, body) => "Function" + tree.id :: Nil

      case Assign(lhs, rhs) => "" :: Nil

      case AssignOrNamedArg(lhs, rhs) => Nil

      case If(cond, thenp, elsep) => "If" + tree.id :: Nil

      case Match(selector, cases) => "Match" + tree.id :: Nil

      case Return(expr) => Nil

      case Try(block, catches, finalizer) => "Try" + tree.id :: Nil

      case Throw(expr) => Nil

      case New(tpt) => getName(tree) :: Nil

      case ArrayValue(elemtpt, trees) => Nil

      case Typed(expr, tpt) => getAllNames(expr)

      case TypeApply(fun, args) => getAllNames(fun)

      case Super(qual, mix) => Nil

      case This(qual) => getName(tree) :: Nil

      case Ident(name) => getName(tree) :: Nil

      case Literal(value) => Nil

      case Select(qualifier, selector) =>
        if(qualifier.symbol == null || qualifier.symbol.isValue)
          getAllNames(qualifier) ::: List(selector.toString)
        else selector.toString :: Nil

      case RefTree(qualifier, selector) => Nil

      case ReferenceToBoxed(idt) => Nil

      case TypeTree() => Nil

      case Annotated(annot, arg) => Nil

      case SingletonTypeTree(ref) => Nil

      case SelectFromTypeTree(qualifier, selector) => Nil

      case CompoundTypeTree(templ) => Nil

      case AppliedTypeTree(tpt, args) => Nil

      case TypeBoundsTree(lo, hi) => Nil

      case ExistentialTypeTree(tpt, whereClauses) => Nil

      case Import(expr, selectors) => Nil

      case TypeDef(mods, name, tparams, rhs) => Nil

      case Star(elem) => Nil

      case EmptyTree => Nil
    }

    implicit def treeToReadWriteTree(tree: Tree): NameTree = {
      new NameTree {
        val name = getName(tree)
        val allNames = getAllNames(tree)
      }
    }
  }
}
