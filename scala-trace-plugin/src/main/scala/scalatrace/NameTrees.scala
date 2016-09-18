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
    val upName: String
    val downName: String
  }

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

  object ReadWriteTree {
    implicit def treeToReadWriteTree(tree: Tree): NameTree = {
      new NameTree {
        val name = getName(tree)
        val upName = name + "^"
        val downName = name + "!"
      }
    }
  }
}
