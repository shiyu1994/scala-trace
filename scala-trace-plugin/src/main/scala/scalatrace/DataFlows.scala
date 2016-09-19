package scalatrace

/**
 * Created by shiyu on 16/9/14.
 */

import scala.collection.mutable.Set
import scala.tools.nsc.Global
import scalatrace.DataFlowExceptions.NoUpOrDownForRawName

trait DataFlows {
  val global: Global
  import global._

  protected abstract class NameTree {
    val rawName: RawName
    val upName: UpName
    val downName: DownName
    val passFuncName: RawName
  }

  class RawName(val rawName: String) {
    val up: Boolean = false
    val passFunc: Boolean = false
    override def toString = rawName
    def getName() = rawName
  }

  case class UpName(override val rawName: String) extends RawName(rawName) {
    override val up: Boolean = true
    override def getName() = rawName + "^"
  }

  case class DownName(override val rawName: String) extends RawName(rawName) {
    override val up: Boolean = false
    override def getName() = rawName + "!"
  }

  case class PassFuncName(override val rawName: String) extends RawName(rawName) {
    override val passFunc = true
    override def getName() = rawName + "@"
  }

  case class DataFlow() {
    import global._

    val froms: Set[RawName] = Set()
    val tos: Set[RawName] = Set()
    val poses: Set[Position] = Set()

    def addFrom(rawName: RawName*): DataFlow = {
      rawName foreach { froms.add(_) }
      this
    }

    def addFrom(rawName: List[RawName]): DataFlow = {
      rawName foreach(addFrom(_))
      this
    }

    def addTo(rawName: RawName*): DataFlow = {
      rawName foreach { tos.add(_) }
      this
    }

    def addTo(rawName: List[RawName]): DataFlow = {
      rawName foreach(addTo(_))
      this
    }

    def addPos(pos: Position*): DataFlow = {
      pos foreach { poses.add(_) }
      this
    }

    def addPos(rawName: List[Position]): DataFlow = {
      rawName foreach(addPos(_))
      this
    }

    override def toString = froms + " " + tos + " "
  }

  def getName(tree: Tree): String = tree match {

    case  PackageDef(pid, stats) => tree.symbol.fullName.toString

    case  ClassDef(mods, name, tparams, impl) => tree.symbol.fullName.toString

    case  ModuleDef(mods, name, impl) => tree.symbol.fullName.toString

    case  ValDef(mods, name, tpt, rhs) => tree.symbol.fullName.toString

    case  DefDef(mods, name, tparams, vparamss, tpt, rhs) => tree.symbol.fullName.toString

    case LabelDef(name, params, rhs) => tree.symbol.fullName.toString

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

    case This(qual) => tree.symbol.fullName.toString

    case Ident(name) => tree.symbol.fullName.toString

    case Literal(value) => "#"

    case Select(qualifier, selector) => {
      tree match {
        case Select(New(tpt), _) => getName(tpt)
        case _ => tree.symbol.fullName.toString
      }
    }

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
        val rawName = new RawName(getName(tree))
        val upName = UpName(rawName.rawName)
        val downName = DownName(rawName.rawName)
        val passFuncName = PassFuncName(rawName.rawName)
      }
    }
  }
}
