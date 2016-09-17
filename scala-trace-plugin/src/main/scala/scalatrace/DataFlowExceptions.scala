package scalatrace

import scala.reflect.api.Position

/**
 * Created by shiyu on 16/9/12.
 */
object DataFlowExceptions {
  class PrintAtNonReturnApply(pos: Position) extends Exception(pos.focus.toString)
  class WrongContextTreeType(pos: Position, tree: String) extends Exception(s"Unexpected tree in function contexts: ${tree}\nat ${pos}")
}
