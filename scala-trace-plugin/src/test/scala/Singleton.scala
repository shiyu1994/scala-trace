import scala.annotation.tailrec

/**
 * Created by shiyu on 16/9/17.
 */
object Singleton {


  val f = {println(); { x: Int => x match { case 1 => 1 } } }
  def gg(): Int = 1
  @tailrec
  def kk: Int = {
    f(1)
    gg()
    10 match {
      case 0 => 1
      case _ => kk
    }
  }
}