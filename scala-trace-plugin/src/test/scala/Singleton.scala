import scala.annotation.tailrec

/**
 * Created by shiyu on 16/9/17.
 */
object Singleton {

  @tailrec
  def kk: Int = {
    10 match {
      case 0 => 1
      case _ => kk
    }
  }
}
