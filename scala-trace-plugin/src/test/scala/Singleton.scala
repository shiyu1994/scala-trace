
/**
 * Created by shiyu on 16/9/17.
 */

class Single(val c: Int) {
  val a = println("I'm Single")
}

trait Double {
  val b = 7777
}

object Singleton extends Single(1234) with Double {
  val f = {println(); { x: Int => x match { case 1 => 1 } } }
  def gg(): Int = 1

  def kk: Int = {
    f(1)
    gg()

    (if(2 > 1) 1 else {println; 2}) match {
      case 1 => 2
    }
  }
  override val a = println("hahahahahah")
  def main(args: Array[String]): Unit = {
    new Single(1)
    println(kk)
  }
}


