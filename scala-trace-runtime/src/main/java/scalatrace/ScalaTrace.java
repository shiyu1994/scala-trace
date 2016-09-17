package scalatrace;

/**
 * Created by shiyu on 16/9/11.
 */
public final class ScalaTrace {
  static public <R> R  __wrapperAfter(R x, boolean ... printAfter) { return x; }
  static public boolean log(String dataFlow, String pos)  {
    return false;
  }

  public static Logger logger = new Logger();

  /*public static void log(String op, String line) {
    logger.log(op, line);
  }*/
}

