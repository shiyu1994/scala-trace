/**
 * Created by shiyu on 16/7/30.
 */

public final class ScalaTrace {

    public static Logger logger = new Logger();

    public static void log(String op, String line) {
        logger.log(op, line);
    }
}
