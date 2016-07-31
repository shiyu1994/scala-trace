/**
 * Created by shiyu on 16/7/30.
 */

public class Logger {

    LogTree logTree;

    public Logger() {
        if(System.getenv("ICODE") != null) {
            logTree = new ICodeLogTree(System.getenv("PRODUCTS").split(" "));
        } else {
            //TODO Plugin Approach
        }
    }

    public void log(String op, String line) {
        /*if(line.contains(","))
            logTree.log(line.substring(0, line.lastIndexOf(",")), op);
        else*/
            logTree.log(line, op);
    }
}
