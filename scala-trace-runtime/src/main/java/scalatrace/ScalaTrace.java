package scalatrace;

/**
 * Created by shiyu on 16/9/11.
 */
public final class ScalaTrace {
    static public <R> R  __wrapperAfter(R x, boolean ... printAfter) { return x; }
    static public <R> R  __wrapperAfter(R x) { return x; }
    static public <R> R  __wrapperBefore(boolean printBefore, R x) { return x; }


    static public boolean multiLog(String ... dataFlows)  {
        for(String dataFlow : dataFlows) {
            String[] fromsTosPoses = dataFlow.split(":");
            astDataFlowGraph.log(fromsTosPoses[0].split(" "), fromsTosPoses[1].split(" "), fromsTosPoses[2].split(" "));
        }
        return false;
    }

    public static ICodeDataFlowGraph icodeLogGraph = new ICodeDataFlowGraph(System.getenv("PRODUCTS").split(" "));
    public static ASTDataFlowGraph astDataFlowGraph = new ASTDataFlowGraph(System.getenv("PRODUCTS").split(" "));

    public static void log(String op, String line) {
        icodeLogGraph.log(op, line);
    }

    static public boolean log(String froms, String tos, String poses)  {
        astDataFlowGraph.log(froms.split(" "), tos.split(" "), poses.split(" "));
        return false;
    }
}

