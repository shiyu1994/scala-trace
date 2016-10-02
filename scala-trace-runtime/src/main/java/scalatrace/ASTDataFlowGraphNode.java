package scalatrace;

import java.io.IOException;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * Created by shiyu on 16/9/19.
 */

public class ASTDataFlowGraphNode {
    String line;
    Set<ASTDataFlowGraphNode> dependOn = new HashSet<>();

    boolean useful;
    ASTDataFlowGraphNode(String line) {
        this.line = line;
        useful = false;
    }
    void addDependOn(ASTDataFlowGraphNode node) {
        dependOn.add(node);
        if(useful && !node.useful) {
            node.setUseful();
        }
    }
    void setUseful() {
        if(!useful) {
            useful = true;
            ++ASTDataFlowGraph.usefulLines;

            try {
                ASTDataFlowGraph.usefulLinesFile.write(line + "\n");
                ASTDataFlowGraph.usefulLinesFile.flush();
                ASTDataFlowGraph.logFile.write(ASTDataFlowGraph.usefulLines + "/" + ASTDataFlowGraph.totalLines + "\n");
                ASTDataFlowGraph.logFile.flush();
            } catch (IOException e) {
                e.printStackTrace();
            }

            Queue<ASTDataFlowGraphNode> setUsefulQueue = new LinkedList<>();
            setUsefulQueue.add(this);

            while(!setUsefulQueue.isEmpty()) {
                ASTDataFlowGraphNode next = setUsefulQueue.remove();
                if(next.useful) {
                    for(ASTDataFlowGraphNode node : next.dependOn) {
                        if(!node.useful) {
                            node.useful = true;
                            ++ASTDataFlowGraph.usefulLines;
                            try {
                                ASTDataFlowGraph.usefulLinesFile.write(node.line + "\n");
                                ASTDataFlowGraph.usefulLinesFile.flush();
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                            setUsefulQueue.add(node);
                        }
                    }
                }
            }
        }
    }
}
