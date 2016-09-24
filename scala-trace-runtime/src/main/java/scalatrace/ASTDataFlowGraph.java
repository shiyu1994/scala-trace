package scalatrace;

import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * Created by shiyu on 16/9/19.
 */
public class ASTDataFlowGraph {

    private HashMap<String, ASTDataFlowGraphNode> lineMapToNode = new HashMap<>();
    private HashSet<String> products = new HashSet<>();
    private HashMap<String, Set<ASTDataFlowGraphNode>> nameLastWrittenBy = new HashMap<>();
    private HashMap<String, Set<ASTDataFlowGraphNode>> backpatchMap = new HashMap<>();

    static public int totalLines = 0;
    static public int usefulLines = 0;
    static FileWriter totoalLinesFile, usefulLinesFile, logFile;
    static public String logFilePrefix = System.getenv("LOGFILENAME");

    ASTDataFlowGraph(String[] products) {
        try {
            totoalLinesFile =  new FileWriter(logFilePrefix + "-" + "all-lines");
            usefulLinesFile = new FileWriter(logFilePrefix + "-" + "useful-lines");
            logFile = new FileWriter(logFilePrefix + "-ratio");
        } catch (IOException e) {
            e.printStackTrace();
        }
        for(String product : products)
            this.products.add(product);
    }

    void log(String[] froms, String[] tos, String[] poses) {
        Set<ASTDataFlowGraphNode> toNodes = new HashSet<>();
        for(String pos : poses) {
            if(lineMapToNode.containsKey(pos)) {
                toNodes.add(lineMapToNode.get(pos));
            } else {
                ++totalLines;
                try {
                    totoalLinesFile.write(pos + "\n");
                    totoalLinesFile.flush();
                } catch (IOException e) {
                    e.printStackTrace();
                }
                ASTDataFlowGraphNode newNode = new ASTDataFlowGraphNode(pos);
                lineMapToNode.put(pos, newNode);
                toNodes.add(newNode);
            }
        }

        Set<ASTDataFlowGraphNode> fromNodes = new HashSet<>();
        for(String from : froms) {
            String rawName = extractRawName(from);
            if((from.endsWith("^") || from.endsWith("@")) && nameLastWrittenBy.containsKey(rawName)) {
                for(ASTDataFlowGraphNode node : nameLastWrittenBy.get(rawName))
                    fromNodes.add(node);
            } else {
                if(backpatchMap.containsKey(rawName)) {
                    for(ASTDataFlowGraphNode toNode : toNodes)
                        backpatchMap.get(rawName).add(toNode);
                } else {
                    HashSet<ASTDataFlowGraphNode> newBackpatch = new HashSet<>();
                    for(ASTDataFlowGraphNode toNode : toNodes) {
                        newBackpatch.add(toNode);
                    }
                    backpatchMap.put(rawName, newBackpatch);
                }
            }
        }

        for(ASTDataFlowGraphNode fromNode : fromNodes) {
            for(ASTDataFlowGraphNode toNode : toNodes) {
                toNode.addDependOn(fromNode);
            }
        }

        for(String toName : tos) {
            Set<ASTDataFlowGraphNode> toNameLastWrittenBy = new HashSet<>();
            for(ASTDataFlowGraphNode toNode : toNodes) {
                toNameLastWrittenBy.add(toNode);
            }
            if(!toName.endsWith("@"))
                nameLastWrittenBy.put(toName, toNameLastWrittenBy);
            if(backpatchMap.containsKey(toName)) {
                for(ASTDataFlowGraphNode toNode : toNodes) {
                    for(ASTDataFlowGraphNode toBeBackpatched : backpatchMap.get(toName)) {
                        toBeBackpatched.addDependOn(toNode);
                    }
                }
                backpatchMap.remove(toName);
            }
            if(products.contains(toName)) {
                System.out.println(toName);
                for(String pos : poses) {
                    System.out.println(pos);
                }
                for(ASTDataFlowGraphNode toNode : toNodes) {
                    toNode.setUseful();
                }
            }
        }
    }

    String extractRawName(String name) {
        if(name.endsWith("^") || name.endsWith("!"))
        return name.substring(0, name.length() - 1);
        else return name;
    }
}
