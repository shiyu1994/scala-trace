package scalatrace;

import java.util.*;

class ICodeDataFlowGraphNode {

    protected String line;
    protected Set<ICodeDataFlowGraphNode> dependOn = new HashSet();
    protected boolean useful = false;
    public ICodeDataFlowGraphNode methodObject = null;

    ICodeDataFlowGraphNode(String line) {
        this.line = line;
    }

    public void addDependency(ICodeDataFlowGraphNode dependency) {
        dependOn.add(dependency);
        if(!dependency.useful && !line.equals("NoPosition") && line.contains("line") &&
                useful && !dependency.line.equals("NoPosition") && dependency.line.contains("line"))
            dependency.setUseful();
    }

    public void setUseful() {
        if(!useful && !line.equals("NoPosition") && line.contains("line")) {

            Queue<ICodeDataFlowGraphNode> queue = new LinkedList<>();
            queue.add(this);

            while(!queue.isEmpty()) {
                ICodeDataFlowGraphNode next = queue.remove();
                if(!next.useful && !next.line.equals("NoPosition") && next.line.contains("line")) {
                    next.useful = true;
                    ICodeDataFlowGraph.increaseUseful();
                    ICodeDataFlowGraph.write(next.line);

                    if(next.methodObject != null && !next.methodObject.useful && !next.methodObject.line.equals("NoPosition") && next.methodObject.line.contains("line")) {
                        queue.add(next.methodObject);
                    }

                    Iterator<ICodeDataFlowGraphNode> itor = next.dependOn.iterator();
                    while(itor.hasNext()) {
                        ICodeDataFlowGraphNode nextNode = itor.next();
                        if(!nextNode.useful && !nextNode.line.equals("NoPosition") && nextNode.line.contains("line")) {
                            queue.add(nextNode);
                        }
                    }
                }
            }
        }
    }
}
