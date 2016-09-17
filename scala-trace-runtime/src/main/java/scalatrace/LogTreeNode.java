package scalatrace; /**
 * Created by shiyu on 16/7/31.
 * @author Yu Shi
 */

import java.util.*;

abstract class LogTreeNode {
    protected String line;
    LogTreeNode(String line) {
        this.line = line;
    }
    protected Set<LogTreeNode> dependOn = new HashSet();
    abstract public void addDependency(LogTreeNode dependency);
    abstract public void setUseful();
    protected boolean useful = false;
    public ICodeLogTreeNode methodObject = null;
}

class ICodeLogTreeNode extends LogTreeNode {

    ICodeLogTreeNode(String line) {
        super(line);
    }

    @Override public void addDependency(LogTreeNode dependency) {
        dependOn.add(dependency);
        if(!dependency.useful && !line.equals("NoPosition") && line.contains("line") &&
               useful && !dependency.line.equals("NoPosition") && dependency.line.contains("line"))
            dependency.setUseful();
    }

    @Override public void setUseful() {
        if(!useful && !line.equals("NoPosition") && line.contains("line")) {

            Queue<LogTreeNode> queue = new LinkedList<>();
            queue.add(this);

            while(!queue.isEmpty()) {
                LogTreeNode next = queue.remove();
                if(!next.useful && !next.line.equals("NoPosition") && next.line.contains("line")) {
                    next.useful = true;
                    LogTree.increaseUseful();
                    LogTree.write(next.line);

                    if(next.methodObject != null && !next.methodObject.useful && !next.methodObject.line.equals("NoPosition") && next.methodObject.line.contains("line")) {
                        queue.add(next.methodObject);
                    }

                    Iterator<LogTreeNode> itor = next.dependOn.iterator();
                    while(itor.hasNext()) {
                        LogTreeNode nextNode = itor.next();
                        if(!nextNode.useful && !nextNode.line.equals("NoPosition") && nextNode.line.contains("line")) {
                            queue.add(nextNode);
                        }
                    }
                }
            }

            /*useful = true;
            scalatrace.LogTree.increaseUseful();
            Iterator<scalatrace.LogTreeNode> itor = dependOn.iterator();
            scalatrace.LogTree.write(line);
            while (itor.hasNext()) {
                scalatrace.LogTreeNode next = itor.next();
                if(!next.useful && !next.line.equals("NoPosition") && next.line.contains("line"))
                    next.setUseful();
            }
            if(methodObject != null && !methodObject.useful && !methodObject.line.equals("NoPosition") && methodObject.line.contains("line")) {
                methodObject.setUseful();
            }*/
        }
    }

    /*@Override public int compareTo(scalatrace.ICodeLogTreeNode other) {
        return line.compareTo(other.line);
    }*/
}
