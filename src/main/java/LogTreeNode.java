/**
 * Created by shiyu on 16/7/31.
 * @author Yu Shi
 */

import java.util.Set;
import java.util.Iterator;
import java.util.TreeSet;

abstract class LogTreeNode {
    protected String line;
    LogTreeNode(String line) {
        this.line = line;
    }
    protected Set<LogTreeNode> dependOn = new TreeSet();
    abstract public void addDependency(LogTreeNode dependency);
    abstract public void setUseful();
    protected boolean useful = false;
}

class ICodeLogTreeNode extends LogTreeNode implements Comparable<ICodeLogTreeNode> {

    public ICodeLogTreeNode methodObject = null;

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
            useful = true;
            LogTree.increaseUseful();
            Iterator<LogTreeNode> itor = dependOn.iterator();
            LogTree.write(line);
            while (itor.hasNext()) {
                LogTreeNode next = itor.next();
                if(!next.useful && !next.line.equals("NoPosition") && next.line.contains("line"))
                    next.setUseful();
            }
            if(methodObject != null && !methodObject.useful && !methodObject.line.equals("NoPosition") && methodObject.line.contains("line")) {
                methodObject.setUseful();
            }
        }
    }

    @Override public int compareTo(ICodeLogTreeNode other) {
        return line.compareTo(other.line);
    }
}
