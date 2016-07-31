/**
 * Created by shiyu on 16/7/30.
 * @author Yu Shi
 */

import java.io.IOException;
import java.util.Stack;
import java.util.HashMap;
import java.io.FileWriter;
import java.util.Set;
import java.util.TreeSet;

abstract class LogTree<T> {

    protected Set<String>  products = new TreeSet();
    static private FileWriter usefulLines;
    static private FileWriter usefulRate;

    LogTree(String[] products) {
        for(int i = 0; i < products.length; ++i) {
            System.out.println(products[i]);
            this.products.add(products[i]);
        }
        try {
            usefulLines = new FileWriter(System.getenv("LOGFILENAME"));
            usefulRate = new FileWriter(System.getenv("LOGFILENAME") + "-rate");

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    abstract public void log(String line, String op);

    static private Integer counter = 0;
    static private Integer useful = 0;
    static public void write(String line)  {
        try {
            usefulLines.write(line + "\n");
            usefulLines.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    static protected void increase() {
        counter += 1;
    }
    static protected void increaseUseful() {
        useful += 1;
        try {
            usefulRate.write(useful + "/" + counter + "\n");
            usefulRate.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}


class ICodeLogTree extends LogTree<ICodeLogTreeNode> {
        ICodeLogTree(String[] products) {
            super(products);
            lineMapToNode.put("NoPosition", noPosition);
            thisStack.push(noPosition);
            staticStack.push(true);
        }

        ICodeLogTreeNode noPosition = new ICodeLogTreeNode("NoPosition");
        private HashMap<String, ICodeLogTreeNode> nameLastWrittenBy = new HashMap();
        private HashMap<String, ICodeLogTreeNode> lineMapToNode = new HashMap();
        private Stack<String> traceStack = new Stack();
        private Stack<ICodeLogTreeNode> nodeStack = new Stack();
        private Stack<Integer> methodCounterStack = new Stack();
        private Integer inPrimitive = 0;
        boolean tryConstructModule = false;
        boolean readingArgs = false;
        ICodeLogTreeNode thisWrittenBy = null;
        Integer methodCounter = 0;
        ICodeLogTreeNode thisLineNode, dependOnLine;
        Stack<ICodeLogTreeNode> thisStack = new Stack();
        Stack<Boolean> staticStack = new Stack();
        private HashMap<String, String[]> opPool = new HashMap();

        @Override
        public void log(String line, String op) {
            if(!lineMapToNode.containsKey(line)) {
                LogTree.increase();
                ICodeLogTreeNode newNode = new ICodeLogTreeNode(line);
                lineMapToNode.put(line, newNode);
                thisLineNode = newNode;
                if(!staticStack.peek())
                    thisLineNode.methodObject = thisWrittenBy;
            } else {
                thisLineNode = lineMapToNode.get(line);
            }

            String[] opArgs;
            if(opPool.containsKey(op))
                opArgs = opPool.get(op);
            else {
                opArgs = op.split(" ");
                opPool.put(op, opArgs);
            }

            if(inPrimitive == 0 && tryConstructModule) {
                if(!opArgs[0].equals("##02")) {
                    traceStack.pop();
                    nodeStack.pop();
                    methodCounter -= 1;
                }
                tryConstructModule = false;
            }

            switch (opArgs[0])  {
                case "#1":
                    if(inPrimitive == 0) {
                        methodCounter -= 1;
                        if (op.equals("#1 @this @")) {
                            readingArgs = true;
                            thisWrittenBy = thisLineNode;
                        } else {
                            nameLastWrittenBy.put(opArgs[1], thisLineNode);
                            traceStack.pop();
                            dependOnLine = nodeStack.pop();
                            thisLineNode.addDependency(dependOnLine);
                            if(op.equals("#1 @this")) {
                                thisStack.pop();
                                thisStack.push(thisLineNode);
                            }
                        }

                        if (products.contains(opArgs[1])) {
                            thisLineNode.setUseful();
                        }
                    }
                    break;

                case "#2":
                    if(inPrimitive == 0) {
                        methodCounter += 1;
                        traceStack.push(opArgs[1]);
                        nodeStack.push(thisLineNode);
                        if (nameLastWrittenBy.containsKey(opArgs[1]))
                            thisLineNode.addDependency(nameLastWrittenBy.get(opArgs[1]));
                    }
                    break;

                case "#3":
                    if(inPrimitive == 0) {
                        for (int i = 1; i < opArgs.length - 1; ++i) {
                            if (nameLastWrittenBy.containsKey(opArgs[i]))
                                thisLineNode.addDependency(nameLastWrittenBy.get(opArgs[i]));
                        }

                        nameLastWrittenBy.put(opArgs[opArgs.length - 1], thisLineNode);
                    }
                    break;

                case "##00":
                    if(inPrimitive == 0) {
                        if(!readingArgs) {
                            //staticMethod = true;
                            staticStack.push(true);
                        }
                        else {
                            readingArgs = false;
                            nameLastWrittenBy.put("@this", thisWrittenBy);
                            traceStack.pop();
                            dependOnLine = nodeStack.pop();
                            thisWrittenBy.addDependency(dependOnLine);
                            //staticMethod = false;
                            thisStack.push(thisWrittenBy);
                            staticStack.push(false);
                        }
                        methodCounterStack.push(methodCounter);
                        methodCounter = 0;
                    }
                    break;

                case "##01":
                    if(inPrimitive == 0) {
                        methodCounter = methodCounterStack.pop() + methodCounter;
                        if(!staticStack.peek()) {
                            thisStack.pop();
                            staticStack.pop();
                            thisWrittenBy = thisStack.peek();
                            nameLastWrittenBy.put("@this", thisWrittenBy);
                        }
                    }
                    break;

                case "##03":
                    if(inPrimitive == 0) {
                        tryConstructModule = true;
                    }
                    break;

                case "##04":
                    inPrimitive += 1;
                    break;

                case "##05":
                    inPrimitive -= 1;
                    break;

                case "##06":
                    inPrimitive += 1;
                    break;

                case "##07":
                    inPrimitive -= 1;
                    break;

                case "##08":
                    if(inPrimitive == 0) {
                        for (int i = 0; i < methodCounter; ++i) {
                            traceStack.pop();
                            nodeStack.pop();
                        }
                        methodCounter = 0;
                    }
                    break;

                default:
            }
        }
}