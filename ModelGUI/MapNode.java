/*
 * MapNode.java
 *
 * Created on July 11, 2003, 11:52 AM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import java.util.*;

public class MapNode {
    private String node;
    private Vector names;
    private Vector children;
    private int depth;
    
    /** Creates a new instance of MapNode */
    public MapNode(String nodeName) {
        node = nodeName;
        names = new Vector();
        children = new Vector();        
    }
    
    /* Creates a full doubly-linked list of MapNodes
     *  assumes that treeNode is the root of the tree
     *  calls traverseTree to recursively fill in all of the root's children
     */
    public MapNode(AdapterNode treeNode) {
        node = treeNode.getName();
        names = new Vector();
        children = new Vector(); 
        depth = -1;
        String nameAttrib = treeNode.getAttributeValue("name");
        if (nameAttrib.length() > 0) addName(nameAttrib);
        
        AdapterNode kid;
        List treeKids = treeNode.getChildren();
        Iterator it = treeKids.iterator();
        while (it.hasNext()) {
            kid = (AdapterNode)it.next();
            traverseTree(kid, this, depth+1);
        }
        //printTree();
    }
    
    public void addChild(String childName) {
        MapNode child = new MapNode(childName);
               
        if (!containsChild(childName)) {
            children.addElement(child);
        }            
    }
    
    private int addChild(MapNode child) {
        int ct = 0;
        MapNode currChild, newChild;
        String targetName, currName;
        boolean add = false;
        targetName = child.getNodeName();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            currChild = (MapNode)it.next();
            currName = currChild.getNodeName();
            if (targetName.equals(currName)) {
                newChild = new MapNode(targetName);
                newChild.setDepth(currChild.getDepth());
                newChild.setChildren(merge(currChild.getChildren(), child.getChildren()));
                newChild.setNames(merge(currChild.getPossibleNames(true), child.getPossibleNames(true)));
                
                replaceChild(targetName, newChild);
                return ct;
            }
            ct++;
        }
        children.addElement(child);
        return ct;
    }
    
    public void replaceChild(String childName, MapNode newChild) {
        int ct = 0;
        MapNode kid;
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (MapNode)it.next();
            if (childName.equals(kid.getNodeName())) {
                break;
            }
            ct++;
        }
        
        children.setElementAt(newChild, ct);
    }
    
    public void addName(String str) {
        if (!containsName(str)) {
            names.addElement(str);
        }
    }
    
    public boolean containsChild(String childName) {
        if (children == null || children.isEmpty()) {
            return false;
        }
        
        MapNode currChild;
        String currName;
        Iterator it = children.iterator();
        while (it.hasNext()) {
            currChild = (MapNode)it.next();
            currName = currChild.getNodeName();
            if (currName.equals(childName)) {
                return true;
            }
        }
        return false;
    }
    
    public boolean containsChild(MapNode child) {
        return containsChild(child.getNodeName());
    }
    
    public boolean containsName(String str) {
        if (names == null || names.isEmpty()) {
            return false;
        } else {
            return names.contains(str);
        }
    }
    
    //perform breadth-first search to find the descendant with "node" feild equal to childName
    public MapNode getDescendant(String childName) {
        Vector queue = new Vector();
        MapNode curr = this;
        
        do {
            if (curr.getNodeName().equals(childName)) return curr;
            else {
                queue.addAll(curr.getChildren());
            }
            
            curr = (MapNode)queue.elementAt(0);
            queue.removeElementAt(0);
        } while (curr != null);     
        
        return null;
    }
    
    public MapNode getDescendant(String parentName, int targetDepth) {
        MapNode parentNode = getDescendant(parentName);
        if (parentName == null) return null;
        
        MapNode child = (MapNode)parentNode.getChildren().elementAt(0);
        return child;
    }
        
    public MapNode getChild(int index) {
        if (children.size() > index) 
            return (MapNode)children.elementAt(index);
         else 
            return null;
    }
    
    //returns a child with the specified name or null if the child is not found
    public MapNode getChild(String childName) {
        MapNode kid;
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (MapNode)it.next();
            if (kid.getNodeName().equals(childName)) return kid;
        }
        return null;
    }
    
    public Vector getChildren() {
        return children;
    }
    
    public Vector getChildNames() {
        MapNode tempNode;
        String name;
        Vector childNames = new Vector();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            tempNode = (MapNode)it.next();
            name = tempNode.getNodeName();
            childNames.addElement(name);
        }
        
        return childNames;
    }
    
    public int getDepth() {
        return depth;
    }
    
    public String getNodeName() {
        return node;
    }
    
    public Vector getPossibleNames(boolean showParent) {
        if (showParent)
            return names;
        else {
            Vector newNames = new Vector();
            String currName;
            int index;
            Iterator it = names.iterator();
            while (it.hasNext()) {
                currName = it.next().toString();
                index = currName.indexOf('-');
                currName = currName.substring(index+1);
                if (!newNames.contains(currName)) newNames.addElement(currName);
            }
            
            return newNames;
        }   
    }
    
    public Vector getPossibleNames(String parentName) {
        Vector newNames = new Vector();
        String currName;
        int index;
        Iterator it = names.iterator();
        
        if (parentName.equals(ControlPanel.DEFAULT_PLURAL_STRING)) {
            //if different parents have the same node children, return list of children
            //  otherwise, return empty
            String currParent, prevParent = "";
            boolean firstRound = true;
            while (it.hasNext()) {
                currName = it.next().toString();
                index = currName.indexOf('-');
                
                currParent = currName.substring(0, index);
                currName = currName.substring(index+1);    
                
                if (firstRound && currParent.equals(prevParent)) prevParent = "";
                if (prevParent.equals("")) {
                    //first round of adding elements to newNames
                    newNames.addElement(currName);
                } else {
                    firstRound = false;
                    //check whether the node has aready been added
                    if (!newNames.contains(currName)) return new Vector();
                }
                prevParent = currParent;
            }
        } else {
            while (it.hasNext()) {
                currName = it.next().toString();
                index = currName.indexOf('-');
                if (parentName.equals(currName.substring(0, index))) {
                    newNames.addElement(currName.substring(index+1));
                }
            }
        }

        return newNames;  
    }
    
    public boolean equals(MapNode testNode) {
        if (!node.equals(testNode.getNodeName())) {
            return false;
        }
        if (children.size() != testNode.getChildren().size()) {
            return false;
        }
        if (!names.equals(testNode.getPossibleNames(true))) {
            return false;
        }
        
        Iterator it = children.iterator(), it2;
        MapNode node1, node2;
        String name1, name2;
        boolean found;
        while (it.hasNext()) {
            found = false;
            node1 = (MapNode)it.next();
            name1 = node1.getNodeName();
            it2 = testNode.getChildren().iterator();
            while(it2.hasNext()) {
                node2 = (MapNode)it2.next();
                name2 = node2.getNodeName();
                if (name1.equals(name2)) {
                    found = true;
                    break;
                }
            }
            if (!found) return false;
        }
        return true;
    }
    
    public boolean hasChildren() {
        if (children.size() > 0) return true;
        else return false;
    }
    
    public boolean hasPossibleNames() {
System.out.println(names);
        if (names.size() > 0) return true;
        else return false;
    }
    
    public boolean isLeaf() {
        if (children.size() > 0) return false;
        else return true;
    }
    
    private Vector merge(List v1, List v2) {
        Vector newVector = new Vector();
        newVector.addAll(v1);

        Object item;
        Iterator it = v2.iterator();
        while (it.hasNext()) {
            item = it.next();
            if (!newVector.contains(item)) {
                newVector.addElement(item);
            }
        }
        return newVector;
    }      
    
    public void setChildren(Vector newChildren) {
        children.removeAllElements();
        children = (Vector)newChildren.clone();
    }
    
    public void setDepth(int newDepth) {
        depth = newDepth;
    }
    
    public void setNames(Vector newNames) {
        names.removeAllElements();
        names = (Vector)newNames.clone();
    }
    
    private void traverseTree(AdapterNode treeNode, MapNode parent, int currDepth) {        
        AdapterNode kid;
        MapNode newNode = new MapNode(treeNode.getName());
        String nameAttrib = treeNode.getAttributeValue("name");
        if (nameAttrib != null && nameAttrib.length() > 0) {
            AdapterNode dad = treeNode.getParent();
            nameAttrib = dad.getAttributeValue("name") + "-" + nameAttrib;
            newNode.addName(nameAttrib);
        }
        newNode.setDepth(currDepth);
        int childIndex = parent.addChild(newNode);
        
        List treeKids = treeNode.getChildren();
        Iterator it = treeKids.iterator();
        while (it.hasNext()) {
            kid = (AdapterNode)it.next();
            traverseTree(kid, parent.getChild(childIndex), currDepth+1);
        }
    }
    
    public String toString() {
        MapNode tempNode;
        String str = "<" + node + "(" + depth + ")" + ", names:(";
        Iterator it = names.iterator();
        while (it.hasNext()) {
            str += it.next().toString() + ",";
        }
        str += "), children:(";
        
        it = children.iterator();
        while (it.hasNext()) {
            tempNode = (MapNode)it.next();
            str += tempNode.getNodeName() + ",";
        }
        str += ")>";
        
        return str;
    }
    
    public void printTree() { 
        Vector queue = new Vector();
        MapNode curr = this;
        
        System.out.println("Map Tree------");
        
        do {
            System.out.println(curr);
            queue.addAll(curr.getChildren());
            
            if (queue.isEmpty()) curr = null;
            else {
                curr = (MapNode)queue.firstElement();
                queue.removeElementAt(0);
            }
        } while (curr != null);     
        
        System.out.println("------end of tree");
    }  
}
