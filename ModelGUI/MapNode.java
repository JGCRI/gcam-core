/*
 * MapNode.java
 *
 * Created on July 11, 2003, 11:52 AM
 */

package ModelGUI;

import java.util.*;

/** This class provides a blueprint to the overall structure of the XML file.
 * ControlPanel uses a tree of MapNodes to determine the descendants and 
 * "name" attribute values of any JDOM node.
 *
 * <br> Each MapNode has Vectors <code> names </code> and <code> children </code>.
 * To create the full tree, MapNode traverses the JDOM. For every JDOM node, 
 * it creates or examines a MapNode. The new map node remembers all possible
 * names and children - for example, node <i> supplysector </i> can have names
 * <i> coal regional, delivered coal, regional biomass, </i> etc. and children 
 * <i> subsector </i>, etc. 
 *
 * <br> Each MapNode also has a <code> depth </code> field that keeps track of 
 * the node's level in the tree. Currently, the root node has depth -1. 
 * This allows the "world" node to have depth 0, and the "region" node to have 
 * depth 1. These numbers correspond to the current implementation of ControlPanel
 * and are used when manipulatin ControlPanel.QueryControls.
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

public class MapNode {
    private String node;
    private Vector names;
    private Vector children;
    private int depth;
    
    /** Creates a new instance of MapNode.
     *
     * @param nodeName the name of the node to be created 
     *      (not the value of the "name" attribute) */
    public MapNode(String nodeName) {
        node = nodeName;
        names = new Vector();
        children = new Vector();        
    }
    
    /* Creates a full doubly-linked list of MapNodes.
     * Assumes that treeNode is the root of the tree,
     * calls traverseTree to recursively fill in all of the root's children
     *
     * @param treeNode root of the JDOM tree that will be used to create the
     *      MapNode tree */    
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
    }
    
    /** Adds a new child to the current MapNode. Does nothing if a child with
     * <code> childName </code> already exists.
     *
     * @param childName node name of the new child to be created */    
    public void addChild(String childName) {
        MapNode child = new MapNode(childName);
               
        if (!containsChild(childName)) {
            children.addElement(child);
        }            
    }
    
    /** Adds a new child to the current MapNode. If a child with the same node 
     * name already exists, merges the data stored in both children by combining 
     * their names and children arrays. 
     *
     * @param child the node object to be added
     * @return the index in the children array where the node was added 
     *      (always at the end), or merged */
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
    
    /** Overwites the MapNode's child named <code> childName </code>
     * with <code> newChild </code>. If no such child is found,
     * adds <code> newChild </code> to the end of the children array.
     *
     * @param childName node name of child to be replaced
     * @param newChild MapNode containing new child data */    
    public void replaceChild(String childName, MapNode newChild) {
        int ct = 0;
        MapNode kid;
        boolean found = false;
        
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (MapNode)it.next();
            if (childName.equals(kid.getNodeName())) {
                found = true;
                break;
            }
            ct++;
        }
        
        if (found) children.setElementAt(newChild, ct);
        else children.addElement(newChild);
    }
    
    /** Adds a value of the "name" attribute to a list of MapNode's possible names.
     * Does nothing if that name already exists. 
     *
     * @param str the "name" attribute value to be appended */    
    public void addName(String str) {
        if (!containsName(str)) {
            names.addElement(str);
        }
    }
    
    /** Determines whether a MapNode child already exists in the children array.
     * Compares only the node names of MapNodes.
     *
     * @param childName name of child node to be compared to existing children array
     * @return true if a node by the name childName already exists */    
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
    
    /** Determines whether a MapNode child already exists in the children array.
     * Compares only the node names of MapNodes.
     * @see containsChild(String childName)
     *
     * @param child MapNode whose existance in the children array is being tested
     * @return true if a node with the same name as <code> child </code> 
     *      already exists */    
    public boolean containsChild(MapNode child) {
        return containsChild(child.getNodeName());
    }
    
    /**
     * @param str
     * @return  */    
    public boolean containsName(String str) {
        if (names == null || names.isEmpty()) {
            return false;
        } else {
            return names.contains(str);
        }
    }
    
    
    /** Performs breadth-first search to find the first descendant whose 
     * node name equals <code> childName </code>
     *
     * @param childName name of node that is being searched for
     * @return the top-most MapNode with node name <code> childName </code> */    
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
     
    /** Retrieves the child of MapNode at the specified index in the children array.
     *
     * @param index index in current MapNode's children array, starting at 0
     * @return the MapNode at target position or null the index is invalid */    
    public MapNode getChild(int index) {
        if (children.size() > index && index >= 0) 
            return (MapNode)children.elementAt(index);
         else 
            return null;
    }
    
    /** Retrieves the child of current MapNode with the specified node name.
     *
     * @param childName node name of target child
     * @return the MapNode child with <code> childname </code> or null if no
     *      such child exists */    
    public MapNode getChild(String childName) {
        MapNode kid;
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (MapNode)it.next();
            if (kid.getNodeName().equals(childName)) return kid;
        }
        return null;
    }
    
    /** Retrieves a list of current MapNode's children.
     *
     * @return Vector containing MapNode representations */    
    public Vector getChildren() {
        return children;
    }
    
    /** Obtains the node names of current MapNode's children.
     *
     * @return Vector of Strings where each String is the node name of one child */    
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
    
    /** Accesses the depth value of current MapNode. The root of MapNodes 
     * has depth -1, each subsequent level of children increases its depth 
     * value by 1.
     *
     * @return MapNode depth */    
    public int getDepth() {
        return depth;
    }
    
    /** Accesses the node name of current MapNode.
     *
     * @return name of node, as used by JDOM */    
    public String getNodeName() {
        return node;
    }
    
    /** Retrieves the list of possible "name" attributes that a node with 
     * current MapNode's node name may have in the JDOM.
     *
     * @param showParent boolean specifying whether to return the parent's node 
     *      name as part of name. Currently, the possible names are stored with 
     *      the name of the parent as a prefix. If showParent is <code> true </code>
     *      than a possible name will look like "coal - delivered coal", while 
     *      <code> false </code> returns "delivered coal"
     * @return Vector of Strings containing "name" attribute values */    
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
    
    /** Retrieves the possible "name" attributes of MapNode. The "name" attribute
     * or the MapNode's parent must match <code> parentName </code>. for example, 
     * this function is used to get all possible names that the children of 
     * < supplysector name="coal" > can have.
     *
     * @param parentName the specific (not node) name of MapNode's parent
     * @return Vector of String names */    
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
    
    /** Determines if the current MapNode is equivalent to <code> testNode </code>.
     * The comparison is based on each MapNode's node name, list of possible "name"
     * attribute values, and number and node names of each child.
     *
     * @param testNode the MapNode to be compaire to the current MapNode
     * @return true if the above-mentioned parameters are all equal */    
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
    
    /** Returns whether current MapNode has children.
     *
     * @return true if the children array is not empty */    
    public boolean hasChildren() {
        if (children.size() > 0) return true;
        else return false;
    }
    
    /** Returns whether current MapNode has possible "name" attribute values.
     *
     * @return true if MapNode has at least one name. */    
    public boolean hasPossibleNames() {
        if (names.size() > 0) return true;
        else return false;
    }
    
    /** Returns whether current MapNode has children.
     * @see hasChildren()
     *
     * @return true if the children array is empty */    
    public boolean isLeaf() {
        if (children.size() > 0) return false;
        else return true;
    }
    
    /** Combines two Vectors into a single object. Ensures that elements are 
     * not duplicated.
     *
     * @param v1 the larger of the two Vectors to be merged
     * @param v2 the smaller of the two Vectors to be merged
     * @return the combined Vector */
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
    
    /** Replaces the children of current MapNode.
     *
     * @param newChildren Vector of MapNodes representing the new child nodes */    
    public void setChildren(Vector newChildren) {
        children.removeAllElements();
        children = (Vector)newChildren.clone();
    }
    
    /** Sets the integer corresponding the node's depth in the tree. 
     * For compatibilty with ControlPanel, by default the root is at depth -1 
     * and its chidren are at parent depth plus one.
     *
     * @param newDepth the new depth value for current MapNode */    
    public void setDepth(int newDepth) {
        depth = newDepth;
    }
    
    /** Sets the array that holds possible "name" attribute values to 
     * a new Vector of Strings.
     *
     * @param newNames the collection of new names */    
    public void setNames(Vector newNames) {
        names.removeAllElements();
        names = (Vector)newNames.clone();
    }
   
    /** Recurses through the complete JDOM tree in a pos-order traversal. 
     * Creates the MapNode tree by generating children and linking them to their
     * pre-created parents.
     *
     * @param treeNode node in the JDOM tree that is to be examined and its 
     *      node name and "name" attribute to saved
     * @param parent the MapNode parent of the MapNode currently being created 
     * @param currDepth depth of the level of the node currenly being created*/
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
    
    /** Returns a String representation of current MapNode.
     * Takes the form: < nodeName(depth), names:(possibleNames), children:(namesOfChildren) >
     *
     * @return String version of a node and its fields */    
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
    
    /** Prints out the entire MapTree. Primarily used for debugging.
     **/
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
