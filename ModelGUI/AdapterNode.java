package ModelGUI;

/*
 * AdapterNode.java
 *
 * Created on June 4, 2003, 1:52 PM
 * @author Yulia Eyman (yulia@umd.edu)
 */

import org.jdom.*;
import java.util.*;
import java.lang.*;
import java.util.*;

/**
 * This class is a wrapper for Element objects, the basic units of a JDOM.
 * AdapterNode is used in all JTree representations and provides accessors for
 * TreeModel and ControlPanel.
 */
public class AdapterNode {
    Element node; 
    int levelIndex;
    
    /** Creates a new blank instance of AdapterNode, 
     node name is set to "custom", levelIndex to -1. */
    public AdapterNode() {
        node = new Element("custom");
        levelIndex = -1;
    }
    
    /** Creates a new instance of AdapterNode, with name nodeName, levelIndex -1.
     *
     * @param nodeName name of the new AdapterNode */
    public AdapterNode(String nodeName) {
        node = new Element(nodeName);
        levelIndex = -1;
    }
    
    /** Creates a new instance of AdapterNode which duplicates and wraps an 
     * existing Element object,  levelIndex is set to -1.
     *
     * @param newNode  Element object that will be duplicated*/
    public AdapterNode(Element newNode) {
        node = newNode;
        levelIndex = -1;
    }
        
    /** Creates a new node and inserts it as the last child.
     * Returns the index where the child was inserted, currently always
     * number of children minus 1.
     * This function is necessary for TreeModel.
     *
     * @param newChild the child AdapterNode
     * @return  the index of the newly added child*/
    public int addChild(AdapterNode newChild) {
        node.addContent(newChild.getElement());
        return (node.getChildren().size()-1);
    }
    
    /** Returns the unwrapped Element object stored in this AdapterNode.
     * This function is necessary for TreeModel. 
     *
     * @return original org.jdom.Element */
    private Element getElement() {
        return node;
    }
    
    /** Returns the AdpaterNode in position searchIndex of the child array of
     * current AdapterNode. This function is necessary for TreeModel.
     *
     * @param searchIndex index of target child
     * @return  the child at searchIndex*/
    public AdapterNode child(int searchIndex) {
        //Note: JTree index is zero-based.
        Element n = (Element)node.getChildren().get(searchIndex);
        return new AdapterNode(n);
    }
    
    /** Returns the number of children of current AdapterNode.
     * This function is necessary for TreeModel.
     *
     * @return the total number of children*/
    public int childCount() {
        return node.getChildren().size();
    }
    
    /** Returns the index of the specified Object in the current AdapterNode's
     * child array. This function is necessary for TreeModel.
     *
     * @param child the Object whose index in the children array is needed
     * @return  index of child*/
    public int index(Object child) {
        AdapterNode n = (AdapterNode)child;
        return node.getChildren().indexOf(n.node);
    }
    
    /** Recursively generates the name of a node by appending names of parents
     * up to Region name. This function is used in generating the Table view.
     *
     * @return String representation of AdapterNode's ansestor names*/
    public String toLefterString() {
        String name = node.getName();
        String retString = "";
        String tempString;
        String attrib = "";
        List attributes = node.getAttributes();
        Iterator attIter = attributes.iterator();
        
        // Stop moving up the tree at the regional level.
        if( !name.equals( "region" ) && node.getParent() != null ){
            retString =  new AdapterNode( node.getParent() ).toLefterString();
        }
        
        if( !(node.getName().equals( "period" )) && !(node.getName().equals("grade"))) {
            while ( attIter.hasNext()) {
                Attribute tempAtt = (Attribute)attIter.next();
                String attName = tempAtt.getName();
                if( attName.equals( "name" ) ) {
                    attrib += tempAtt.getValue();
                }
            }
            retString += ("<" + name + "=" + attrib + ">");
        }
        return retString;
    }
    
    /** Returns a String representation of an AdapterNode.
     * Takes the form: " < name > attribute1Name=attribute1Value attribute2Name=attribute2Value ..."
     * This function is necessary for TreeModel and used by ControlPanel.
     *
     * @return  String representation of AdapterNode*/
    public String toString() {
        String name = node.getName();
        List attributes = node.getAttributes();
        String attrib = "";
        String a;
        
        Iterator it = attributes.iterator();
        while (it.hasNext()) {
            a = it.next().toString();
            //parse the Attribute so only its name and value is displayed
            int index1 = a.indexOf(':');
            int index2 = a.lastIndexOf(']');
            attrib += a.substring(index1+1, index2);
        }
        
        return ("<" + name + ">" + attrib);
    }
        
    /** Returns a complete List of AdapterNode's parents, grandparents, etc.
     * The List's first element is the AdapterNode's parent, the last is
     * the root of the tree. The objects in the List are org.jdom.Element
     *
     * @return  List of all ansestors*/
    public List getAnsestors() {
        Vector list = new Vector();
        
        //get all parents, ending at the root
        Element parent = node.getParent();
        while (parent != null) {
            list.add(parent);
            parent = parent.getParent();
        }
        return list;
    }
    
    /** Returns a String containing all of current AdapterNode's attributes.
     * The String takes the form: 
     * "attribute1Name=attribute1Value attribut2Name=attribute2Value ..."
     *
     * @return String representaion of all attribute names and values */
    public String getAttributes() {
        List attribs = node.getAttributes();
        
        String strAttribs = "";
        Attribute at;
        Iterator it = attribs.iterator();
        while(it.hasNext()) {
            at = (Attribute)it.next();
            strAttribs = strAttribs + at.getName() + "=" + at.getValue();
        }
        return strAttribs;   
    }
    
    /** Returns the String value of the AdapterNode's attribute with
     * the specified attribName.
     *
     * @param attribName name of target attribute
     * @return String value of target attribute */
    public String getAttributeValue(String attribName) {
        return node.getAttributeValue(attribName);
    }
    
    /** Returns the first child of AdapterNode with name NodeName, 
     * value of its Name attribute equal to attribVal.
     * If attribVal is an empty string, returns the fist child with name nodeName.
     * Returns null if no child meets above criteria.
     *
     * @param nodeName name of child node
     * @param attribVal name of target child's attribute
     * @return AdapterNode representation of child */
    public AdapterNode getChild(String nodeName, String attribVal) {
        Element kid;
        List children = node.getChildren();
        Iterator it = children.iterator();

        while (it.hasNext()) {
            kid = (Element)it.next();
            if (nodeName.equals(kid.getName())) {
                if (attribVal.equals("") || attribVal.equals(kid.getAttributeValue("name"))) {
                    return new AdapterNode(kid);
                }
            }
        }
        return null;
    }
    
    /** Returns a List of all children of AdapterNode with the 
     * name NodeName, value of its Name attribute equal to attribVal.
     * attribVal may be an empty String.
     * Returns a List with no elements if children cannot be found.
     *
     * @param nodeName name of child node
     * @param attribVal name of target child's attribute
     * @return List of AdapterNode representations of children */
    public List getChildren(String nodeName, String attribVal) {
        Vector list = new Vector();
        
        Element kid;
        List children = node.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (Element)it.next();
            if (nodeName.equals(kid.getName())) {
                if (attribVal.equals("") || attribVal.equals(kid.getAttributeValue("name"))) {
                    list.addElement(new AdapterNode(kid));
                }
            }
        }
        return list;
    }
    
    /** Returns a complete List of current AdapterNode's children.
     * The List contains AdapterNode elements, and is an empty List if
     * no children exist.
     *
     * @return List of AdapterNode representations of children */
    public List getChildren() {
        Vector list = new Vector();
        
        Element kid;
        List children = node.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (Element)it.next();
            list.addElement(new AdapterNode(kid));
        }
        return list;
    }
    
    /** Returns the name of the current AdapterNode.
     *
     * @return  String name*/
    public String getName() {
        return node.getName();
    }
   
    /** Returns the levelIndex of the current AdapterNode, -1 by default.
     *
     * @return int levelIndex */
    public int getIndex() {
        return levelIndex;
    }
    
    /** Returns the immediate parent of the current AdapterNode.
     *
     * @return AdapterNode parent */
    public AdapterNode getParent() {
        Element parent = node.getParent();
        
        if (parent == null) return null;
        else return new AdapterNode(parent);
    }
    
    /** Returns the text, or value, of the current AdapterNode.
     *
     * @return String text */
    public String getText() {
        String val = node.getText();
        return val.trim();
    }
    
    /** Returns true if the current AdapterNode has at least one child with
     * the name childName.
     *
     * @param childName name of target child
     * @return true if child exists */
    public boolean hasChild(String childName) {
        Element kid;
        List children = node.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (Element)it.next();
            if (childName.equals(kid.getName())) {
                return true;
            }
        }
        return false;
    }
   
    /** Returns true if current AdapterNode has at least one child with
     * the name childName and an attribute named attribName.
     *
     * @param childName name of target child
     * @param attribName name of attribute possesed by target child
     * @return true if child exists */ 
    public boolean hasChildWithAttribute(String childName, String attribName) {
        Element kid;
        Attribute attrib;
        List children = node.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (Element)it.next();
            if (childName.equals(kid.getName())) {
                attrib = kid.getAttribute(attribName);
                if (attrib != null) return true;
            }
        }
        return false;
    }
    
    /** Returns true if current AdapterNode has no children.
     *
     * @return true if AdapterNode is a leaf */
    public boolean isLeaf() {
        if (node.getChildren().size() < 1) return true;
        else return false;
    }
    
    /** Removes the first child of current AdapterNode with the name childName.
     * Returns the index that the removed child used to have in the children
     * array.
     *
     * @param childName name of target child
     * @return index of child that was removed */
    public int removeChild(String childName) {
        int ct = 0;
        List children = node.getChildren();
        Iterator it = children.iterator();
        Element kid;
        while(it.hasNext()) {
            kid = (Element)it.next();
            if (kid.getName().equals(childName)) break;
            ct++;
        }
        
        node.removeChild(childName);
        
        return ct;
    }
    
    /** Sets the value of levelIndex to index.
     *
     * @param index value of levelIndex */
    public void setIndex(int index) {
        levelIndex = index;
    }
    
    /** Sets the text, or value, of current AdapterNode to text.
     *
     * @param text new text of AdapterNode */
    public void setText(String text) {
        node.setText(text);
    }
    
    /** Sets the attribute with the name atName to the value atValue
     *
     * @param atName name of target attribute
     * @param atValue value of target attribute */
    public void setAttribute(String atName, String atValue) {
        Element newNode = node.setAttribute(atName, atValue);
        node = newNode;
    }
    
    /** Sets the list of attributes of current AdapterNode to attribs.
     *
     * @param attribs List of new attributes */
    public void setAttributes(List attribs) {
        node.setAttributes(attribs);
    }
    
    /** Duplicates the current AdapterNode. Copies the name, text, and
     * attributes but not the parent.
     *
     * @return unattached duplicate of AdapterNode */
    public Object clone(){
        AdapterNode newNode = new AdapterNode(node.getName());
        newNode.setText(node.getText());
        newNode.setAttributes(node.getAttributes());
        
        return newNode;
    }
}
