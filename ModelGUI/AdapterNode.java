/*
 * AdapterNode.java
 *
 * Created on June 4, 2003, 1:52 PM
 */

package ModelGUI;

/**
 *
 * @author  yeyman
 */

import org.jdom.*;
import java.util.*;
import java.lang.*;
import java.util.*;

public class AdapterNode {
    Element node;
    int levelIndex;
    
    /** Creates a new instance of AdapterNode */
    public AdapterNode() {
        node = new Element("custom");
    }
    
    /*public AdapterNode(String value) {
        node = new Element("custom");
        node.setText(value);
    }*/
    
    public AdapterNode(Element newNode) {
        node = newNode;
        levelIndex = -1;
    }
    
    /************** Functions used by TreeModel********************/
    
    public AdapterNode child(int searchIndex) {
        //Note: JTree index is zero-based.
        Element n = (Element)node.getChildren().get(searchIndex);
        return new AdapterNode(n);
    }
    
    public int childCount() {
        return node.getChildren().size();
    }
    
    public int index(Object child) {
        AdapterNode n = (AdapterNode)child;
        return node.getChildren().indexOf(n.node);
    }
    
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
    
    /************* Accesessor Functions*****************/
    
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
    
    public String getAttributeValue(String attribName) {
        return node.getAttributeValue(attribName);
    }
    
    /*
     * Returns the first child of node whose name and attribute match the parameters
     *      nodeName - name of the child node
     *      attribVal - value of the Name attribute
     *          if attribVal is an empty string, return the fist child with the name nodeName
     * Returns null if such a child does not exist
     */
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
    
    public List getChildren(String nodeName, String attribVal) {
        Vector list = new Vector();
        
        Element kid;
        List children = node.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (Element)it.next();
System.out.println("looking at " + kid.getName());
            if (nodeName.equals(kid.getName())) {
System.out.println("Blah 1");
                if (attribVal.equals("") || attribVal.equals(kid.getAttributeValue("name"))) {
System.out.println("Blah 2");
                    list.addElement(new AdapterNode(kid));
System.out.println("added " + kid.getName());
                }
            }
        }
        return list;
    }
    
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
    
    public String getName() {
        return node.getName();
    }
    
    public int getIndex() {
        return levelIndex;
    }
    
    public String getText() {
        String val = node.getText();
        return val.trim();
    }
    
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
    
    public boolean isLeaf() {
        if (node.getChildren().size() < 1) return true;
        else return false;
    }
    
    /***************** Modifier Functions ******************/
    
    public void setIndex(int index) {
        levelIndex = index;
    }
    
    public void setText(String text) {
        node.setText(text);
    }
}
