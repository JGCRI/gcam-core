/*
 * WrappedNode.java
 *
 * Created on May 28, 2003, 3:07 PM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import org.w3c.dom.*;
import java.util.*;
import java.lang.*;
import java.util.*;

public class WrappedNode {
    Node node;
    
    /** Creates a new instance of WrappedNode */
    public WrappedNode(Node newNode) {
        node = newNode;
    }
    
    public String toString() {
        String str = "";
        String nodeName = "<" + node.getNodeName() + ">";
        if (nodeName.equals("<#text>")) {
            nodeName = node.getNodeValue().trim();
        }
        
        if (node.hasAttributes()) {
            NamedNodeMap attributes = node.getAttributes();
            String attrib = "";
            Node a;
            for (int j = 0; j < attributes.getLength(); j++) {
                a = attributes.item(j);
                attrib = " " + a.getNodeName() + "=" + a.getNodeValue();
                str = str + attrib;
            }
        }
       
        return (nodeName + str);

       /* if (node.getNodeValue() != null) {
            if (s.startsWith("ProcInstr"))
                s += ", ";
            else
                s += ": ";
            
            // Trim the value to get rid of NL's
            //    at the front
            String t = node.getNodeValue().trim();
            int x = t.indexOf('\"');
            if (x >= 0) t = t.substring(0, x);
            s += t;
        } */      
        
        /*String name = node.getName();
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
         
        return ("<" + name + ">" + attrib);     */
    }
    
    public String getValue() {
        String val;
        if (node.getNodeType() == Node.TEXT_NODE) {
            val = node.getNodeValue();
        } else {
            val = node.getFirstChild().getNodeValue();
        }
        return val.trim();
    }    
    
    public int index(Object child) {
        int count = childCount();
        for (int i=0; i<count; i++) {
            WrappedNode n = this.child(i);
            if (child == n) return i;
        }
        return -1; // Should never get here.
        
        /*WrappedNode n = (WrappedNode)child;
        return node.getChildren().indexOf(n.node);*/
    }
    
    public WrappedNode child(int searchIndex) {
        //Note: JTree index is zero-based.
        Node n = node.getChildNodes().item(searchIndex);
        return new WrappedNode(n);
        //Note: JTree index is zero-based.
       /* Element n = (Element)node.getChildren().get(searchIndex);
        return new WrappedNode(n); */
    }
    
    public int childCount() {
        return node.getChildNodes().getLength(); 
        
       /* int ct = 0;
        if (node.hasChildNodes()) {
            NodeList children = node.getChildNodes();
            for (int j = 0; j < children.getLength(); j++) {
                if (children.item(j).getNodeType() != Node.TEXT_NODE)
                    ct++;
            }
        }   
        return ct; */
    }
}
