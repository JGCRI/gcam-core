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
    
    /** Creates a new instance of WrappedNode */
    public AdapterNode(Element newNode) {
        node = newNode;
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
    
    public String getValue() {
        String val = node.getText();
        return val.trim();
    }    
    
    public int index(Object child) {
        AdapterNode n = (AdapterNode)child;
        return node.getChildren().indexOf(n.node);
    }
    
    public AdapterNode child(int searchIndex) {
        //Note: JTree index is zero-based.
        Element n = (Element)node.getChildren().get(searchIndex);
        return new AdapterNode(n); 
    }
    
    public int childCount() {
        return node.getChildren().size(); 
    }
    
    public List getTableHeadings() {
        Vector list = new Vector();
        
        //get all parents, ending at the root
        Element parent = node.getParent();
        while (parent != null) {
            list.add(parent);
            parent = parent.getParent();
        }
        return list;
    }
    
    public String getName() {
        return node.getName();
    }
    
    public void setText(String text) {
        node.setText(text);
    }
}
