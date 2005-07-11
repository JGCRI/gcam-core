/**
 * 
 */
package utils;

import java.awt.Container;
import java.awt.Window;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xerces.dom3.bootstrap.DOMImplementationRegistry;
import org.apache.xpath.domapi.XPathEvaluatorImpl;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.w3c.dom.xpath.XPathResult;

/**
 * A set of methods for manipulating and querying the DOM.
 * 
 * @author Josh Lurz
 */
public final class DOMUtils {
    /**
     * Get the number of element nodes which are the children of a node.
     * 
     * @param aNode
     *            Node for which to count the number of element children.
     * @return The number of children of the node which are elements.
     */
    public static int getNumberOfElementChildren(Node aNode) {
        if(aNode == null) {
            Logger.global.log(Level.WARNING, "Cannot count the number of children of a null element.");
            return 0;
        }
        // Count the child nodes which are elements.
        int size = 0;
        NodeList childNodes = aNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); ++i) {
            if (childNodes.item(i).getNodeType() == Node.ELEMENT_NODE) {
                ++size;
            }
        }
        return size;
    }

    /**
     * Create the nodes in an XPath if they do not exist. This code is currently
     * a pretty big hack. I couldn't find a library routine to do this.
     * 
     * @param aDocument
     *            Document to add the nodes to.
     * @param aXPath
     *            Path for which to create the nodes.
     * @return The last node added.
     */
    public static Node addNodesForXPath(Document aDocument, String aXPath) {
    	// Check for a null document.
    	if(aDocument == null){
    		Logger.global.log(Level.WARNING, "Cannot add nodes for a null document.");
    		return null;
    	}
        // Split the string around the slashes.
        String[] comps = aXPath.split("/+"); //$NON-NLS-1$

        // Get the root node of the document.
        Node current = aDocument.getDocumentElement();

        for (int i = 0; i < comps.length; ++i) {
            // Skip "node()" and blank items.
            if (comps[i].equals("node()") || comps[i].length() == 0) { //$NON-NLS-1$
                continue;
            }
            boolean foundNode = false;

            // Construct a new node even if we dont need to
            // add it later so we can use the equals method.
            Node newNode = DOMUtils.constructNode(aDocument, comps[i]);
            // If the new node is null return immediately.
            if (newNode == null) {
                return null;
            }
            // Skip the root node. TODO: Improve this hack.
            if (newNode.getNodeName().equals(
                    aDocument.getDocumentElement().getNodeName())) {
                continue;
            }
            // Search for the node in the list of children of the current node.
            NodeList children = current.getChildNodes();
            for (int j = 0; j < children.getLength(); ++j) {
                Node currChild = children.item(j);
                // Skip non-element nodes.
                if (currChild.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }
                // Check if the node names match.
                if (currChild.getNodeName().equals(newNode.getNodeName())) {
                    // Check if the attributes match.
                    String currChildNameAttr = DOMUtils
                            .getNameAttrValue(currChild);
                    String newNodeNameAttr = DOMUtils.getNameAttrValue(newNode);
                    // Check if both have null names or the names match.
                    if (((currChildNameAttr == null) && (newNodeNameAttr == null))
                            || ((currChildNameAttr != null) && currChildNameAttr
                                    .equals(newNodeNameAttr))) {
                        current = currChild;
                        foundNode = true;
                        break;
                    }
                }
            }
            // If the node wasn't found we need to add it.
            if (!foundNode) {
                current.appendChild(newNode);
                current = newNode;
            }
        }
        // Return the last node in the path.
        return current;
    }

    /**
     * Construct a node from a single part of an XPath string.
     * 
     * @param aDocument
     *            A document to use to construct the node.
     * @param aPartialPath
     *            A string containing a section of an XPath query representing a
     *            single node.
     * @return A newly constructed node.
     */
    static Node constructNode(Document aDocument, String aPartialPath) {
        // Find the node portion of the path before the bracket.
        String[] compParts = aPartialPath.split("\\["); //$NON-NLS-1$

        Node newNode = aDocument.createElement(compParts[0]);

        // See if we need to add an attribute.
        if (compParts.length > 1) {
            // The name of the attribute is between the @ and the =.
            int atLocation = compParts[1].indexOf('@');
            int equalsLocation = compParts[1].indexOf('=');
            String attrName = compParts[1].substring(atLocation + 1,
                    equalsLocation);
            // Now find the value.
            int quoteLocOne = compParts[1].indexOf('=');
            int quoteLocTwo = compParts[1].indexOf(']');
            String attrValue = compParts[1].substring(quoteLocOne + 2,
                    quoteLocTwo - 1);
            // Refuse to create names that are null.
            if (attrValue.equals("null")) { //$NON-NLS-1$
                Logger.global.log(Level.WARNING, Messages
                        .getString("FileUtils.31")); //$NON-NLS-1$
                return null;
            }
            // Add the attribute.
            Attr newAttr = aDocument.createAttribute(attrName);
            newAttr.setNodeValue(attrValue);
            newNode.getAttributes().setNamedItem(newAttr);
        }
        return newNode;
    }

    /**
     * Write an XML document to a file.
     * 
     * @param aDocument
     *            The document to serialize.
     * @return Whether the document was successfully serialized.
     * @param aContainer
     *            A container to use as the parent frame for printing error
     *            dialogs.
     */
    public static boolean serializeDocument(Document aDocument,
            Container aContainer) {
    	// Check for a null document.
    	if(aDocument == null){
    		Logger.global.log(Level.WARNING, "Cannot serialize a null document.");
    		return false;
    	}
        // Create the implementation which is needed for serializing.
        System.setProperty(DOMImplementationRegistry.PROPERTY,
                "org.apache.xerces.dom.DOMImplementationSourceImpl"); //$NON-NLS-1$
        DOMImplementationRegistry registry = null;
        try {
            registry = DOMImplementationRegistry.newInstance();
        } catch (Exception e) {
            // Unexpected error creating the DOM registry. Inform the user
            // and log the error.
            Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
            String errorMessage = Messages.getString("DOMUtils.12") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            String errorTitle = Messages.getString("DOMUtils.14"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aContainer, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }
        DOMImplementationLS domImpl = null;
        try {
            domImpl = (DOMImplementationLS) registry.getDOMImplementation("LS"); //$NON-NLS-1$
        } catch (Exception e) {
            // Unexpected error creating the DOM implementation. Inform the user
            // and log the error.
            Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
            String errorMessage = Messages.getString("DOMUtils.16") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            String errorTitle = Messages.getString("DOMUtils.18"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aContainer, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // Create the serialize.
        LSSerializer writer = domImpl.createLSSerializer();

        // Mark that the file is no longer in need of a save so the
        // attribute doesn't show up in output.
        aDocument.getDocumentElement().removeAttribute("needs-save"); //$NON-NLS-1$

        // Serialize the document into a string.
        String serializedDocument = writer.writeToString(aDocument);

        // Now attempt to write it to a file.
        try {
            File outputFile = FileUtils.getDocumentFile(aDocument);
            assert (outputFile != null);
            FileWriter fileWriter = new FileWriter(outputFile);
            fileWriter.write(serializedDocument);
            fileWriter.close();
            return true;
        } catch (IOException e) {
            // Unexpected error creating writing the file. Inform the user
            // and log the error.
            e.printStackTrace();
            Logger.global.throwing("FileUtils", "serializeDocument", e); //$NON-NLS-1$ //$NON-NLS-2$
            String errorMessage = Messages.getString("DOMUtils.22") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            String errorTitle = Messages.getString("DOMUtils.24"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aContainer, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }
    }

    /**
     * Helper method to perform an XPath query on the Document and return a
     * resulting node.
     * 
     * @param aDocument
     *            Document on which to perform the query.
     * @param aXPath
     *            Query to perform on the document.
     * @return The node which satisfies the query. If none are found this will
     *         be null, if multiple are found this will return the last.
     */
    static public Node getResultNodeFromQuery(Document aDocument, String aXPath) {
        // A query may be performed before the document is set, avoid attempting
        // the query.
        if (aDocument == null) {
            return null;
        }
        assert (aXPath != null);
        // Create the XPath evaluator.
        XPathEvaluatorImpl xPathEvaluator = new XPathEvaluatorImpl(aDocument);

        // Perform an XPath query on the tree. If this proves too slow
        // we could do the search initially and store the node.
        XPathResult result = (XPathResult) xPathEvaluator
                .createExpression(
                        aXPath,
                        xPathEvaluator.createNSResolver(aDocument
                                .getDocumentElement())).evaluate(
                        aDocument.getDocumentElement(),
                        XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

        // Check for any results. Loop so that we always use the last node.
        Node currResultNode = null;
        Node resultNode = null;
        boolean valueSet = false;
        while ((currResultNode = result.iterateNext()) != null) {
            resultNode = currResultNode;
            // If we already set this once we have multiple values matching the
            // XPath. This is not illegal but merits a warning.
            if (valueSet) {
                Logger.global.log(Level.INFO, Messages
                        .getString("DOMUtils.10") + aXPath); //$NON-NLS-1$
            }
            valueSet = true;
        }
        return resultNode;
    }

    /**
     * Get an initialized document builder.
     * 
     * @param aWindow
     *            A window used to center error messages, allowed to be null.
     * @return An initialized document builder.
     */
    public static DocumentBuilder getDocumentBuilder(Window aWindow) {
        // Create the document builder.
        DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory
                .newInstance();
        DocumentBuilder docBuilder = null;
        try {
            docBuilder = docBuilderFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            // Unexpected error creating the document builder. Notify
            // the user of the error and print to the log.
            Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
            String errorMessage = Messages.getString("DOMUtils.7") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            String errorTitle = Messages.getString("DOMUtils.9"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aWindow, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
        }
        return docBuilder;
    }

    /**
     * Get the value of the name attribute of a specific node.
     * 
     * @param aNode
     *            Node to fetch the name attribute of.
     * @return The value of the name attribute for a node.
     */
    public static String getNameAttrValue(Node aNode) {
        if (aNode == null) {
            return null;
        }
        // Get the name attribute from a node.
        NamedNodeMap attrs = aNode.getAttributes();
        if (attrs == null) {
            return null;
        }
        Node nameAttr = attrs.getNamedItem("name"); //$NON-NLS-1$
        if (nameAttr == null) {
            return null;
        }
        return nameAttr.getNodeValue() != "" ? nameAttr.getNodeValue() : null; //$NON-NLS-1$
    }

    /**
     * Helper function to find a single item in the list.
     * 
     * @param aParent Parent who's list to search.
     * @param aObject
     *            A list item to find.
     * @return The index into the DOM of the item, -1 if an error occurs or the
     *         item cannot be found.
     */
    static public int getDOMIndexOfObject(Node aParent, Object aObject) {
        // Check if the parent is null.
        if (aParent == null) {
            Logger.global.log(Level.WARNING,
                    "Cannot get the DOM index because parent node is null.");
            return -1;
        }

        if (aObject == null || !(aObject instanceof Node)) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.22")); //$NON-NLS-1$
            return -1;
        }

        // Get the name of the new object.
        String newNodeName = getNameAttrValue((Node) aObject);
        NodeList children = aParent.getChildNodes();

        for (int i = 0; i < children.getLength(); ++i) {
            String name = getNameAttrValue(children.item(i));
            // Check if the child matches the requested element.
            if ((name != null) && name.equals(newNodeName)) {
                return i;
            }
        }
        // The item was not found.
        return -1;
    }

    /**
     * Return the DOM child index for a list index. This conversion ignores
     * non-element nodes.
     * 
     * @param aParentNode
     *            The parent node who's children should be searched for the
     *            correct list index.
     * @param aListIndex
     *            The list index to convert to a DOM index.
     * @return The DOM index for a list index, -1 on failure.
     */
    public static int getDOMIndexForListIndex(Node aParentNode, int aListIndex) {
        if (aParentNode == null) {
            Logger.global
                    .log(Level.WARNING,
                            "Cannot convert list index to DOM index because the parent node is null.");
            return -1;
        }
        if (aListIndex < 0) {
            Logger.global
                    .log(Level.WARNING,
                            "Illegal negative list index passed as argument to DOM index conversion.");
            return -1;
        }

        // Find the actual position ignoring non element nodes.
        NodeList children = aParentNode.getChildNodes();
        if (children != null) {
            int elementNodes = -1;
            for (int i = 0; i < children.getLength(); ++i) {
                // Increment the actual child count if this location
                // contains an element node.
                if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                    ++elementNodes;
                }

                // Check if the actual position is the desired position. This
                // will only be true for element nodes given that elementNodes
                // is
                // initialized to negative 1.
                if (elementNodes == aListIndex) {
                    return i;
                }
            }
        }
        // Allow the caller to handle this failure.
        return -1;
    }

    /**
     * Create a new element and add a name attribute which is the value of the
     * object.
     * 
     * @param aParent
     *            The node which will be the parent of the new node.
     * @param aElementName
     *            The name of the new element.
     * @param aObject
     *            An object to convert into a node.
     * @param aAddTextContent
     *            Whether to set the value of the object as the text content of
     *            the node.
     * @return A new node representing the object.
     */
    public static Element createElement(Node aParent, String aElementName,
            Object aObject, boolean aAddTextContent) {
        // Check if the parent is null.
        if(aParent == null) {
            Logger.global.log(Level.WARNING, "Cannot create element because the parent is null.");
            return null;
        }
        
        // Check if the object to add is null.
        if(aObject == null) {
            Logger.global.log(Level.WARNING, "Cannot create element because the object to wrap is null.");
            return null;
        }
        Element newElement = aParent.getOwnerDocument().createElement(
                aElementName);
        newElement.setAttribute("name", aObject.toString()); //$NON-NLS-1$

        // Set the name as the text content if it was requested.
        if (aAddTextContent) {
            newElement.setTextContent(aObject.toString());
        }
        return newElement;
    }

    /**
     * Get the previous list item starting at an item.
     * 
     * @param aParentNode The parent of the item.
     * @param aItem
     *            The list item to find the item before.
     * @return The previous list item, null if there is not one.
     */
    static public Node getItemBefore(Node aParentNode, Node aItem) {
        // Get the index of the node.
        int domIndex = getDOMIndexOfObject(aParentNode, aItem);
    
        // Iterate backwards and search for a list item.
        NodeList children = aParentNode.getChildNodes();
        for (int i = domIndex - 1; i >= 0; --i) {
            // Check if the item is a element node, which represent a list item.
            if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                return children.item(i);
            }
        }
        // If the search failed return null, calling function will handle this.
        return null;
    }

    /**
     * Get the next list item starting at an item.
     * 
     * @param aParentNode Parent of the item.
     * @param aItem
     *            The list item to find the item after.
     * @return The next list item, null if there is not one.
     */
    static public Node getItemAfter(Node aParentNode, Node aItem) {
        // Get the index of the node.
        int domIndex = getDOMIndexOfObject(aParentNode, aItem);
    
        // Iterate forwards and search for a list item.
        NodeList children = aParentNode.getChildNodes();
        for (int i = domIndex + 1; i < children.getLength(); ++i) {
            // Check if the item is a element node, which represent a list item.
            if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                return children.item(i);
            }
        }
        // If the search failed return null, calling function will handle this.
        return null;
    }

    /**
     * Helper function to find the list index of an item.
     * 
     * @param aParentNode The parent node who's children to search for the list item.
     * @param aListItem
     *            A item to find the list index of.
     * @return The index into the list of the item, -1 if an error occurs or the
     *         item cannot be found.
     */
    static public int getListIndexOfObject(Node aParentNode, Object aListItem) {
        // Check if this operation can be performed.
        if (aParentNode == null) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.23")); //$NON-NLS-1$
            return 0;
        }
    
        if (aListItem == null || !(aListItem instanceof Node)) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.24")); //$NON-NLS-1$
        }
    
        // Get the name of the new object.
        String newNodeName = getNameAttrValue((Node) aListItem);
        NodeList children = aParentNode.getChildNodes();
    
        // Keep track of the number of elements, or list items found.
        int listIndex = -1;
        for (int i = 0; i < children.getLength(); ++i) {
            // Check if the item is a list item, or element.
            if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                ++listIndex;
            } else {
                // Don't check the names of non-element nodes.
                continue;
            }
            String name = getNameAttrValue(children.item(i));
            // Check if the child matches the requested element.
            if ((name != null) && name.equals(newNodeName)) {
                return listIndex;
            }
        }
        // The item was not found.
        return -1;
    }
}
