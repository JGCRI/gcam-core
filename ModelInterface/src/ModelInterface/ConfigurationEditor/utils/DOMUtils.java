/**
 * 
 */
package ModelInterface.ConfigurationEditor.utils;

import java.awt.Container;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import org.apache.xpath.domapi.XPathEvaluatorImpl;
import org.w3c.dom.Attr;
import org.w3c.dom.DOMImplementation;
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
     * Private constructor to prevent creation of the static class.
     */
    private DOMUtils() {
        super();
    }
    /**
     * Get the number of element nodes which are the children of a node.
     * 
     * @param aNode
     *            Node for which to count the number of element children.
     * @return The number of children of the node which are elements.
     */
    public static int getNumberOfElementChildren(final Node aNode) {
        if(aNode == null) {
            Logger.global.log(Level.WARNING, "Cannot count the number of children of a null element.");
            return 0;
        }
        // Count the child nodes which are elements.
        int size = 0;
        final NodeList childNodes = aNode.getChildNodes();
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
    public static Node addNodesForXPath(final Document aDocument, final String aXPath) {
    	// Check for a null document.
    	if(aDocument == null){
    		Logger.global.log(Level.WARNING, "Cannot add nodes for a null document.");
    		return null;
    	}
        
        // Split the string around the slashes.
        final String[] comps = aXPath.split("/+"); //$NON-NLS-1$

        // Create the XPath evaluator.
        final XPathEvaluatorImpl xPathEvaluator = new XPathEvaluatorImpl(aDocument);
        
        // Store the parent of the current query.
        Node parent = null;
        
        // Iterate over each section of the XPath and determine if it 
        // exists already. If it does not, attempt to create it.
        final StringBuilder query = new StringBuilder();
        for(int i = 0; i < comps.length; ++i) {
            // Add the node to the query string.
            query.append("/").append(comps[i]);
            
            // Perform an XPath query on the tree. If this proves too slow
            // we could do the search initially and store the node.
            final XPathResult result = executeQuery(aDocument, query.toString(), xPathEvaluator);
            
            // Check the number of results.
            final Node firstResult = result.iterateNext();
            // If there isn't a result the node must be created.
            if(firstResult == null) {
                // Create the new node.
                final Node newNode = constructNode(aDocument, comps[i]);
                
                // Check if construction failed.
                if(newNode == null) {
                    break;
                }
                // If the parent is null than there is no document element.
                if(parent == null) {
                    aDocument.appendChild(newNode);
                }
                // Otherwise add it to the parent.
                else {
                    parent.appendChild(newNode);
                }
                // Set the parent to the result.
                parent = newNode;
            }
            else {
                // Check for multiple results.
                final Node secondResult = result.iterateNext();
                if(secondResult == null) {
                    // Set the parent to the single result.
                    parent = firstResult;
                }
                else {
                    // Multiple results.
                    Logger.global.log(Level.WARNING, "Cannot add nodes for XPath with multiple results.");
                    break;
                }
            }
        }
        
        // Check if the process was successful by executing the entire
        // query.
        final XPathResult result = executeQuery(aDocument, aXPath, xPathEvaluator);
        
        // Unneccessary to check for multiple results here as they were 
        // checked for at every level.
        return result.iterateNext();
    }
    
    /**
     * Execute an XPath query on a document.
     * 
     * @param aDocument Document to perform the query on.
     * @param aXPath Query to execute.
     * @param xPathEvaluator Evaluator to use to execute.
     * @return The result of the query.
     */
    private static XPathResult executeQuery(final Document aDocument, final String aXPath, final XPathEvaluatorImpl xPathEvaluator) {
        return (XPathResult) xPathEvaluator
                .createExpression(
                        aXPath,
                        xPathEvaluator.createNSResolver(aDocument
                                .getDocumentElement())).evaluate(
                        aDocument.getDocumentElement(),
                        XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
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
    static Node constructNode(final Document aDocument, final String aPartialPath) {
        // Find the node portion of the path before the bracket.
        final String[] compParts = aPartialPath.split("\\["); //$NON-NLS-1$

        final Node newNode = aDocument.createElement(compParts[0]);

        // See if we need to add an attribute.
        if (compParts.length > 1) {
            // The name of the attribute is between the @ and the =.
            final int atLocation = compParts[1].indexOf('@');
            final int equalsLocation = compParts[1].indexOf('=');
            final String attrName = compParts[1].substring(atLocation + 1,
                    equalsLocation);
            // Now find the value.
            final int quoteLocOne = compParts[1].indexOf('=');
            final int quoteLocTwo = compParts[1].indexOf(']');
            final String attrValue = compParts[1].substring(quoteLocOne + 2,
                    quoteLocTwo - 1);
            // Refuse to create names that are null.
            if (attrValue.equals("null")) { //$NON-NLS-1$
                Logger.global.log(Level.WARNING, Messages
                        .getString("FileUtils.31")); //$NON-NLS-1$
                return null;
            }
            // Add the attribute.
            final Attr newAttr = aDocument.createAttribute(attrName);
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
    public static boolean serialize(final Document aDocument,
            final Container aContainer) {
    	// Check for a null document.
    	if(aDocument == null){
    		Logger.global.log(Level.WARNING, "Cannot serialize a null document.");
    		return false;
    	}

        
        // Create the serializer.
        DOMImplementation impl = aDocument.getImplementation();
        DOMImplementationLS implLS = (DOMImplementationLS) impl.getFeature("LS","3.0");
        LSSerializer writer = implLS.createLSSerializer();
        
        // Turn on pretty print for readable output.
        if(writer.getDomConfig().canSetParameter("format-pretty-print", "true")) {
            writer.getDomConfig().setParameter("format-pretty-print", "true");
        }
        else {
            Logger.global.log(Level.INFO, "DOM serializer does not support pretty-print");
        }
        
        // Mark that the file is no longer in need of a save so the
        // attribute doesn't show up in output.
        if(!aDocument.getDocumentElement().getAttribute("needs-save").equals("true")) {
            System.out.println("Saving an unmodified document. needs-save: " + aDocument.getDocumentElement().getAttribute("needs-save"));
        }
        aDocument.getDocumentElement().removeAttribute("needs-save"); //$NON-NLS-1$

        // Serialize the document into a string.
        final String docContent = writer.writeToString(aDocument);
        
        // Now attempt to write it to a file.
        try {
            final File outputFile = FileUtils.getDocumentFile(aDocument);
            assert (outputFile != null);
            final FileWriter fileWriter = new FileWriter(outputFile);
            fileWriter.write(docContent);
            fileWriter.close();
        } catch (IOException e) {
            // Unexpected error creating writing the file. Inform the user
            // and log the error.
            Logger.global.throwing("DOMUtils", "serialize", e); //$NON-NLS-1$ //$NON-NLS-2$
            final String errorMessage = Messages.getString("DOMUtils.22") //$NON-NLS-1$
                    + e.getMessage() + "."; //$NON-NLS-1$
            final String errorTitle = Messages.getString("DOMUtils.24"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aContainer, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
            return false;
        }
        // Add an attribute that signals that the document has been saved
        // successfully.
        aDocument.getDocumentElement().setAttribute("document-saved", "true");
        return true;
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
    static public Node getResultNodeFromQuery(final Document aDocument, final String aXPath) {
        // A query may be performed before the document is set, avoid attempting
        // the query.
        if (aDocument == null) {
            return null;
        }
        assert (aXPath != null);
        // Create the XPath evaluator.
        final XPathEvaluatorImpl xPathEvaluator = new XPathEvaluatorImpl(aDocument);

        final XPathResult result = executeQuery(aDocument, aXPath, xPathEvaluator);

        // Check for any results. Loop so that we always use the last node.
        Node resultNode = null;
        while (true) {
            final Node newResult = result.iterateNext();
            // If we already set this once we have multiple values matching the
            // XPath. This is not illegal but merits a warning.
            if(newResult != null && resultNode != null) {
                Logger.global.log(Level.INFO, Messages
                        .getString("DOMUtils.10") + aXPath); //$NON-NLS-1$
            }
            // The last result was blank so quit searching.
            if(newResult == null) {
                break;
            }
            resultNode = newResult;
        }
        return resultNode;
    }

    /**
     * Get the value of the name attribute of a specific node.
     * 
     * @param aNode
     *            Node to fetch the name attribute of.
     * @return The value of the name attribute for a node.
     */
    public static String getNameAttrValue(final Node aNode) {
        if (aNode == null) {
            return null;
        }
        // Get the name attribute from a node.
        final NamedNodeMap attrs = aNode.getAttributes();
        if (attrs == null) {
            return null;
        }
        final Node nameAttr = attrs.getNamedItem("name"); //$NON-NLS-1$
        if (nameAttr == null) {
            return null;
        }
        return nameAttr.getNodeValue();
    }

    /**
     * Set the value of the name attribute of a specific node.
     * @param aNode The node of which to modify the name attribute.
     * @param aNewValue The new value of the name attribute.
     */
    public static void setNameAttrValue(final Node aNode, final String aNewValue){
        // Add the attribute.
        final Attr newAttr = aNode.getOwnerDocument().createAttribute("name");
        newAttr.setNodeValue(aNewValue);
        aNode.getAttributes().setNamedItem(newAttr);
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
    static public int getDOMIndexOfObject(final Node aParent, final Object aObject) {
        // Check if the parent is null.
        if (aParent == null) {
            Logger.global.log(Level.WARNING,
                    "Cannot get the DOM index because parent node is null.");
            return -1;
        }

        if (!(aObject instanceof Node)) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.22")); //$NON-NLS-1$
            return -1;
        }

        // Get the name of the new object.
        final String newNodeName = getNameAttrValue((Node) aObject);
        final NodeList children = aParent.getChildNodes();

        for (int i = 0; i < children.getLength(); ++i) {
            final String name = getNameAttrValue(children.item(i));
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
    public static int getDOMIndexForListIndex(final Node aParentNode, final int aListIndex) {
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
        final NodeList children = aParentNode.getChildNodes();
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
    public static Element createElement(final Node aParent, final String aElementName,
            final Object aObject, final boolean aAddTextContent) {
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
        final Element newElement = aParent.getOwnerDocument().createElement(
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
    static public Node getItemBefore(final Node aParentNode, final Node aItem) {
        // Get the index of the node.
        final int domIndex = getDOMIndexOfObject(aParentNode, aItem);
    
        // Iterate backwards and search for a list item.
        final NodeList children = aParentNode.getChildNodes();
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
    static public Node getItemAfter(final Node aParentNode, final Node aItem) {
        // Get the index of the node.
        final int domIndex = getDOMIndexOfObject(aParentNode, aItem);
    
        // Iterate forwards and search for a list item.
        final NodeList children = aParentNode.getChildNodes();
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
    static public int getListIndexOfObject(final Node aParentNode, final Object aListItem) {
        // Check if this operation can be performed.
        if (aParentNode == null) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.23")); //$NON-NLS-1$
            return 0;
        }
    
        if (!(aListItem instanceof Node)) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("DOMListModel.24")); //$NON-NLS-1$
        }
    
        // Get the name of the new object.
        final String newNodeName = getNameAttrValue((Node) aListItem);
        final NodeList children = aParentNode.getChildNodes();
    
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
            final String name = getNameAttrValue(children.item(i));
            // Check if the child matches the requested element.
            if ((name != null) && name.equals(newNodeName)) {
                return listIndex;
            }
        }
        // The item was not found.
        return -1;
    }

    /**
     * Helper method which checks if the text content of a node is 1 or 0.
     * @param aNode Node of which to check the text value.
     * @return Whether the text node child of this node is 1.
     */
    public static boolean isTextContentTrue(final Node aNode) {
    	// Check if the node does not have a text value.
    	if (aNode.getTextContent() == null) {
    		Logger.global.log(Level.WARNING, "No text content for node.");
    		return false;
    	}
    	// Otherwise check the text value. Currently the node will contain a
    	// 1 or a 0, so this needs to be converted to a true false.
    	if (aNode.getTextContent().equals("1")) { //$NON-NLS-1$
    	    return true;
        }
    	if (aNode.getTextContent().equals("0")) { //$NON-NLS-1$
    		return false;
    	}
    	// Check for unknown values. Warn that these exist and return false.
    	Logger.global.log(Level.WARNING, "Unknown text value contained in node.");
    	return false;
    }
}
