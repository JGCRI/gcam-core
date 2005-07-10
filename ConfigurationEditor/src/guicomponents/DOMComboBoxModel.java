/**
 * 
 */
package guicomponents;


import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractListModel;
import javax.swing.ComboBoxModel;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import utils.NodeWrapper;
import utils.Util;

/**
 * A combo box model which has as its list of values the element node children
 * of a single element node. TODO: There is a lot of code shared with the
 * DOMListModel.
 * 
 * @author Josh Lurz
 * 
 */
public class DOMComboBoxModel extends AbstractListModel implements
        ComboBoxModel {
    /**
     * Unique identifier used for serialization.
     */
    private static final long serialVersionUID = 4829102744109149915L;

    /**
     * The underlying document.
     */
    private Document mDocument = null;

    /**
     * The root node of the combo box.
     */

    private Node mRootNode = null;

    /**
     * The stored selected item.
     */
    private Object mSelectedItem = null;

    /**
     * The XPath of the parent of this combo box.
     */
    private String mParentXPath = null;

    /**
     * The name of the element for this combo box.
     */
    private String mElementName = null;

    /**
     * The name of this item.
     */
    private String mItemName = null;

    /**
     * Constructor.
     * 
     * @param aDocument
     *            The document containing the contained node of this model.
     * @param aParentXPath
     *            The XPath of the parent element of this node.
     * @param aElementName
     *            The element node name.
     * @param aItemName
     *            The name of this specific combo box as stored on the name
     *            attribute.
     */
    public DOMComboBoxModel(Document aDocument, String aParentXPath,
            String aElementName, String aItemName) {
        super();
        Logger.global.log(Level.INFO, "Creating DOM combo box model.");
        mDocument = aDocument;
        mParentXPath = aParentXPath;
        mElementName = aElementName;
        mItemName = aItemName;

        // Set the root node now that all information is known.
        mRootNode = getRootNode();
    }

    /**
     * Queries the list for an element at a given position. The element at this
     * position may not reside at the same position in the DOM due to comment
     * nodes.
     * 
     * @see javax.swing.ListModel#getElementAt(int)
     * @param aPosition
     *            in the list to return the element at.
     */
    public Object getElementAt(int aPosition) {
        if (!canAccessDOM()) {
            Logger.global.log(Level.WARNING,
                    "Get element at failed because DOM is inaccessable.");
            return null;
        }
        if (aPosition < 0) {
            Logger.global.log(Level.WARNING,
                    "Get element at failed because position is invalid.");
            return null;
        }

        if (mRootNode == null) {
            Logger.global.log(Level.WARNING, "Root node of combo box is null.");
            return null;
        }
        // Find the actual position ignoring non element nodes.
        NodeList children = mRootNode.getChildNodes();
        if (children != null) {
            int trueNumber = -1;
            for (int i = 0; i < children.getLength(); ++i) {
                // Increment the actual child count if this location
                // contains an element node.
                if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                    ++trueNumber;
                }

                // Check if the actual position is the desired position. This
                // will only be true for element nodes given that trueNumber is
                // initialized to negative 1.
                if (trueNumber == aPosition) {
                    // Return a wrapped DOM node which overrides the toString
                    // method so it reports the correct value to the list.
                    return NodeWrapper.createProxy(children.item(i));
                }
            }
        }
        Logger.global
                .log(
                        Level.WARNING,
                        "getElement failed because the requested position was greater than the number of elements.");
        return null;
    }

    /**
     * Get the size of the list.
     * 
     * @return The size of the list.
     * @see javax.swing.ListModel#getSize()
     */
    public int getSize() {
        if (!canAccessDOM()) {
            Logger.global.log(Level.INFO,
                    "Returning size of zero because the DOM is inaccessable.");
            return 0;
        }

        // Count the child nodes which are elements.
        int size = 0;
        NodeList childNodes = mRootNode.getChildNodes();
        for (int i = 0; i < childNodes.getLength(); ++i) {
            if (childNodes.item(i).getNodeType() == Node.ELEMENT_NODE) {
                ++size;
            }
        }
        return size;
    }

    /**
     * Store the currently selected item.
     * 
     * @param aSelectedItem
     *            The item selected by the user.
     */
    public void setSelectedItem(Object aSelectedItem) {
        Logger.global.log(Level.INFO, "setSelectedItem");
        mSelectedItem = aSelectedItem;
        
        // Call the default list models set selected item
        // to notify listeners.
        int changedIndex = getListIndex(aSelectedItem);
        fireContentsChanged(this, changedIndex, changedIndex );
    }

    /**
     * Get the list index of the specific object. This is the position
     * in the combo-box, not in the DOM.
     * @param aObject The object for which to search the DOM.
     * @return The list index of the object.
     */
    public int getListIndex(Object aObject) {
        if (!canAccessDOM()) {
            Logger.global.log(Level.WARNING,
                    "Get list index failed because DOM is inaccessable.");
            return -1;
        }
        if (aObject == null) {
            Logger.global.log(Level.WARNING,
                    "Get list index failed because object is null.");
            return -1;
        }

        if (mRootNode == null) {
            Logger.global.log(Level.WARNING, "Root node of combo box is null.");
            return -1;
        }
        // Find the actual position ignoring non element nodes.
        NodeList children = mRootNode.getChildNodes();
        if (children != null) {
            int trueNumber = -1;
            for (int i = 0; i < children.getLength(); ++i) {
                // Increment the actual child count if this location
                // contains an element node.
                if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                    ++trueNumber;
                }
                else {
                    continue;
                }
                // Check if the node is equal to the requested node.
                if(aObject.toString() == Util.getNameAttrValue(children.item(i))){
                    return trueNumber;
                }
            }
        }
        Logger.global
                .log(
                        Level.WARNING,
                        "getElement failed because the requested position was greater than the number of elements.");
        return -1;
    }
    /**
     * Get the currently selected item.
     * 
     * @return The stored selected item.
     */
    public Object getSelectedItem() {
        Logger.global.log(Level.INFO, "getSelectedItem");
        return mSelectedItem;
    }

    /**
     * Returns whether the DOM is currently accessable.
     * 
     * @return Whether the DOM is accessable.
     */
    boolean canAccessDOM() {
        return (mDocument != null && mRootNode != null);
    }

    /**
     * Return the root of this combo box in the DOM tree.
     * 
     * @return The root node.
     */
    Node getRootNode() {
        Node rootNode = Util.getResultNodeFromQuery(mDocument, getXPath());
        // Create the root node if there is not one present in the DOM.
        if (rootNode == null) {
            Logger.global.log(Level.INFO,
                    "Creating new root node for combo box model with XPath: "
                            + getXPath());
            rootNode = Util.addNodesForXPath(mDocument, getXPath());
            // Check for failure adding nodes to the tree.
            if (rootNode == null) {
                Logger.global.log(Level.WARNING,
                        "Failed to create a root node for combo box.");
            }
        }
        return rootNode;
    }

    /**
     * Function to return the XPath for the root node containing the information
     * for this combobox.
     */
    private String getXPath() {
        String XPath = "";
        if (mParentXPath != null) {
            XPath += mParentXPath;
        }
        XPath += "/" + mElementName;
        if (mItemName != null) {
            XPath += "[@name='" + mItemName + "']"; //$NON-NLS-1$ //$NON-NLS-2$
        }
        return XPath;
    }
}
