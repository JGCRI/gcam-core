/**
 * 
 */
package guicomponents;


import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JComboBox;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import utils.Util;

/**
 * A class which creates combo boxes which update their values from a node in a
 * DOM tree. The factory creates the combo boxes with the correct values, and
 * adds listeners to them to update the DOM when their values are changed.
 * 
 * @authod Josh Lurz
 */
public class SingleDOMValueComboBoxFactory {
    /**
     * The DOM document which contains the information pertaining to the combo
     * box.
     */
    private Document mDocument = null;

    /**
     * The root element name for the document.
     */
    private String mRootElementName = null;

    /**
     * The element name for the parent nodes of the combo boxes.
     */
    private String mParentNodeName = null;

    /**
     * The name of the current parent element node.
     */
    private String mCurrentParentName = null;

    /**
     * Mapping of combo box name to combo box.
     */
    Map<String, JComboBox> mComboBoxes = null;

    /**
     * Constructor
     * 
     * @param aDocument
     *            Initial document.
     * @param aRootElementName
     *            Root element name.
     * @param aParentNodeName
     *            The element name for the parent nodes.
     */
    public SingleDOMValueComboBoxFactory(Document aDocument,
            String aRootElementName, String aParentNodeName) {
        mDocument = aDocument;
        mRootElementName = aRootElementName;
        mParentNodeName = aParentNodeName;
        mComboBoxes = new HashMap<String, JComboBox>();
    }

    /**
     * Set a new parent node name for all combo boxes created by this factory.
     * This will update all the combo box values.
     * 
     * @param aNewNodeName
     *            The name of the new parent node.
     */
    public void setParentName(String aNewNodeName) {
        mCurrentParentName = aNewNodeName;
        // Iterate through any children combo boxes and reset their values.
        Iterator<String> currChild = mComboBoxes.keySet().iterator();
        while (currChild.hasNext()) {
            final String currChildName = currChild.next();
            final String xPath = createXPath(currChildName);

            // Query the DOM for a value for this XPath.
            Node result = Util.getResultNodeFromQuery(mDocument, xPath);
            int newValue = -1;
            if (result != null && result.getTextContent() != null) {
                newValue = Integer.parseInt(result.getTextContent());
            }
            // Set the value into the current combo box. If there was not a
            // value returned from the DOM this will set null into the field
            // which needs to occur to erase an old value.
            JComboBox currComboBox = mComboBoxes.get(currChildName);
            currComboBox.setSelectedIndex(newValue);
        }
    }

    /**
     * Method to construct a new combo box.
     * 
     * @param aValues
     *            An ordered list of strings which will be used as values.
     * @param aItemName
     *            The name of the item's corresponding node.
     * @return A newly created JComboBox.
     */
    public JComboBox createComboBox(String[] aValues, String aItemName) {
        // Check if the name already exists in the map. There
        // can't be duplicate elements.
        if (mComboBoxes.containsKey(aItemName)) {
            Logger.global.log(Level.WARNING,
                    "Cannot create duplicate combo box: " + aItemName);
            return null;
        }

        // Create the combo box with the values and add it to the map.
        JComboBox newComboBox = new JComboBox(aValues);
        // Don't initialize it now, this can't happen until the parent
        // node is known.

        // Add an action listener to the combo-box which will update
        // the field when the focus changes.
        newComboBox.addItemListener(new ComboBoxItemListener(aItemName));
        return newComboBox;
    }

    /**
     * Helper method to create an XPath for the item.
     * 
     * @param aItemName
     *            The name of the item for which to create an XPath.
     * @return The XPath to this item.
     */
    private String createXPath(String aItemName) {
        return "/" + mRootElementName + "/" + mParentNodeName + "[@name='"
                + mCurrentParentName + "']/" + aItemName;
    }

    /**
     * An internal focus listener class which updates the node in the DOM
     * corresponding to this combo box when the user removes focus from the text
     * field. The DOM is only updated when the value is changed to limit DOM
     * mutation events.
     * 
     * @author Josh Lurz
     */
    private final class ComboBoxItemListener implements ItemListener {
        /**
         * The name of the node in the DOM.
         */
        private String mItemName = null;

        /**
         * Constructor which sets the XPath.
         * 
         * @param aItemName
         *            The name of the node in the DOM.
         */
        public ComboBoxItemListener(String aItemName) {
            mItemName = aItemName;
        }

        /**
         * Method called when an item is selected in the combo box. This will
         * save the value in the DOM if the value is different than the previous
         * value.
         * 
         * @param aEvent
         *            The item event.
         */
        public void itemStateChanged(ItemEvent aEvent) {
            System.out.println("ITEM STATE CHANGED! " + aEvent.toString());
            // Check for the invalid condition when there isn't a document.
            if (mDocument == null) {
                Logger.global.log(Level.WARNING,
                        "Item state changed without an underlying document.");
                return;
            }

            // Perform the query.
            final String xPath = createXPath(mItemName);
            Node resultNode = Util.getResultNodeFromQuery(mDocument, xPath);
            int newIndex = ((JComboBox) aEvent.getSource()).getSelectedIndex();
            // If the node is null it means that there were no results. Don't
            // create a new
            // node if the value which will be set in is blank. This avoid
            // adding unnecessary
            // nodes to the DOM.
            if (resultNode == null && newIndex != -1) {
                Logger.global.log(Level.INFO, xPath);
                // Create a position in the DOM tree to store the value.
                resultNode = Util.addNodesForXPath(mDocument, xPath);
            }
            // Check if the text was unchanged. This prevents the DOM from
            // being modified unneccessarily.
            if ((resultNode != null)
                    && (Integer.parseInt(resultNode.getTextContent()) != newIndex)) {
                // Store the combo box index as the value of the node.
                resultNode.setTextContent(Integer.toString(newIndex));
            }
        }
    }
}
