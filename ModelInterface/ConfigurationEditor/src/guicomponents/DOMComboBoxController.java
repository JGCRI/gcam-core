/**
 * 
 */
package ModelInterface.ConfigurationEditor.src.guicomponents;

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

import ModelInterface.ConfigurationEditor.src.utils.DOMUtils;

/**
 * A class which creates combo boxes which update their values from a node in a
 * DOM tree. The factory creates the combo boxes with the correct values, and
 * adds listeners to them to update the DOM when their values are changed.
 * 
 * @author Josh Lurz
 */
public class DOMComboBoxController implements ItemListener {
	/**
	 * The DOM document which contains the information pertaining to the combo
	 * box.
	 */
	private transient Document mDocument = null;

	/**
	 * The root element name for the document.
	 */
	private transient String mRootElementName = null;

	/**
	 * The element name for the parent nodes of the combo boxes.
	 */
	private transient String mParentNodeName = null;

	/**
	 * The name of the current parent element node.
	 */
	private transient String mCurrParentName = null;

	/**
	 * Mapping of combo box name to combo box.
	 */
	private transient Map<String, JComboBox> mComboBoxes = null;

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
	public DOMComboBoxController(Document aDocument, String aRootElementName,
			String aParentNodeName) {
		super();
		mDocument = aDocument;
		mRootElementName = aRootElementName;
		mParentNodeName = aParentNodeName;
		mComboBoxes = new HashMap<String, JComboBox>();
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
	public JComboBox createComboBox(final String[] aValues,
			final String aItemName) {
		// Check if the name already exists in the map. There
		// can't be duplicate elements.
		if (mComboBoxes.containsKey(aItemName)) {
			Logger.global.log(Level.WARNING,
					"Cannot create duplicate combo box: " + aItemName);
			return null;
		}

		// Create the combo box with the values and add it to the map.
		final JComboBox newComboBox = new JComboBox(aValues);
		// Don't initialize it now, this can't happen until the parent
		// node is known.
		// Add the combo box to those controlled.
		mComboBoxes.put(aItemName, newComboBox);

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
	private String createXPath(final String aItemName) {
		return "/" + mRootElementName + "/" + mParentNodeName + "[@name='"
				+ mCurrParentName + "']/" + aItemName;
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
		private transient final String mItemName;

		/**
		 * Constructor which sets the XPath.
		 * 
		 * @param aItemName
		 *            The name of the node in the DOM.
		 */
		public ComboBoxItemListener(String aItemName) {
			super();
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
		public void itemStateChanged(final ItemEvent aEvent) {
			// Ignore item unselected events, they should be 
			// paired with a selected event.
			if (aEvent.getStateChange() != ItemEvent.SELECTED) {
				return;
			}
			// Check for the invalid condition when there isn't a document.
			if (mDocument == null) {
				Logger.global.log(Level.WARNING,
						"Item state changed without an underlying document.");
				return;
			}

			// Perform the query.
			final String xPath = createXPath(mItemName);
			Node resultNode = DOMUtils.getResultNodeFromQuery(mDocument, xPath);
			final int newIndex = ((JComboBox) aEvent.getSource())
					.getSelectedIndex();
			// If the node is null it means that there were no results.
			// Don't create a new node if the value which will be set in is
			// blank. This avoid adding unnecessary nodes to the DOM.
			if (resultNode == null && newIndex != -1) {
				Logger.global.log(Level.INFO, xPath);
				// Create a position in the DOM tree to store the value.
				resultNode = DOMUtils.addNodesForXPath(mDocument, xPath);
			}
			// Check if the text was unchanged. This prevents the DOM from
			// being modified unneccessarily.
			if (resultNode != null) {
				boolean isSameValue = false;
				try {
					isSameValue = (Integer
							.parseInt(resultNode.getTextContent()) == newIndex);
				} catch (NumberFormatException e) {
					Logger.global
							.log(Level.WARNING,
									"Number format caught while setting DOM value for combo box.");
					// The value in the DOM wasn't a number which is
					// incorrect, so allow it to be reset.
				}
				if (!isSameValue) {
					// Store the combo box index as the value of the node.
					resultNode.setTextContent(Integer.toString(newIndex));
				}
			}
		}
	}

	/**
	 * Method called when the parent combo box's item state is changed.
	 * 
	 * @param aEvent
	 *            The event received.
	 * @see java.awt.event.ItemListener#itemStateChanged(java.awt.event.ItemEvent)
	 */
	public void itemStateChanged(final ItemEvent aEvent) {
		// Only pay attention to selection events.
		if (aEvent.getStateChange() == ItemEvent.SELECTED) {
			mCurrParentName = aEvent.getItem().toString();
			// Iterate through any children combo boxes and reset their values.
			final Iterator<String> currChild = mComboBoxes.keySet().iterator();
			while (currChild.hasNext()) {
				final String currChildName = currChild.next();
				final JComboBox currComboBox = mComboBoxes.get(currChildName);
				// If the current parent is null disable the combo box.
				if (mCurrParentName == null) {
					currComboBox.setSelectedIndex(-1);
					currComboBox.setEnabled(false);
				}
				// Otherwise enable the combo box and set its value.
				else {
					currComboBox.setEnabled(true);
					final String xPath = createXPath(currChildName);

					// Query the DOM for a value for this XPath.
					final Node result = DOMUtils.getResultNodeFromQuery(
							mDocument, xPath);
					int newValue = -1;
					if (result != null && result.getTextContent() != null) {
						newValue = Integer.parseInt(result.getTextContent());
					}
					// Check for invalid values.
					if (newValue >= currComboBox.getModel().getSize()) {
						Logger.global.log(Level.WARNING,
								"Resetting invalid combo box value.");
						newValue = currComboBox.getModel().getSize() - 1;
					}
					// Set the value into the current combo box. If there was
					// not a value returned from the DOM this will set null into
					// the field which needs to occur to erase an old value.
					currComboBox.setSelectedIndex(newValue);
				}
			}
		}
	}
}
