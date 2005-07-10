package guicomponents;



import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;

import utils.Messages;

/**
 * A factory class which creates DOMFileListPanels which are initialized
 * to the correct parent node. The factory also tracks the the panels
 * and resets their content when the underlying document changes.
 * 
 * @author Josh Lurz
 *
 */
public class DOMListPanelFactory {
	/**
	 * Reference to the top level document.
	 */
	private Document mDocument = null;
	
	/**
	 * A mapping of category names to the related DOMFileListPanels created by this factory.
	 * This is needed to reset the lists when the underlying document changes.
	 *
	 */
	private Map<String,DOMListPanel> mChildListPanels = null;
	
	/**
	 * Constructor
	 */
	public DOMListPanelFactory() {
		mChildListPanels = new HashMap<String,DOMListPanel>();
	}
	
	/**
	 * Set the document which all DOMFileListPanels created by this factory
	 * will use to set their values into the DOM.
	 * @param aDocument The new document.
	 */
	public void setDocument(Document aDocument){
		mDocument = aDocument;
		// Iterate through any children text fields and reset their values.
		Iterator<String> currPath = mChildListPanels.keySet().iterator();
		while(currPath.hasNext()){
			DOMListPanel currList = mChildListPanels.get(currPath.next());
			currList.setDocument(aDocument);
		}
	}
	
	/**
	 * Factory method which creates text fields with initial values and
	 * action listeners to set the values back into the DOM. The factory
	 * also keeps a reference to the text fields so it can reinitialize them
	 * when the underlying document changes.
	 * @param aCategory The location of the set of value elements for this list.
     * @param aElementName The name of the elements of the list.
	 * @param aPanelLabel The label associated with this list.
	 * @param aChildrenAreLeaves Whether the elements of this list are leaves. Leaf lists
	 * will add new files as new items, node lists will add elements as new items.
	 * @return An initialized text field.
	 */
	public DOMListPanel createDOMFileListPanel(String aCategory, String aElementName,
                                                   String aPanelLabel, boolean aChildrenAreLeaves){
		// Check if the XPath already exists in the map. There can't be multiple
		// text fields mapping to the same node in the DOM.
		if(mChildListPanels.containsKey(aCategory)){
			Logger.global.log(Level.WARNING, Messages.getString("DOMListPanelFactory.0") + aCategory //$NON-NLS-1$
					+ Messages.getString("DOMListPanelFactory.1")); //$NON-NLS-1$
			return null;
		}
		
		// Create the text field and add it into the map.
		DOMListPanel newListPanel = new DOMListPanel(aCategory, aElementName, 
                                                             aPanelLabel, aChildrenAreLeaves);
		newListPanel.setDocument(mDocument);
		mChildListPanels.put(aCategory, newListPanel);
		return newListPanel;
	}
}
