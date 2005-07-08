/*
 */
package interfaceutils;

import java.awt.Container;
import java.awt.Window;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xerces.dom3.bootstrap.DOMImplementationRegistry;
import org.apache.xpath.domapi.XPathEvaluatorImpl;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;
import org.w3c.dom.xpath.XPathResult;

import actions.Messages;

import configurationeditor.ConfigurationEditor;
import configurationeditor.PropertiesInfo;

/**
 * Static utility class with helper functions.
 * @author Josh Lurz
 * 
 */
final public class Util {
	/**
	 * Check if a document is dirty or needs to be saved by checking 
	 * an attribute on the root element of the document.
	 * @param aDocument The document to check if it needs to be saved.
	 */
	static public boolean isDirty(Document aDocument){
		return ( ( aDocument != null ) 
				&& aDocument.getDocumentElement().getAttribute("needs-save").length() > 0 ); //$NON-NLS-1$
	}
	
	/**
	 * Get the document name from the document as a File.
	 * @param aDocument The document of which to get the name.
	 */
	static public File getDocumentFile(Document aDocument){
        String uri = (aDocument != null) ? aDocument.getDocumentURI() : null;
        if(uri == null || uri.equals("")) { //$NON-NLS-1$
            return null;
        }
        // Strip off the file:\ the document added.
        String newFileName = uri.replaceFirst("file:\\\\",""); //$NON-NLS-1$ //$NON-NLS-2$
		return new File( newFileName );
	}
	
	/**
	 * Set the path of a document onto it.
	 * @param aDocument The document to set the name onto.
	 * @param aFile The file to set as the path to the document.
	 */
	static public void setDocumentFile(Document aDocument, File aFile){
		aDocument.setDocumentURI(aFile != null ? aFile.getAbsolutePath() : Messages.getString("Util.4")); //$NON-NLS-1$
	}
	
	/**
	 * Get the selected files from a file chooser. This method handles multiple
	 * or single selected items correctly.
	 * @param aChooser The file chooser from which to get the selected files.
	 */
	static public File[] getSelectedFiles(JFileChooser aChooser){
		// First try to get multiple items. If there is only one
		// item selected this will return an empty array.
		File[] selectedItems = aChooser.getSelectedFiles();
		if (selectedItems.length == 0) {
			selectedItems = new File[1];
        	selectedItems[ 0 ] = aChooser.getSelectedFile();
		}
		return selectedItems;
    }
	
	/**
	 * Get the file extension string.
	 * 
	 * @param aFile
	 *            File name
	 * @return The file extension.
	 */
	public static String getExtension(File aFile) {
		String s = aFile.getName();
		int i = s.lastIndexOf('.');
		String extension = null;
		if (i > 0 && i < s.length() - 1) {
			extension = s.substring(i + 1).toLowerCase();
		}
		return extension;
	}

	/**
	 * Get the value of the name attribute of a specific node.
	 * 
	 * @param aNode
	 *            Node to fetch the name attribute of.
	 * @return The value of the name attribute for a node.
	 */
	public static String getNameAttrValue(Node aNode) {
	    if(aNode == null) {
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
	 * Initialize a properties object with read in data.
	 * 
	 * @param aWindow A window over which to display error messages.
	 * @return An initialized properties object.
	 */
	public static Properties getInitializedProperties(Window aWindow){
		// Get the executable path from the properties file.
		Properties props = new Properties();
		try {
			FileInputStream inputStream = new FileInputStream(PropertiesInfo.PROPERTY_FILE);
			props.loadFromXML(inputStream);
			inputStream.close();
		} catch (FileNotFoundException e) {
	        // The properties file does not exist yet, this 
			// is an error because it should have already been created.
            String errorMessage = Messages.getString("RunAction.3"); //$NON-NLS-1$
            String errorTitle = Messages.getString("RunAction.4"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aWindow, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
		} catch (IOException e) {
			String errorMessage = Messages.getString("RunAction.5") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
			String errorTitle = Messages.getString("RunAction.7"); //$NON-NLS-1$
			JOptionPane.showMessageDialog(aWindow, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
			// This is an unexpected error, log the error.
			Logger.global.log(Level.SEVERE, e.getStackTrace().toString());
		}
    return props;
	}
    
    /**
     * Save the properties to the file.
     * @param aProperties The properties object to save.
     */
    public static void saveProperties(Properties aProperties) {
        // Properties shouldn't be null.
        assert( aProperties != null);
        try {
            FileOutputStream saveStream = new FileOutputStream(PropertiesInfo.PROPERTY_FILE);
            aProperties.storeToXML( saveStream, Messages.getString("ShowPreferencesAction.2") ); //$NON-NLS-1$
            saveStream.close();
        } catch( Exception e ) {
            e.printStackTrace();
        }
    }
    
	/**
	 * Get an initialized document builder.
	 * @param aWindow A window used to center error messages, allowed to be null.
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
			String errorMessage = Messages.getString("Util.7") //$NON-NLS-1$
					+ e.getMessage() + "."; //$NON-NLS-1$
			String errorTitle = Messages.getString("Util.9"); //$NON-NLS-1$
			JOptionPane.showMessageDialog(aWindow, errorMessage,
					errorTitle, JOptionPane.ERROR_MESSAGE);
		}
		return docBuilder;
	}
	
	/** 
	 * Helper method to perform an XPath query on the Document and return
	 * a resulting node.
	 * @param aDocument Document on which to perform the query.
	 * @param aXPath Query to perform on the document.
	 * @return The node which satisfies the query. If none are found this
	 * will be null, if multiple are found this will return the last.
	 */
	static public Node getResultNodeFromQuery(Document aDocument, String aXPath ){
		// A query may be performed before the document is set, avoid attempting the query.
		if(aDocument == null ){
			return null;
		}
		assert(aXPath != null);
		// Create the XPath evaluator.
		XPathEvaluatorImpl xPathEvaluator = new XPathEvaluatorImpl(aDocument);

		// Perform an XPath query on the tree. If this proves too slow
		// we could do the search initially and store the node.
		XPathResult result = (XPathResult) xPathEvaluator.createExpression(
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
			Logger.global.log(Level.INFO, Messages.getString("Util.10") + aXPath); //$NON-NLS-1$
		}
		valueSet = true;
	}
	return resultNode;
	}
	
	/**
	 * Write an XML document to a file.
	 * @param aDocument
	 *            The document to serialize.
	 * @return Whether the document was successfully serialized.
	 * @param aContainer
	 *            A container to use as the parent frame for printing error
	 *            dialogs.
	 */
	public static boolean serializeDocument(Document aDocument, Container aContainer) {
        assert(aDocument != null);
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
			String errorMessage = Messages.getString("Util.12") //$NON-NLS-1$
					+ e.getMessage() + "."; //$NON-NLS-1$
			String errorTitle = Messages.getString("Util.14"); //$NON-NLS-1$
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
			String errorMessage = Messages.getString("Util.16") //$NON-NLS-1$
					+ e.getMessage() + "."; //$NON-NLS-1$
			String errorTitle = Messages.getString("Util.18"); //$NON-NLS-1$
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
            File outputFile = getDocumentFile(aDocument);
            assert(outputFile != null);
			FileWriter fileWriter = new FileWriter(outputFile);
			fileWriter.write(serializedDocument);
			fileWriter.close();
			return true;
		} catch (IOException e) {
			// Unexpected error creating writing the file. Inform the user
			// and log the error.
            e.printStackTrace();
			Logger.global.throwing("Util", "serializeDocument", e); //$NON-NLS-1$ //$NON-NLS-2$
			String errorMessage = Messages.getString("Util.22") //$NON-NLS-1$
					+ e.getMessage() + "."; //$NON-NLS-1$
			String errorTitle = Messages.getString("Util.24"); //$NON-NLS-1$
			JOptionPane.showMessageDialog(aContainer, errorMessage, errorTitle,
					JOptionPane.ERROR_MESSAGE);
			return false;
		}
	}

	/**
	 * Ask the user if they would like to perform a save, save the file if they
	 * respond affirmatively.
	 * TODO: Make this more generic.
	 * @param aEditor
	 *            The editor
	 * @return Whether the user wants to continue with the action which
	 *         initiated this request.
	 */
	public static boolean askForSave(ConfigurationEditor aEditor) {
		if (isDirty(aEditor.getDocument())) {
			final String message = Messages.getString("Util.25"); //$NON-NLS-1$
			int rv = JOptionPane.showConfirmDialog(aEditor, message,
					Messages.getString("Util.26"), JOptionPane.YES_NO_CANCEL_OPTION); //$NON-NLS-1$
			if (rv == JOptionPane.YES_OPTION) {
				(aEditor).dispatchSave();
			} else if (rv == JOptionPane.NO_OPTION) {
				// They don't want to save, but continue the action.
			} else {
				// They closed the dialog, they want to continue.
				return false;
			}
		}
		return true;
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
			Node newNode = constructNode(aDocument, comps[i]);
            // If the new node is null return immediately.
            if(newNode == null) {
                return null;
            }
            // Skip the root node. TODO: Improve this hack.
			if(newNode.getNodeName().equals(aDocument.getDocumentElement().getNodeName())) {
			    continue;
            }
			// Search for the node in the list of children of the current node.
			NodeList children = current.getChildNodes();
			for (int j = 0; j < children.getLength(); ++j) {
                Node currChild = children.item(j);
                // Skip non-element nodes.
                if(currChild.getNodeType() != Node.ELEMENT_NODE) {
                    continue;
                }
                // Check if the node names match.
				if(currChild.getNodeName().equals(newNode.getNodeName())) {
                   // Check if the attributes match.
                    String currChildNameAttr = Util.getNameAttrValue(currChild);
                    String newNodeNameAttr = Util.getNameAttrValue(newNode);
                    // Check if both have null names or the names match.
                    if(((currChildNameAttr == null) && (newNodeNameAttr == null))
                    || ((currChildNameAttr != null) && 
                            currChildNameAttr.equals(newNodeNameAttr))){
                        current = currChild;
                        foundNode = true;
                        break;
                    }
				}
			}
			// If the node wasn't found we need to add it.
			if(!foundNode){
				current.appendChild(newNode);
				current = newNode;
			}
		}
		// Return the last node in the path.
		return current;
	}
	
	/**
	 * Construct a node from a single part of an XPath string.
	 * @param aDocument A document to use to construct the node.
	 * @param aPartialPath A string containing a section of an XPath query representing a single node.
	 * @return A newly constructed node.
	 */
	static Node constructNode(Document aDocument, String aPartialPath){
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
            if(attrValue.equals("null")) { //$NON-NLS-1$
                Logger.global.log(Level.WARNING, Messages.getString("Util.31")); //$NON-NLS-1$
                return null;
            }
			// Add the attribute.
			Attr newAttr = aDocument.createAttribute(attrName);
			newAttr.setNodeValue(attrValue);
			newNode.getAttributes().setNamedItem(newAttr);
		}
		return newNode;
	}
	

}