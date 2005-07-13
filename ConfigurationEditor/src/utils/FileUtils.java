/*
 */
package utils;

import java.awt.Window;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import configurationeditor.ConfigurationEditor;
import configurationeditor.PropertiesInfo;

/**
 * Static utility class with helper functions.
 * @author Josh Lurz
 * 
 */
final public class FileUtils {
    /**
     * Private constructor to prevent the creation of the class.
     */
    private FileUtils(){
        super();
    }
    
	/**
	 * Check if a document is dirty or needs to be saved by checking 
	 * an attribute on the root element of the document.
	 * @param aDocument The document to check if it needs to be saved.
	 * @return Whether the document needs to be saved.
	 */
	static public boolean isDirty(final Document aDocument){
		return ( ( aDocument != null ) 
				&& aDocument.getDocumentElement().getAttribute("needs-save").length() > 0 ); //$NON-NLS-1$
	}
	
	/**
	 * Get the document name from the document as a File.
	 * @param aDocument The document of which to get the name.
	 * @return The file name of the document.
	 */
	static public File getDocumentFile(final Document aDocument){
        final String uri = (aDocument == null) ? null : aDocument.getDocumentURI();
        if(uri == null || uri.equals("")) { //$NON-NLS-1$
            return null;
        }
        // Strip off the file:\ the document added.
        final String newFileName = uri.replaceFirst("file:\\\\",""); //$NON-NLS-1$ //$NON-NLS-2$
		return new File( newFileName );
	}
	
	/**
	 * Set the path of a document onto it.
	 * @param aDocument The document to set the name onto.
	 * @param aFile The file to set as the path to the document.
	 */
	static public void setDocumentFile(final Document aDocument, final File aFile){
		aDocument.setDocumentURI(aFile == null ? "" : aFile.getAbsolutePath()); //$NON-NLS-1$
	}
	
	/**
	 * Get the selected files from a file chooser. This method handles multiple
	 * or single selected items correctly.
	 * @param aChooser The file chooser from which to get the selected files.
	 * @return The list of selected files.
	 */
	static public File[] getSelectedFiles(final JFileChooser aChooser){
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
	public static String getExtension(final File aFile) {
        final String fileName = aFile.getName();
        final int periodIndex = fileName.lastIndexOf('.');
		String extension = null;
		if (periodIndex > 0 && periodIndex < fileName.length() - 1) {
			extension = fileName.substring(periodIndex + 1).toLowerCase();
		}
		return extension;
	}

	/**
	 * Initialize a properties object with read in data.
	 * 
	 * @param aWindow A window over which to display error messages.
	 * @return An initialized properties object.
	 */
	public static Properties getInitializedProperties(final Window aWindow){
		// Get the executable path from the properties file.
        final Properties props = new Properties();
		try {
            final FileInputStream inputStream = new FileInputStream(PropertiesInfo.PROPERTY_FILE);
			props.loadFromXML(inputStream);
			inputStream.close();
		} catch (FileNotFoundException e) {
	        // The properties file does not exist yet, this 
			// is an error because it should have already been created.
            final String errorMessage = Messages.getString("RunAction.3"); //$NON-NLS-1$
            final String errorTitle = Messages.getString("RunAction.4"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(aWindow, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
		} catch (IOException e) {
            final String errorMessage = Messages.getString("RunAction.5") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
            final String errorTitle = Messages.getString("RunAction.7"); //$NON-NLS-1$
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
    public static void saveProperties(final Properties aProperties) {
        // Properties shouldn't be null.
        assert( aProperties != null);
        try {
            final FileOutputStream saveStream = new FileOutputStream(PropertiesInfo.PROPERTY_FILE);
            aProperties.storeToXML( saveStream, Messages.getString("ShowPreferencesAction.2") ); //$NON-NLS-1$
            saveStream.close();
        } catch(final Exception aException) {
            // TODO: Error dialog.
            Logger.global.throwing("saveProperties", "DOMUtils", aException);
        }
    }
    
	/**
	 * Ask the user if they would like to perform a save, save the file if they
	 * respond affirmatively.
	 * TODO: Make this more generic.
	 * @param aEditor
	 *            The editor
     *
	 * @return Whether the user wants to continue with the action which
	 *         initiated this request.
	 */
	public static boolean askForSave(final ConfigurationEditor aEditor) {
		if (isDirty(aEditor.getDocument())) {
			final String message = Messages.getString("FileUtils.25"); //$NON-NLS-1$
            final int returnValue = JOptionPane.showConfirmDialog(aEditor, message,
					Messages.getString("FileUtils.26"), JOptionPane.YES_NO_CANCEL_OPTION); //$NON-NLS-1$
			if (returnValue == JOptionPane.YES_OPTION) {
				(aEditor).dispatchSave();
			} else if (returnValue == JOptionPane.CANCEL_OPTION) {
                // They closed the dialog, they want to continue.
                return false;
            }
		}
		return true;
	}
}