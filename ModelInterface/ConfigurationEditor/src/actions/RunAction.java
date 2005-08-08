/**
 * 
 */
package actions;



import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;

import configurationeditor.ConfigurationEditor;
import configurationeditor.ModelRunner;
import configurationeditor.PropertiesInfo;

/**
 * This action is called when a user tries to run the model. It saves the configuration
 * to a temporary location and dispatches a new ModelRunner thread to run the model. 
 * Errors are displayed to the user in dialogs.
 * @author Josh Lurz
 */
public class RunAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = -3368634278919104142L;

    /**
     * The temporary file to write the document to before the executable is run.
     */
    private static final String mTempConfFile = "configuration_temp.xml"; //$NON-NLS-1$

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private final transient ConfigurationEditor mParentEditor;
    
    /**
     * Whether an instance of the model is currently running.
     */
    private transient boolean mModelRunning = false;
    
    /**
     * Constructor which sets the name of the Action and stores the parent editor.
     * @param aParentEditor
     *            The top level editor.
     */
    public RunAction(ConfigurationEditor aParentEditor) {
        super("Run"); //$NON-NLS-1$
        mParentEditor = aParentEditor;
    }

    /**
     * The method triggered by the user sending a run event through the 
     * menu or the run button. 
     * 
     * Runs the model by attemption to read the properties to find the
     * location of the executable, saving the current configuration to 
     * a temporary location and calling the executable.
     * 
     * @param aEvent Event that triggered this action.
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(final ActionEvent aEvent) {
    	// First check if an instance of the model is already running.
    	if(isModelRunning()){
    		// Warn the user that this isn't possible to do and return.
    		final String errorMessage = Messages.getString("RunAction.7"); //$NON-NLS-1$
    		final String errorTitle = Messages.getString("RunAction.6"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
    	}
        // Get the executable path from the properties file.
        final Properties props = FileUtils.getInitializedProperties(mParentEditor);
        
        // Get the path to the executable.
        final String executableFile = props.getProperty(PropertiesInfo.EXE_PATH);
        
        // Check if the executable path has been initialized.
        if( executableFile == null) {
            final String errorMessage = Messages.getString("RunAction.8"); //$NON-NLS-1$
            final String errorTitle = Messages.getString("RunAction.9"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }

        // Check if the executable path points to a valid location.
        final File executable = new File(executableFile);
        if( !executable.exists()) {
            final String errorMessage = Messages.getString("RunAction.10"); //$NON-NLS-1$
            final String errorTitle = Messages.getString("RunAction.11"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }
        assert(mParentEditor.getDocument() != null);
        
        // Clone the document before serializing to a different name to 
        // preserve all state.
        // Create a new document.
        DOMImplementation domImpl = null;
        try {
            domImpl = DocumentBuilderFactory.newInstance().newDocumentBuilder().getDOMImplementation();
        } catch(ParserConfigurationException e) {
            Logger.global.log(Level.SEVERE, "Failed to create DOM implementation necessary to create the document.");
            return;
        }
        final Document clonedDoc = domImpl.createDocument(null, null, null);
        
        // Perform a deep clone on the document which will copy all nodes
        // and their children.
        final Node clonedRoot = (mParentEditor.getDocument().getDocumentElement().cloneNode(true));
        
        // Move the cloned node and its children from the configuration document 
        // which created it to the cloned document.
        clonedDoc.adoptNode(clonedRoot);
        
        // Set the cloned tree as the root element.
        clonedDoc.appendChild(clonedRoot);
        
        // Serialize the cloend document to a temporary location.
        final File tempConfFile = new File(mTempConfFile);
        FileUtils.setDocumentFile(clonedDoc, tempConfFile);
        
        DOMUtils.serialize(clonedDoc, mParentEditor);
        
        // Create a new thread to run the process on which will handle updating 
        // a dialog box containing the output of the model.
        final Thread runnerThread = new Thread(new ModelRunner( executable, tempConfFile, mParentEditor, this));
        runnerThread.start();
    }
    
    /**
     * Get whether the model is running. This method should be used instead of accessing the local
     * variable to protect against race conditions.
     * @return Whether the model is running.
     */
    public boolean isModelRunning(){
        synchronized(this) {
            return mModelRunning;
        }
    }
    
    /**
     * Set that the model is either running or not running.
     * @param aIsRunning Whether the model is running.
     */
    public void setModelRunning(final boolean aIsRunning){
        Logger.global.log(Level.INFO, "Setting model running to:" + aIsRunning);
        synchronized(this) {
            mModelRunning = aIsRunning;
        }
    }
}
