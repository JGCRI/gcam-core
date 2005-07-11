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

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;

import configurationeditor.ConfigurationEditor;
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
    private final String mTempConfigurationFile = "configuration_temp.xml"; //$NON-NLS-1$

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private ConfigurationEditor mParentEditor = null;
    
    /**
     * Whether an instance of the model is currently running.
     */
    private boolean mModelRunning = false;
    
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
    public void actionPerformed(ActionEvent aEvent) {
    	// First check if an instance of the model is already running.
    	if(isModelRunning()){
    		// Warn the user that this isn't possible to do and return.
    		String errorMessage = Messages.getString("RunAction.7"); //$NON-NLS-1$
    		String errorTitle = Messages.getString("RunAction.6"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
    	}
        // Get the executable path from the properties file.
        Properties props = FileUtils.getInitializedProperties(mParentEditor);
        
        // Get the path to the executable.
        String executableFile = props.getProperty(PropertiesInfo.EXE_PATH_PROPERTY);
        
        // Check if the executable path has been initialized.
        if( executableFile == null) {
            String errorMessage = Messages.getString("RunAction.8"); //$NON-NLS-1$
            String errorTitle = Messages.getString("RunAction.9"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }

        // Check if the executable path points to a valid location.
        File executable = new File(executableFile);
        if( !executable.exists()) {
            String errorMessage = Messages.getString("RunAction.10"); //$NON-NLS-1$
            String errorTitle = Messages.getString("RunAction.11"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }
        // Serialize the document to a temporary location.
        File tempConfFile = new File(mTempConfigurationFile);
        assert(mParentEditor.getDocument() != null);
        FileUtils.setDocumentFile(mParentEditor.getDocument(), tempConfFile);
        
        DOMUtils.serializeDocument(mParentEditor.getDocument(), mParentEditor);
        
        // Create a new thread to run the process on which will handle updating 
        // a dialog box containing the output of the model.
        Thread runnerThread = new Thread(new ModelRunner( executable, tempConfFile, mParentEditor, this));
        runnerThread.start();
    }
    
    /**
     * Get whether the model is running. This method should be used instead of accessing the local
     * variable to protect against race conditions.
     * @return Whether the model is running.
     */
    synchronized boolean isModelRunning(){
    	return mModelRunning;
    }
    
    /**
     * Set that the model is either running or not running.
     * @param aIsRunning Whether the model is running.
     */
    synchronized void setModelRunning(boolean aIsRunning){
        Logger.global.log(Level.INFO, "Setting model running to:" + aIsRunning);
    	mModelRunning = aIsRunning;
    }
}
