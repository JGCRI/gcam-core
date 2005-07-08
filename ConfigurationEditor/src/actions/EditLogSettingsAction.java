/**
 * 
 */
package actions;

import interfaceutils.SingleDOMValueComboBoxFactory;
import interfaceutils.Util;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.BevelBorder;

import org.w3c.dom.Document;

import configurationeditor.ConfigurationEditor;
import configurationeditor.Messages;
import configurationeditor.PropertiesInfo;

/**
 * This class displays a dialog for editing log settings.
 * @author Josh Lurz
 */
public class EditLogSettingsAction extends AbstractAction {
    /**
     * Identifier used for serializing.
     */
    private static final long serialVersionUID = 1257512799098654257L;

    /**
     * A reference to the top level editor from which this action is receiving
     * commands.
     */
    private ConfigurationEditor mParentEditor = null;
	
    /**
	 * The dialog which contains the log editing UI.
	 */
	private JDialog mEditLogsDialog = null;
	
	private JPanel mLogPane = null;

	private JSplitPane mLogSplitPane = null;

	private JPanel mLogModificationPanel = null;

	private JComboBox mMinLogWarningLevel = null;

	private JLabel mLogWarningLevelLabel = null;

	private JPanel mLogSelecterPanel = null;

	private JComboBox mLoggerSelector = null;

	private JLabel mLogToScreenLevelLabel = null;

	private JComboBox mMinLogToScreenWarningLevel = null;

	private JCheckBox mPrintLogWarningLevelCheckBox = null;
	
    /**
     * The factory which will create the level selection combo boxes.
     */
    private SingleDOMValueComboBoxFactory mSingleValueComboBoxFactory = null;
    
    /**
     * The document which contains the log configuration options.
     */
    private Document mDocument = null;
	
    /** 
     * The name of the root element of a log conf file.
     * TODO: Check if this is right.
     */
    final static String LOG_CONF_ROOT = "Loggers";
    
    /**
	 * Constructor
	 */
	public EditLogSettingsAction(ConfigurationEditor aParentEditor) {
		super("EditLogSettings"); //$NON-NLS-1$
		mParentEditor = aParentEditor;
	}

	/**
	 * Method called when an action is performed on the dispatching object.
	 * @param aEvent The event received.
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
    public void actionPerformed(ActionEvent aEvent) {
        // Open the document.
        loadDocument();
        mSingleValueComboBoxFactory = new SingleDOMValueComboBoxFactory(mDocument, LOG_CONF_ROOT);
        JDialog editLogsDialog = getEditLogsDialog();
        editLogsDialog.pack();
        editLogsDialog.setVisible(true);
	}
	
    /**
     * This method initializes preferenceDialogFrame
     * @return The edit logs dialog box.
     */    
    private JDialog getEditLogsDialog() {
        if (mEditLogsDialog == null) {
        	mEditLogsDialog = new JDialog(mParentEditor, Messages.getString("ShowPreferencesAction.3"), true ); //$NON-NLS-1$
        	mEditLogsDialog.setContentPane(getLogPanel());
        }
        return mEditLogsDialog;
    }
    
    /**
     * This method initializes the log panel.
     * 
     * @return The log panel.
     */
    private JPanel getLogPanel() {
        if (mLogPane == null) {
            mLogPane = new JPanel();
            mLogPane.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
            mLogPane.setToolTipText(Messages.getString("ConfigurationEditor.80")); //$NON-NLS-1$
            mLogPane.add(getLogSplitPane(), null);
        }
        return mLogPane;
    }
    
    /**
     * This method initializes the log split pane.
     * 
     * @return The log split pane.
     */
    private JSplitPane getLogSplitPane() {
        if (mLogSplitPane == null) {
            mLogSplitPane = new JSplitPane();
            mLogSplitPane.setRightComponent(getLogModificationPanel());
            mLogSplitPane.setLeftComponent(getLogSelecterPanel());
        }
        return mLogSplitPane;
    }
    
    /**
     * This method initializes the log modification panel.
     * 
     * @return The log modification panel.
     */
    private JPanel getLogModificationPanel() {
        if (mLogModificationPanel == null) {
            mLogToScreenLevelLabel = new JLabel();
            mLogToScreenLevelLabel.setText(Messages.getString("ConfigurationEditor.81")); //$NON-NLS-1$
            mLogToScreenLevelLabel.setToolTipText(Messages.getString("ConfigurationEditor.83")); //$NON-NLS-1$
            mLogWarningLevelLabel = new JLabel();
            mLogWarningLevelLabel.setText(Messages.getString("ConfigurationEditor.84")); //$NON-NLS-1$
            mLogWarningLevelLabel
                    .setToolTipText(Messages.getString("ConfigurationEditor.86")); //$NON-NLS-1$
            mLogModificationPanel = new JPanel();
            mLogModificationPanel.setLayout(new BoxLayout(mLogModificationPanel,
                    BoxLayout.Y_AXIS));
            mLogModificationPanel.add(mLogWarningLevelLabel, null);
            mLogModificationPanel.add(getMinLogWarningLevel(), null);
            mLogModificationPanel.add(mLogToScreenLevelLabel, null);
            mLogModificationPanel.add(getMinLogToScreenWarningLevel(), null);
            mLogModificationPanel.add(getPrintLogWarningLevelCheckBox(), null);
        }
        return mLogModificationPanel;
    }

    /**
     * This method initializes the minimum log warning level combo box.
     * 
     * @return The minimum log warning level combo box.
     */
    private JComboBox getMinLogWarningLevel() {
        if (mMinLogWarningLevel == null) {
            mMinLogWarningLevel = new JComboBox();
            mMinLogWarningLevel.setSelectedIndex(-1);
            mMinLogWarningLevel
                    .setToolTipText(Messages.getString("ConfigurationEditor.87")); //$NON-NLS-1$
        }
        return mMinLogWarningLevel;
    }

    /**
     * This method initializes the log selection panel.
     * 
     * @return The log selection panel.
     */
    private JPanel getLogSelecterPanel() {
        if (mLogSelecterPanel == null) {
            mLogSelecterPanel = new JPanel();
            JLabel logSelectorLabel = new JLabel(Messages.getString("ConfigurationEditor.89")); //$NON-NLS-1$
            logSelectorLabel.setToolTipText(Messages.getString("ConfigurationEditor.90")); //$NON-NLS-1$
            mLogSelecterPanel.add(logSelectorLabel, null);
            mLogSelecterPanel.add(getLoggerSelector(), null);
        }
        return mLogSelecterPanel;
    }

    /**
     * This method initializes the logger selector.
     * 
     * @return The log selector combo box.
     */
    private JComboBox getLoggerSelector() {
        if (mLoggerSelector == null) {
            mLoggerSelector = new JComboBox();
        }
        return mLoggerSelector;
    }

    /**
     * This method initializes the minimum log to screen combo box.
     * 
     * @return The minimum log to screen combo box.
     */
    private JComboBox getMinLogToScreenWarningLevel() {
        if (mMinLogToScreenWarningLevel == null) {
            mMinLogToScreenWarningLevel = new JComboBox();
            mMinLogToScreenWarningLevel.setSelectedIndex(-1);
            mMinLogToScreenWarningLevel
                    .setToolTipText(Messages.getString("ConfigurationEditor.92")); //$NON-NLS-1$
        }
        return mMinLogToScreenWarningLevel;
    }

    /**
     * This method initializes the print log warning level checkbox.
     * 
     * @return The print log warning level checkbox.
     */
    private JCheckBox getPrintLogWarningLevelCheckBox() {
        if (mPrintLogWarningLevelCheckBox == null) {
            mPrintLogWarningLevelCheckBox = new JCheckBox();
            mPrintLogWarningLevelCheckBox.setMnemonic(KeyEvent.VK_W);
            mPrintLogWarningLevelCheckBox
                    .setText(Messages.getString("ConfigurationEditor.94")); //$NON-NLS-1$
            mPrintLogWarningLevelCheckBox
                    .setToolTipText(Messages.getString("ConfigurationEditor.95")); //$NON-NLS-1$
        }
        return mPrintLogWarningLevelCheckBox;
    }
    
    /**
     * Load a document into the current document.
     */
    private void loadDocument() {
        Logger.global.log(Level.INFO, "Loading log configuration document.");
        // Use the parent editor for error messages as this window
        // will not have opened yet.
        
        // Search the preferences information for the name of the file to open.
        // Get the executable path from the properties file.
        Properties props = Util.getInitializedProperties(mEditLogsDialog);
        
        // Get the path to the executable.
        String logConfPath = props.getProperty(PropertiesInfo.LOG_CONF_PROPERTY);
        
        // Check if the log path has been initialized.
        if( logConfPath == null) {
            String errorMessage = "You must set the path to the log configuration file under Edit->Preferences before you can edit the file.";
            String errorTitle = "Log configuration file path unset";
            JOptionPane.showMessageDialog(mParentEditor, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
            return;
        }
        try {
            File newFile = new File(logConfPath);
            // Check if the file exists.
            // TODO: How does the user create a new log file?
            if(!newFile.exists()){
                // Tell the user the file does not exist.
                String message = "File does not exist";
                String messageTitle = "Incorrect path";
                Logger.global.log(Level.SEVERE, message);
                JOptionPane.showMessageDialog(mParentEditor, message, messageTitle,
                        JOptionPane.ERROR_MESSAGE);
                return;
            }
            mDocument = Util.getDocumentBuilder(mParentEditor).parse(logConfPath);
            Util.setDocumentFile(mDocument, newFile);
        } catch (Exception e) {
            // Report the error to the user.
            String message = "File could not be loaded: " + e.getMessage();
            String messageTitle = "Error reading XML file";
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(mParentEditor, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
            return;
        }
        
        // Check if this is a log configuration document.
        if(!mDocument.getDocumentElement().getNodeName().equals(LOG_CONF_ROOT)) {
            final String message = "Selected document is not a log configuration document.";
            final String messageTitle = "Incorrect document";
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(mParentEditor, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
        }
    }

}
