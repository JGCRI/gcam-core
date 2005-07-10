/**
 * 
 */
package configurationeditor;

import guicomponents.DOMComboBoxModel;
import guicomponents.SingleDOMValueComboBoxFactory;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.border.BevelBorder;

import org.w3c.dom.Document;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

import utils.Util;
import utils.WindowCloseListener;

/**
 * @author Josh Lurz
 * 
 */
public class LogEditor extends JDialog implements DOMDocumentEditor {
    /**
     * A unique class identifier used for serializing.
     */
    private static final long serialVersionUID = -1626218404879455659L;

    private JPanel mLogPanel = null;

    private JSplitPane mLogSplitPane = null;

    private JPanel mLogModificationPanel = null;

    private JPanel mLogSelecterPanel = null;
    
    /**
     * A combo box containing the minimum log warning level for the currently
     * selected log.
     */
    private JComboBox mMinLogWarningLevel = null;

    /**
     * A combo box containing the minimul log warning level to print to screen
     * for the currently selected log.
     */
    private JComboBox mMinLogToScreenWarningLevel = null;

    /**
     * A check box which tells whether to prefix log messages with their warning
     * level for the currently selected log.
     */
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
     * The name of the root element of a log conf file. TODO: Check if this is
     * right.
     */
    final static String LOG_CONF_ROOT = "Loggers";

    /**
     * The element name for a single logger. TODO: Check this.
     */
    final static String SINGLE_LOGGER_NAME = "Logger";

    /**
     * The strings representing each warning level in ascending order of
     * severity.
     */
    final private static String[] LEVELS = { "Debug", "Notice", "Warning",
            "Error", "Severe" };

    /**
     * The node name of the log attribute which specifies the minimum level of
     * message to send to the screen.
     */
    private static final String MIN_TO_SCREEN_WARNING_LEVEL = "minToScreenWarningLevel";

    /**
     * The node name of the log attribute which specifies the minimum level of
     * message to record.
     */
    private static final String MIN_LOG_WARNING_LEVEL = "minLogWarningLevel";

    /**
     * Constructor
     * 
     * @param aParentFrame
     *            The parent frame which determines where this dialog is
     *            displayed.
     */
    public LogEditor(Frame aParentFrame) {
        super(aParentFrame, "Log Settings", true);
        loadDocument();
        initialize();
    }

    /**
     * This method initializes the log editor dialog box.
     */
    private void initialize() {
        addWindowListener(new WindowCloseListener());
        mSingleValueComboBoxFactory = new SingleDOMValueComboBoxFactory(
                mDocument, LOG_CONF_ROOT, SINGLE_LOGGER_NAME);
        setContentPane(getLogPanel());
    }

    /**
     * This method initializes the log panel.
     * 
     * @return The log panel.
     */
    private JPanel getLogPanel() {
        if (mLogPanel == null) {
            mLogPanel = new JPanel();
            mLogPanel.setBorder(BorderFactory
                    .createBevelBorder(BevelBorder.RAISED));
            mLogPanel.setToolTipText(Messages
                    .getString("ConfigurationEditor.80")); //$NON-NLS-1$
            mLogPanel.add(getLogSplitPane(), null);
        }
        return mLogPanel;
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
            mLogModificationPanel = new JPanel();
            mLogModificationPanel.setLayout(new BoxLayout(
                    mLogModificationPanel, BoxLayout.Y_AXIS));

            JLabel minLogToScreenLabel = new JLabel();
            minLogToScreenLabel.setText(Messages
                    .getString("ConfigurationEditor.81")); //$NON-NLS-1$
            minLogToScreenLabel.setToolTipText(Messages
                    .getString("ConfigurationEditor.83")); //$NON-NLS-1$
            JLabel minLogWarningLevel = new JLabel();
            minLogWarningLevel.setText(Messages
                    .getString("ConfigurationEditor.84")); //$NON-NLS-1$
            minLogWarningLevel.setToolTipText(Messages
                    .getString("ConfigurationEditor.86")); //$NON-NLS-1$
            mLogModificationPanel.add(minLogWarningLevel);
            mLogModificationPanel.add(getMinLogWarningLevel(), null);
            mLogModificationPanel.add(getMinLogToScreenWarningLevel(), null);
            mLogModificationPanel.add(getPrintLogWarningLevelCheckBox(), null);

            // Add OK and cancel buttons.
            JButton okButton = new JButton();
            okButton.setToolTipText("Save the current changes.");
            okButton.setText("OK");

            // Add a listener which will save the batch file and close the
            // window.
            okButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    // Save the batch file document.
                    Util.serializeDocument(mDocument, getTopLevelUI());
                    getTopLevelUI().dispose();
                }
            });
            mLogModificationPanel.add(okButton);

            JButton cancelButton = new JButton();
            cancelButton.setToolTipText("Discard the current changes.");
            cancelButton.setText("Cancel");

            // Add a listener which will close the window.
            cancelButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    getTopLevelUI().dispose();
                }
            });
            mLogModificationPanel.add(cancelButton);
        }
        return mLogModificationPanel;
    }

    /**
     * Return the top level UI window.
     */
    JDialog getTopLevelUI() {
        return this;
    }
    /**
     * This method initializes the minimum log warning level combo box.
     * 
     * @return The minimum log warning level combo box.
     */
    private JComboBox getMinLogWarningLevel() {
        if (mMinLogWarningLevel == null) {
            // TODO: Check this string.
            mMinLogWarningLevel = mSingleValueComboBoxFactory.createComboBox(
                    LEVELS, MIN_LOG_WARNING_LEVEL);
            mMinLogWarningLevel.setSelectedIndex(-1);
            mMinLogWarningLevel.setToolTipText(Messages
                    .getString("ConfigurationEditor.87")); //$NON-NLS-1$
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
            JLabel logSelectorLabel = new JLabel(Messages
                    .getString("ConfigurationEditor.89")); //$NON-NLS-1$
            logSelectorLabel.setToolTipText(Messages
                    .getString("ConfigurationEditor.90")); //$NON-NLS-1$
            mLogSelecterPanel.add(logSelectorLabel, null);
            
            // Create the logger selecter combo box.
            JComboBox loggerSelecter = new JComboBox(new DOMComboBoxModel(mDocument, null, "Loggers", null));
            
            // Add a selection listener which will update the other two combo boxes.
            // TODO: UI needs an update here I bet.
            loggerSelecter.addItemListener(new ItemListener() {
                /**
                 * Method which is called when the selected item
                 * changes.
                 */
                public void itemStateChanged(ItemEvent aEvent) {
                    System.out.println("ITEM STATE CHANGED!");
                    // Update the dependent combo boxes
                    // that their parent has changed.
                    // Set the parent to be the name of the currently selected
                    // log.
                    if(aEvent.getItem() != null) {
                        mSingleValueComboBoxFactory.setParentName(aEvent.getItem().toString());
                    }
                    else {
                        // Not sure if this is right.
                        mSingleValueComboBoxFactory.setParentName("");
                    }
                }
                
            });
            mLogSelecterPanel.add(loggerSelecter, null);
        }
        return mLogSelecterPanel;
    }

    /**
     * This method initializes the minimum log to screen combo box.
     * 
     * @return The minimum log to screen combo box.
     */
    private JComboBox getMinLogToScreenWarningLevel() {
        if (mMinLogToScreenWarningLevel == null) {
            // TODO: Check this string.
            mMinLogToScreenWarningLevel = mSingleValueComboBoxFactory
                    .createComboBox(LEVELS, MIN_TO_SCREEN_WARNING_LEVEL);
            mMinLogToScreenWarningLevel.setSelectedIndex(-1);
            mMinLogToScreenWarningLevel.setToolTipText(Messages
                    .getString("ConfigurationEditor.92")); //$NON-NLS-1$
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
            mPrintLogWarningLevelCheckBox.setText(Messages
                    .getString("ConfigurationEditor.94")); //$NON-NLS-1$
            mPrintLogWarningLevelCheckBox.setToolTipText(Messages
                    .getString("ConfigurationEditor.95")); //$NON-NLS-1$
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
        Properties props = Util.getInitializedProperties(this);

        // Get the path to the executable.
        String logConfPath = props
                .getProperty(PropertiesInfo.LOG_CONF_PROPERTY);

        // Check if the log path has been initialized.
        if (logConfPath == null) {
            String errorMessage = "You must set the path to the log configuration file under Edit->Preferences before you can edit the file.";
            String errorTitle = "Log configuration file path unset";
            JOptionPane.showMessageDialog(this, errorMessage,
                    errorTitle, JOptionPane.ERROR_MESSAGE);
            return;
        }
        try {
            File newFile = new File(logConfPath);
            // Check if the file exists.
            // TODO: How does the user create a new log file?
            if (!newFile.exists()) {
                // Tell the user the file does not exist.
                String message = "File does not exist";
                String messageTitle = "Incorrect path";
                Logger.global.log(Level.SEVERE, message);
                JOptionPane.showMessageDialog(this, message,
                        messageTitle, JOptionPane.ERROR_MESSAGE);
                return;
            }
            mDocument = Util.getDocumentBuilder(this).parse(
                    logConfPath);
            Util.setDocumentFile(mDocument, newFile);
        } catch (Exception e) {
            // Report the error to the user.
            String message = "File could not be loaded: " + e.getMessage();
            String messageTitle = "Error reading XML file";
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(this, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
            return;
        }

        // Check if this is a log configuration document.
        if (!mDocument.getDocumentElement().getNodeName().equals(LOG_CONF_ROOT)) {
            final String message = "Selected document is not a log configuration document.";
            final String messageTitle = "Incorrect document";
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(this, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
            return;
        }
        // Add an event handler which will listen for the document being
        // changed and set that the document needs to be saved.
        EventTarget target = (EventTarget) mDocument.getDocumentElement();
        target.addEventListener("DOMSubtreeModified",
                new LoggerSettingsDocumentMutationListener(), true);
    }

    /**
     * Get the DOM document which stores the data this interface modifies.
     */
    public Document getDocument() {
        return mDocument;
    }

    /**
     * This mutation listener watches for events which change the underlying
     * document. When a mutation event is received the dirty attribute is set on
     * the document.
     * 
     * @author Josh Lurz
     */
    private final class LoggerSettingsDocumentMutationListener implements
            EventListener {
        public void handleEvent(Event aEvent) {
            // This doesn't recursively send another event,
            // not sure why but it works.
            mDocument.getDocumentElement().setAttribute("needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
}
