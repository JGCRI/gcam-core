/**
 * 
 */
package configurationeditor;

import guicomponents.DOMButtonModel;
import guicomponents.DOMComboBoxModel;
import guicomponents.SingleDOMValueComboBoxFactory;

import java.awt.Component;
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

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;
import utils.WindowCloseListener;

/**
 * The log editor is a component used for editing the settings of the various
 * logs used by the model. The log editor contains two panes, one to select the
 * log to modify and the second to modify the settings. The log editor also
 * allows the creation of new logs and the deletion of old ones. TODO: Handle
 * the type attribute of the logger.
 * 
 * @author Josh Lurz
 * 
 */
public class LogEditor extends JDialog implements DOMDocumentEditor {
    /**
     * A unique class identifier used for serializing.
     */
    private static final long serialVersionUID = -1626218404879455659L;

    /**
     * The panel which contains the user interface elements to modify the
     * settings for a chosen log.
     */
    private transient JPanel mLogModificationPanel = null;

    /**
     * A combo box which selects the logger to modify.
     */
    private transient JComboBox mLoggerSelectorComboBox = null;

    /**
     * A check box which tells whether to prefix log messages with their warning
     * level for the currently selected log.
     */
    private transient JCheckBox mPrintLogWarningLevelCheckBox = null;

    /**
     * The factory which will create the level selection combo boxes.
     */
    private transient SingleDOMValueComboBoxFactory mComboBoxFactory = null;

    /**
     * The document which contains the log configuration options.
     */
    private transient Document mDocument = null;

    /**
     * The name of the root element of a log conf file.
     */
    final static String LOG_ROOT = "LoggerFactory";

    /**
     * The element name for a single logger.
     */
    final static String SINGLE_LOGGER = "Logger";

    /**
     * The strings representing each warning level in ascending order of
     * severity.
     */
    final private static String[] LEVEL_STRINGS = { "Debug", "Notice",
            "Warning", "Error", "Severe" };

    /**
     * The node name of the log attribute which specifies the minimum level of
     * message to send to the screen.
     */
    private static final String MIN_SCREEN_LEVEL = "minToScreenWarningLevel";

    /**
     * The node name of the log attribute which specifies the minimum level of
     * message to record.
     */
    private static final String MIN_LOG_LEVEL = "minLogWarningLevel";

    /**
     * The name of the node that contains the boolean value representing whether
     * to print the warning level as part of each log message.
     */
    private static final String PRINT_LEVEL = "printLogWarningLevel";

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
        editorInitialize();
    }

    /**
     * This method initializes the log editor dialog box.
     */
    private final void editorInitialize() {
        addWindowListener(new WindowCloseListener());
        mComboBoxFactory = new SingleDOMValueComboBoxFactory(
                mDocument, LOG_ROOT, SINGLE_LOGGER);
        setContentPane(createMainPanel());
    }

    /**
     * Create the main panel for the log editor.
     * 
     * @return The main panel for the log editor.
     */
    private JPanel createMainPanel() {
        final JPanel logPanel = new JPanel();
        logPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
        logPanel.setToolTipText(Messages.getString("ConfigurationEditor.80")); //$NON-NLS-1$
        final JSplitPane splitPane = createSplitPane();
        logPanel.add(splitPane, null);
        return logPanel;
    }

    /**
     * Create the split pane which will seperate the log selecter and log
     * modification panel.
     * 
     * @return A split pane containing the log selecter panel and log modifier
     *         panel.
     */
    private JSplitPane createSplitPane() {
        final JSplitPane splitPane = new JSplitPane();
        splitPane.setRightComponent(getLogModificationPanel());
        splitPane.setLeftComponent(createLoggerSelecterPanel());
        return splitPane;
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

            final JLabel minLogToScreenLabel = new JLabel(Messages
                    .getString("ConfigurationEditor.81")); //$NON-NLS-1$
            mLogModificationPanel.add(minLogToScreenLabel);

            final JLabel minLogWarningLevel = new JLabel(Messages
                    .getString("ConfigurationEditor.84")); //$NON-NLS-1$;
            mLogModificationPanel.add(minLogWarningLevel);

            mLogModificationPanel.add(createMinLogLevelCombo(), null);
            mLogModificationPanel.add(createMinScreenLogLevelCombo(), null);
            mLogModificationPanel.add(getPrintLogWarningLevelCheckBox(), null);

            // Check if there are any loggers currently.
            if (getLoggerSelectorComboBox().getModel().getSize() > 0) {
                // Set the selected item to be the first logger.
                getLoggerSelectorComboBox().setSelectedIndex(0);
                final String parentName = getLoggerSelectorComboBox()
                        .getSelectedItem().toString();
                mComboBoxFactory.setParentName(parentName);
                ((DOMButtonModel) getPrintLogWarningLevelCheckBox().getModel())
                        .setParent(parentName);
            }
            // Otherwise disable the GUI elements.
            else {
                final Component logModUIs[] = getLogModificationPanel()
                        .getComponents();
                // Disable the UI until the user creates a log.
                for (int i = 0; i < logModUIs.length; ++i) {
                    logModUIs[i].setEnabled(false);
                }
            }
            // TODO: This should not be in this frame.
            // Add OK and cancel buttons.
            final JButton okButton = new JButton();
            okButton.setToolTipText("Save the current changes.");
            okButton.setText("OK");

            // Add a listener which will save the batch file and close the
            // window.
            okButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    // Save the batch file document.
                    DOMUtils.serialize(mDocument, getTopLevelUI());
                    getTopLevelUI().dispose();
                }
            });
            mLogModificationPanel.add(okButton);

            final JButton cancelButton = new JButton();
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
     * 
     * @return The top level window.
     */
    JDialog getTopLevelUI() {
        return this;
    }

    /**
     * This method initializes the log selection panel.
     * 
     * @return The log selection panel.
     */
    private JPanel createLoggerSelecterPanel() {
        final JPanel loggerSelecter = new JPanel();
        final JLabel logSelectorLabel = new JLabel(Messages
                .getString("ConfigurationEditor.89")); //$NON-NLS-1$
        logSelectorLabel.setToolTipText(Messages
                .getString("ConfigurationEditor.90")); //$NON-NLS-1$
        loggerSelecter.add(logSelectorLabel, null);
        loggerSelecter.add(getLoggerSelectorComboBox(), null);
        return loggerSelecter;
    }

    /**
     * This method initializes the logger selector combo box.
     * 
     * @return The logger selector combo box.
     */
    private JComboBox getLoggerSelectorComboBox() {
        if (mLoggerSelectorComboBox == null) {
            // Create the logger selecter combo box.
            mLoggerSelectorComboBox = new JComboBox(new DOMComboBoxModel(
                    mDocument, null, LOG_ROOT, null));
            // Add a selection listener which will update the other two combo
            // boxes.
            // TODO: UI needs an update here I bet.
            mLoggerSelectorComboBox.addItemListener(new ItemListener() {
                /**
                 * Method which is called when the selected item changes.
                 */
                public void itemStateChanged(ItemEvent aEvent) {
                    // Update the dependent combo boxes
                    // that their parent has changed.
                    // Set the parent to be the name of the currently selected
                    // log.
                    boolean enable;
                    String parentName;
                    if (aEvent.getItem() == null) {
                        // Not sure if this is right.
                        parentName = "";
                        enable = false;
                    } else {
                        parentName = aEvent.getItem().toString();
                        enable = true;
                    }

                    mComboBoxFactory.setParentName(parentName);
                    ((DOMButtonModel) getPrintLogWarningLevelCheckBox()
                            .getModel()).setParent(parentName);
                    // Enable or disable the log modification components
                    // based on whether a log is selected.
                    final Component logModUIs[] = getLogModificationPanel()
                            .getComponents();
                    for (int i = 0; i < logModUIs.length; ++i) {
                        logModUIs[i].setEnabled(enable);
                    }
                    // Update the user interface.
                    getLogModificationPanel().repaint();
                }
            });
        }
        return mLoggerSelectorComboBox;
    }

    /**
     * This method creates and initializes the minimum log warning level combo
     * box.
     * 
     * @return A minimum log warning level combo box.
     */
    private JComboBox createMinLogLevelCombo() {
        final JComboBox minLogLevel = mComboBoxFactory
                .createComboBox(LEVEL_STRINGS, MIN_LOG_LEVEL);
        minLogLevel
                .setToolTipText(Messages.getString("ConfigurationEditor.87")); //$NON-NLS-1$
        return minLogLevel;
    }

    /**
     * This method initializes the minimum log to screen combo box.
     * 
     * @return The minimum log to screen combo box.
     */
    private JComboBox createMinScreenLogLevelCombo() {
        final JComboBox minScreenLevel = mComboBoxFactory
                .createComboBox(LEVEL_STRINGS, MIN_SCREEN_LEVEL);
        minScreenLevel.setToolTipText(Messages
                .getString("ConfigurationEditor.92")); //$NON-NLS-1$
        return minScreenLevel;
    }

    /**
     * This method initializes the print log warning level checkbox.
     * 
     * @return The print log warning level checkbox.
     */
    private JCheckBox getPrintLogWarningLevelCheckBox() {
        if (mPrintLogWarningLevelCheckBox == null) {
            mPrintLogWarningLevelCheckBox = new JCheckBox();
            final String parentXPath = "/" + LOG_ROOT + "/" + SINGLE_LOGGER;
            mPrintLogWarningLevelCheckBox.setModel(new DOMButtonModel(this,
                    parentXPath, PRINT_LEVEL, "", true));
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
        final Properties props = FileUtils.getInitializedProperties(this);

        // Get the path to the executable.
        final String logConfPath = props.getProperty(PropertiesInfo.LOG_CONF);

        // Check if the log path has been initialized.
        if (logConfPath == null) {
            final String errorMessage = "You must set the path to the log configuration file under Edit->Preferences before you can edit the file.";
            final String errorTitle = "Log configuration file path unset";
            JOptionPane.showMessageDialog(this, errorMessage, errorTitle,
                    JOptionPane.ERROR_MESSAGE);
            return;
        }
        try {
            final File newFile = new File(logConfPath);
            // Check if the file exists.
            // TODO: How does the user create a new log file?
            if (!newFile.exists()) {
                // Tell the user the file does not exist.
                final String message = "File does not exist";
                final String messageTitle = "Incorrect path";
                Logger.global.log(Level.SEVERE, message);
                JOptionPane.showMessageDialog(this, message, messageTitle,
                        JOptionPane.ERROR_MESSAGE);
                return;
            }
            mDocument = DOMUtils.getDocumentBuilder(this).parse(logConfPath);
            FileUtils.setDocumentFile(mDocument, newFile);
        } catch (Exception e) {
            // Report the error to the user.
            final String message = "File could not be loaded: "
                    + e.getMessage();
            final String messageTitle = "Error reading XML file";
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(this, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
            return;
        }

        // Check if this is a log configuration document.
        if (!mDocument.getDocumentElement().getNodeName().equals(LOG_ROOT)) {
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
        final EventTarget target = (EventTarget) mDocument.getDocumentElement();
        target.addEventListener("DOMSubtreeModified",
                new LoggerSettingsDocumentMutationListener(), true);
    }

    /**
     * Get the DOM document which stores the data this interface modifies.
     * 
     * @return The log configuration XML document tree.
     */
    public Document getDocument() {
        return mDocument;
    }

    /**
     * Returns that the user does not need to be asked before the file is saved.
     * 
     * @return False, meaning that the user does not need to be asked before
     *         saving the file.
     */
    public boolean askBeforeSaving() {
        return false;
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
        /**
         * Constructor
         */
        LoggerSettingsDocumentMutationListener() {
            super();
        }

        /**
         * The method called when a mutation event is received from the log
         * configuration document.
         * 
         * @param aEvent
         *            The mutation event received.
         */
        public void handleEvent(final Event aEvent) {
            // This doesn't recursively send another event,
            // not sure why but it works.
            mDocument.getDocumentElement().setAttribute("needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
}
