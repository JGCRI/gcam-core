/**
 * 
 */
package configurationeditor;

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
 * allows the creation of new logs and the deletion of old ones.
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
	 * The top level panel contains by the dialog. The panel contains the split
	 * pane.
	 */
	private JPanel mLogPanel = null;

	/**
	 * The split pane which seperates the panel for selecting the log to modify
	 * and the log modification panel.
	 */
	private JSplitPane mLogSplitPane = null;

	/**
	 * The panel which contains the user interface elements to modify the
	 * settings for a chosen log.
	 */
	private JPanel mLogModificationPanel = null;

	/**
	 * The panel where the user can select the log to modify, or add and remove
	 * logs.
	 */
	private JPanel mLogSelecterPanel = null;

	/**
	 * A combo box which selects the logger to modify.
	 */
	private JComboBox mLoggerSelectorComboBox = null;

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

			JLabel minLogToScreenLabel = new JLabel(Messages
					.getString("ConfigurationEditor.81")); //$NON-NLS-1$
			mLogModificationPanel.add(minLogToScreenLabel);

			JLabel minLogWarningLevel = new JLabel(Messages
					.getString("ConfigurationEditor.84")); //$NON-NLS-1$;
			mLogModificationPanel.add(minLogWarningLevel);

			mLogModificationPanel.add(getMinLogWarningLevelComboBox(), null);
			mLogModificationPanel.add(getMinLogToScreenWarningLevelComboBox(),
					null);
			mLogModificationPanel.add(getPrintLogWarningLevelCheckBox(), null);

			// Check if there are any loggers currently.
			if (getLoggerSelectorComboBox().getModel().getSize() > 0) {
				// Set the selected item to be the first logger.
				getLoggerSelectorComboBox().setSelectedIndex(0);
			}
			// Otherwise disable the GUI elements.
			else {
				Component logModificationUIs[] = getLogModificationPanel()
						.getComponents();
				// Disable the UI until the user creates a log.
				for (int i = 0; i < logModificationUIs.length; ++i) {
					logModificationUIs[i].setEnabled(false);
				}
			}
			// TODO: This should not be in this frame.
			// Add OK and cancel buttons.
			JButton okButton = new JButton();
			okButton.setToolTipText("Save the current changes.");
			okButton.setText("OK");

			// Add a listener which will save the batch file and close the
			// window.
			okButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent aEvent) {
					// Save the batch file document.
					DOMUtils.serializeDocument(mDocument, getTopLevelUI());
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
	private JPanel getLogSelecterPanel() {
		if (mLogSelecterPanel == null) {
			mLogSelecterPanel = new JPanel();
			JLabel logSelectorLabel = new JLabel(Messages
					.getString("ConfigurationEditor.89")); //$NON-NLS-1$
			logSelectorLabel.setToolTipText(Messages
					.getString("ConfigurationEditor.90")); //$NON-NLS-1$
			mLogSelecterPanel.add(logSelectorLabel, null);
			mLogSelecterPanel.add(getLoggerSelectorComboBox(), null);
		}
		return mLogSelecterPanel;
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
					mDocument, null, "Loggers", null));

			// Add a selection listener which will update the other two combo
			// boxes.
			// TODO: UI needs an update here I bet.
			mLoggerSelectorComboBox.addItemListener(new ItemListener() {
				/**
				 * Method which is called when the selected item changes.
				 */
				public void itemStateChanged(ItemEvent aEvent) {
					System.out.println("ITEM STATE CHANGED!");
					// Update the dependent combo boxes
					// that their parent has changed.
					// Set the parent to be the name of the currently selected
					// log.
					boolean enableLogModificationUI;
					if (aEvent.getItem() != null) {
						mSingleValueComboBoxFactory.setParentName(aEvent
								.getItem().toString());
						enableLogModificationUI = true;
					} else {
						// Not sure if this is right.
						mSingleValueComboBoxFactory.setParentName("");
						enableLogModificationUI = false;
					}

					// Enable or disable the log modification components
					// based on whether a log is selected.
					Component logModificationUIs[] = getLogModificationPanel()
							.getComponents();
					for (int i = 0; i < logModificationUIs.length; ++i) {
						logModificationUIs[i]
								.setEnabled(enableLogModificationUI);
					}
				}
			});
		}
		return mLoggerSelectorComboBox;
	}

	/**
	 * This method initializes the minimum log warning level combo box.
	 * 
	 * @return The minimum log warning level combo box.
	 */
	private JComboBox getMinLogWarningLevelComboBox() {
		if (mMinLogWarningLevel == null) {
			mMinLogWarningLevel = mSingleValueComboBoxFactory.createComboBox(
					LEVELS, MIN_LOG_WARNING_LEVEL);
			mMinLogWarningLevel.setToolTipText(Messages
					.getString("ConfigurationEditor.87")); //$NON-NLS-1$
		}
		return mMinLogWarningLevel;
	}

	/**
	 * This method initializes the minimum log to screen combo box.
	 * 
	 * @return The minimum log to screen combo box.
	 */
	private JComboBox getMinLogToScreenWarningLevelComboBox() {
		if (mMinLogToScreenWarningLevel == null) {
			mMinLogToScreenWarningLevel = mSingleValueComboBoxFactory
					.createComboBox(LEVELS, MIN_TO_SCREEN_WARNING_LEVEL);
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
		Properties props = FileUtils.getInitializedProperties(this);

		// Get the path to the executable.
		String logConfPath = props
				.getProperty(PropertiesInfo.LOG_CONF_PROPERTY);

		// Check if the log path has been initialized.
		if (logConfPath == null) {
			String errorMessage = "You must set the path to the log configuration file under Edit->Preferences before you can edit the file.";
			String errorTitle = "Log configuration file path unset";
			JOptionPane.showMessageDialog(this, errorMessage, errorTitle,
					JOptionPane.ERROR_MESSAGE);
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
				JOptionPane.showMessageDialog(this, message, messageTitle,
						JOptionPane.ERROR_MESSAGE);
				return;
			}
			mDocument = DOMUtils.getDocumentBuilder(this).parse(logConfPath);
			FileUtils.setDocumentFile(mDocument, newFile);
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
	 * 
	 * @return The log configuration XML document tree.
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
		/**
		 * The method called when a mutation event is received from the log
		 * configuration document.
		 * 
		 * @param aEvent
		 *            The mutation event received.
		 */
		public void handleEvent(Event aEvent) {
			// This doesn't recursively send another event,
			// not sure why but it works.
			mDocument.getDocumentElement().setAttribute("needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}
}
