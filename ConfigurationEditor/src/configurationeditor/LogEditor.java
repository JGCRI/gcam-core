/**
 * 
 */
package configurationeditor;

import guicomponents.DOMButtonModel;
import guicomponents.DOMComboBoxController;
import guicomponents.DOMComboBoxModel;
import guicomponents.DOMDocumentSaveSetter;
import guihelpers.WindowCloseListener;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
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
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.border.BevelBorder;

import org.w3c.dom.Document;
import org.w3c.dom.events.EventTarget;

import utils.DOMUtils;
import utils.FileUtils;
import utils.Messages;

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
	 * The document which contains the log configuration options.
	 */
	final private transient Document mDocument;

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
		mDocument = loadDocument();
		editorInitialize();
	}

	/**
	 * This method initializes the log editor dialog box.
	 */
	private final void editorInitialize() {
		addWindowListener(new WindowCloseListener());
		setContentPane(createMainPanel(this));
	}

	/**
	 * Create the main panel for the log editor.
	 * 
	 * @param aParentDialog
	 *            The parent dialog of the panel.
	 * @return The main panel for the log editor.
	 */
	private JPanel createMainPanel(final JDialog aParentDialog) {
		// Create the main panel.
		final JPanel logPanel = new JPanel(new GridBagLayout());
		logPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));

		// Create grid bag constraints to use to position elements.
		final GridBagConstraints cons = new GridBagConstraints();
		// Create a border around all elements.
		cons.insets = new Insets(5, 5, 5, 5);
		cons.anchor = GridBagConstraints.WEST;
		cons.gridx = GridBagConstraints.RELATIVE;
		cons.gridy = 0;

		// Add a combo box which will select the log.
		// Add the label.
		final JLabel logSelectorLabel = new JLabel(Messages
				.getString("ConfigurationEditor.89")); //$NON-NLS-1$
		logSelectorLabel.setToolTipText(Messages
				.getString("ConfigurationEditor.90")); //$NON-NLS-1$
		logSelectorLabel.setHorizontalAlignment(SwingConstants.LEFT);
		logPanel.add(logSelectorLabel, cons);

		// Add the combo box.
		cons.fill = GridBagConstraints.HORIZONTAL;
		cons.weightx = 1;
		final JComboBox loggerSelecter = createLoggerSelecter();
		logPanel.add(loggerSelecter, cons);

		// Add the second set of labels in the third column.
		cons.weightx = 0;
		cons.gridx = 2;

		// Add a label linking all fields for modifying a single logger.
		cons.gridwidth = 2;
		cons.anchor = GridBagConstraints.CENTER;
		final JLabel singleLogLabel = new JLabel(
				"Settings for the selected log");
		singleLogLabel.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		logPanel.add(singleLogLabel);

		cons.anchor = GridBagConstraints.WEST;
		cons.fill = GridBagConstraints.NONE;
		cons.gridwidth = 1;
		cons.gridy = 1;
		final JLabel minToScreenLabel = new JLabel(Messages
				.getString("ConfigurationEditor.81")); //$NON-NLS-1$
		minToScreenLabel.setHorizontalAlignment(SwingConstants.LEFT);
		logPanel.add(minToScreenLabel, cons);

		cons.gridy = 2;
		final JLabel minWarningLevel = new JLabel(Messages
				.getString("ConfigurationEditor.84")); //$NON-NLS-1$;
		minWarningLevel.setHorizontalAlignment(SwingConstants.LEFT);
		logPanel.add(minWarningLevel, cons);

		cons.gridy = 3;
		cons.weightx = 1;
		cons.fill = GridBagConstraints.HORIZONTAL;
		final JCheckBox printLevel = createPrintLevelCheckBox();
		// Add the checkbox model as an item listener to the log selecter
		// to receive updates.
		loggerSelecter.addItemListener((DOMButtonModel) printLevel.getModel());
		logPanel.add(printLevel, cons);

		cons.gridx = 3;
		cons.gridy = 1;
		// Create a factory which will create and control the dependent
		// log modification combo boxes.
		final DOMComboBoxController comboFactory = new DOMComboBoxController(
				mDocument, LOG_ROOT, SINGLE_LOGGER);
		// Add the factory as an item listener of the log selection combo
		// box.
		loggerSelecter.addItemListener(comboFactory);
		logPanel.add(createMinLogLevelCombo(comboFactory), cons);

		cons.gridy = 2;
		logPanel.add(createMinScreenLogLevelCombo(comboFactory), cons);

		// Reset the selected index now that the combo boxes are listening.
		// This is a hack to get the model to send a selection changed event.
		// The 0th item is already selected, so it must be cleared and reset.
		loggerSelecter.setSelectedIndex(-1);
		loggerSelecter.setSelectedIndex(0);

		// Add OK and cancel buttons.
		cons.fill = GridBagConstraints.NONE;
		cons.gridx = 2;
		cons.gridy = 4;
		cons.anchor = GridBagConstraints.EAST;
		final JButton okButton = createOKButton(aParentDialog);
		logPanel.add(okButton, cons);

		cons.gridx = 3;
		cons.anchor = GridBagConstraints.WEST;
		final JButton cancelButton = createCancelButton(aParentDialog);
		logPanel.add(cancelButton, cons);
		return logPanel;
	}

	/**
	 * Creates an OK button which will save changes to the log settings.
	 * 
	 * @param aParentDialog
	 *            The parent dialog of the button.
	 * @return An OK button.
	 */
	private JButton createOKButton(final JDialog aParentDialog) {
		final JButton okButton = new JButton();
		okButton.setToolTipText("Save the current changes.");
		okButton.setText("OK");

		// Add a listener which will save the batch file and close the
		// window.
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				// Save the batch file document.
				DOMUtils.serialize(mDocument, aParentDialog);
				aParentDialog.dispose();
			}
		});
		return okButton;
	}

	/**
	 * Creates a cancel button which will cancel changes to the log settings.
	 * 
	 * @param aParentDialog
	 *            The parent dialog of the button.
	 * @return A cancel button.
	 */
	private JButton createCancelButton(final JDialog aParentDialog) {
		final JButton cancelButton = new JButton();
		cancelButton.setToolTipText("Discard the current changes.");
		cancelButton.setText("Cancel");

		// Add a listener which will close the window.
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				aParentDialog.dispose();
			}
		});
		return cancelButton;
	}

	/**
	 * This method creates and initializes the logger selector combo box.
	 * 
	 * @return The logger selector combo box.
	 */
	private JComboBox createLoggerSelecter() {
		// Create the logger selecter combo box.
		final JComboBox loggerSelecter = new JComboBox(new DOMComboBoxModel(
				mDocument, null, LOG_ROOT, null));

		// Add a selection listener which will update the fields
		// which modify a specific log.
		loggerSelecter.addItemListener(new ItemListener() {
			/**
			 * Method which is called when the selected item changes which will
			 * update the UI.
			 * 
			 * @param aEvent
			 *            The event received.
			 */
			public void itemStateChanged(ItemEvent aEvent) {
				try {
					// Make sure other updates occur first.
					// TODO: This is a hack.
					Thread.sleep(50);
				} catch (InterruptedException aException) {
					Logger.global.log(Level.WARNING,
							"Item listener was interrupted, repainting UI.");
				}
				repaint();
			}
		});
		return loggerSelecter;
	}

	/**
	 * This method creates and initializes the minimum log warning level combo
	 * box.
	 * 
	 * @param aFactory
	 *            A factory to use to create combo boxes.
	 * @return A minimum log warning level combo box.
	 */
	private JComboBox createMinLogLevelCombo(
			final DOMComboBoxController aFactory) {
		final JComboBox minLogLevel = aFactory.createComboBox(LEVEL_STRINGS,
				MIN_LOG_LEVEL);
		minLogLevel
				.setToolTipText(Messages.getString("ConfigurationEditor.87")); //$NON-NLS-1$
		// Disable the combo box until an item is selected.
		minLogLevel.setEnabled(false);
		return minLogLevel;
	}

	/**
	 * This method initializes the minimum log to screen combo box.
	 * 
	 * @param aFactory
	 *            A factory to use to create combo boxes.
	 * @return The minimum log to screen combo box.
	 */
	private JComboBox createMinScreenLogLevelCombo(
			final DOMComboBoxController aFactory) {
		final JComboBox minScreenLevel = aFactory.createComboBox(LEVEL_STRINGS,
				MIN_SCREEN_LEVEL);
		minScreenLevel.setToolTipText(Messages
				.getString("ConfigurationEditor.92")); //$NON-NLS-1$
		// Disable the combo box until an item is selected.
		minScreenLevel.setEnabled(false);
		return minScreenLevel;
	}

	/**
	 * This method creates and initializes the print log warning level checkbox.
	 * 
	 * @return The print log warning level checkbox.
	 */
	private JCheckBox createPrintLevelCheckBox() {
		final JCheckBox printLevel = new JCheckBox();
		final String parentXPath = "/" + LOG_ROOT + "/" + SINGLE_LOGGER;
		printLevel.setModel(new DOMButtonModel(this, parentXPath, PRINT_LEVEL,
				"", true));
		printLevel.setMnemonic(KeyEvent.VK_W);
		printLevel.setHorizontalTextPosition(SwingConstants.LEFT);
		printLevel.setText(Messages.getString("ConfigurationEditor.94")); //$NON-NLS-1$
		printLevel.setToolTipText(Messages.getString("ConfigurationEditor.95")); //$NON-NLS-1$
		// Disable the checkbox until an item is selected.
		printLevel.setEnabled(false);
		return printLevel;
	}

	/**
	 * Load a document into the current document.
	 * 
	 * @return The loaded document, null on failure.
	 */
private Document loadDocument() {
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
			return null;
		}
		final Document newDocument = FileUtils.loadDocument(this, new File(logConfPath), LOG_ROOT);
		if(newDocument != null){
		// Add an event handler which will listen for the document being
		// changed and set that the document needs to be saved.
		final EventTarget target = (EventTarget) newDocument.getDocumentElement();
		target.addEventListener("DOMSubtreeModified",
				new DOMDocumentSaveSetter(newDocument), true);
	}
		return newDocument;
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
}
