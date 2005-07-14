/*
 */
package configurationeditor;

import guicomponents.DOMListModel;
import guicomponents.DOMListPanel;
import guicomponents.DOMListPanelFactory;
import guihelpers.WindowCloseListener;

import javax.swing.JFrame;
import org.w3c.dom.Document;

import utils.DOMUtils;
import utils.Messages;
import utils.FileUtils;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Creates a window which has the capability to create or edit a batch file. The
 * selection of the batch file is done by the ConfigurationEditor and passed to
 * this class. The class is composed of three DOMFileListPanels, along with
 * confirmation and cancel buttons.
 * 
 * @author Josh Lurz
 */
public class BatchFileEditor extends JFrame implements DOMDocumentEditor {
	/**
	 * Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = 5932800223359767162L;

	/**
	 * The parsed batch file XML document.
	 */
	private transient Document mDocument = null;

	/**
	 * The name of the batch file root element.
	 */
	private static final String ROOT_ELEMENT_NAME = "ComponentSets"; //$NON-NLS-1$
	
	/**
	 * This is the default constructor
	 * 
	 * @param aInitialFile
	 *            The file to open immediately in the editor.
	 * @param aIsNewFile
	 *            Whether this is an existing or new file.
	 */
	public BatchFileEditor(String aInitialFile, boolean aIsNewFile) {
		super();
		initialize(aInitialFile, aIsNewFile);
	}

	/**
	 * Returns whether the editor is in a state ready to be displayed.
	 * @return Whether the editor is ready to be displayed.
	 */
	public boolean isValidEditor(){
		return mDocument != null;
	}
	
	/**
	 * Get the current document.
	 * 
	 * @return The current document.
	 */
	public Document getDocument() {
		return mDocument;
	}

	/**
	 * Returns that the user does not need to be asked before the file is saved.
	 * 
	 * @return false, meaning that the user does not need to be asked before
	 *         saving the file.
	 */
	public boolean askBeforeSaving() {
		return false;
	}

	/**
	 * Create a new document and set it is the current document.
	 * 
	 * @param aFileName
	 *            The name to save the document as.
	 * @param aParent
	 *            The parent frame to use to center error messages.
	 * @return A newly constructed batch file document with the root node set.
	 */
	private static Document createNewDocument(final String aFileName,
			final Window aParent) {
		// Check if the new file already exists.
		final File newFile = new File(aFileName);
		if (newFile.exists()) {
			final String errorMessage = Messages
					.getString("ConfigurationEditor.147"); //$NON-NLS-1$
			final String errorTitle = Messages
					.getString("ConfigurationEditor.148"); //$NON-NLS-1$
			JOptionPane.showMessageDialog(aParent,
					errorMessage, errorTitle,
					JOptionPane.ERROR_MESSAGE);
			return null;
		}
		
		// Create a new document.
		final Document newDocument = DOMUtils.getDocumentBuilder(aParent)
				.newDocument();

		// Add the root element onto it.
		newDocument.appendChild(newDocument.createElement(ROOT_ELEMENT_NAME));

		// Set the name of the file into the document.
		FileUtils.setDocumentFile(newDocument, new File(aFileName));

		// Put up a message telling the user that a new file was created,
		// otherwise there is no feedback for this action.
		final String message = Messages.getString("BatchFileEditor.1"); //$NON-NLS-1$
		final String messageTitle = Messages.getString("BatchFileEditor.2"); //$NON-NLS-1$
		Logger.global.log(Level.INFO, message);
		JOptionPane.showMessageDialog(aParent, message, messageTitle,
				JOptionPane.INFORMATION_MESSAGE);
		return newDocument;
	}

	/**
	 * Load a document into the current document.
	 * 
	 * @param aFileName
	 *            The path to a file to load.
	 * @param aParent
	 *            The parent window to use to display error messages.
	 * @return The loaded document.
	 */
	private static Document loadDocument(final String aFileName,
			final Window aParent) {
		Document newDocument = null;
		try {
			final File newFile = new File(aFileName);
			// Check if the file exists.
			if (!newFile.exists()) {
				// Tell the user the file does not exist.
				final String message = Messages.getString("BatchFileEditor.3"); //$NON-NLS-1$
				final String messageTitle = Messages
						.getString("BatchFileEditor.4"); //$NON-NLS-1$
				Logger.global.log(Level.SEVERE, message);
				JOptionPane.showMessageDialog(aParent, message, messageTitle,
						JOptionPane.ERROR_MESSAGE);
				return null;
			}
			newDocument = DOMUtils.getDocumentBuilder(aParent).parse(aFileName);
			FileUtils.setDocumentFile(newDocument, newFile);
		} catch (Exception e) {
			// Report the error to the user.
			final String message = Messages.getString("BatchFileEditor.5") + e.getMessage(); //$NON-NLS-1$
			final String messageTitle = Messages.getString("BatchFileEditor.6"); //$NON-NLS-1$
			Logger.global.log(Level.SEVERE, message);
			JOptionPane.showMessageDialog(aParent, message, messageTitle,
					JOptionPane.ERROR_MESSAGE);
			return null;
		}
		// Check if this has any chance at being a valid batch file.
		if (newDocument != null
				&& !newDocument.getDocumentElement().getNodeName().equals(
						ROOT_ELEMENT_NAME)) {
			final String message = "The selected document is not a valid batch file.";
			final String messageTitle = "Invalid Document";
			Logger.global.log(Level.SEVERE, message);
			JOptionPane.showMessageDialog(aParent, message, messageTitle,
					JOptionPane.ERROR_MESSAGE);
			return null;
		}
		return newDocument;
	}

	/**
	 * This method initializes the main content pane.
	 * 
	 * @return The main content pane.
	 */
	private JPanel createContentPane() {
		final JPanel contentPane = new JPanel(new GridBagLayout());

		final GridBagConstraints cons = new GridBagConstraints();
		// Set that the panels should grow to fit the cells.
		cons.fill = GridBagConstraints.BOTH;

		// Position the panels in increasing columns.
		cons.gridx = GridBagConstraints.RELATIVE;

		// Add panels in the first row.
		cons.gridy = 0;

		// Put a border of 5 around the entire panel.
		cons.insets = new Insets(5, 5, 5, 5);

		// Weight the panels so they grow when the window is resized
		// horizontally
		// but not vertically.
		cons.weightx = 1;

		// Center the panels in their cells.
		cons.anchor = GridBagConstraints.CENTER;
		
		// Create the list panel factory and add it as a property 
		// change listener so it will know when the document changes.
		final DOMListPanelFactory factory = new DOMListPanelFactory();
		addPropertyChangeListener(factory);
		// Initialize panels in right to left order so that
		// the panel which depends on another panel is always created
		// before the panel that it depends on.
		final JPanel rightPanel = createRightPanel(factory);
		final JPanel middlePanel = createMiddlePanel(rightPanel, factory);
		final JPanel leftPanel = createLeftPanel(middlePanel, factory);
		contentPane.add(leftPanel, cons);
		contentPane.add(middlePanel, cons);
		
		// Initialize list parents.
		// TODO: Find a better way to do this. This is incredibly ugly.
		((DOMListModel)((DOMListPanel) middlePanel).getList(null).getModel()).setParentList(
				((DOMListPanel) leftPanel).getList(null));
		((DOMListModel)((DOMListPanel) rightPanel).getList(null).getModel()).setParentList(
				((DOMListPanel) middlePanel).getList(null));
		
		// Put the right pane in 2 cells to help position the buttons.
		cons.gridwidth = 2;
		contentPane.add(rightPanel, cons);

		// Add okay and cancel buttons to the content pane.
		// Don't allow the buttons to fill the cells.

		// Add an OK button.
		cons.gridx = 3;
		cons.gridy = 1;
		cons.weightx = 0;
		cons.fill = GridBagConstraints.NONE;
		cons.gridwidth = 1;
		cons.anchor = GridBagConstraints.EAST;

		final JButton okButton = new JButton();
		okButton.setToolTipText(Messages.getString("BatchFileEditor.10")); //$NON-NLS-1$
		okButton.setText(Messages.getString("BatchFileEditor.11")); //$NON-NLS-1$

		// Add a listener which will save the batch file and close the
		// window.
		// Must be a way to avoid this.
		final JFrame parent = this;
		okButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				// Save the batch file document.
				DOMUtils.serialize(mDocument, parent);
				parent.dispose();
			}
		});
		contentPane.add(okButton, cons);

		final JButton cancelButton = new JButton();
		cancelButton.setToolTipText(Messages.getString("BatchFileEditor.8")); //$NON-NLS-1$
		cancelButton.setText(Messages.getString("BatchFileEditor.9")); //$NON-NLS-1$

		// Add a listener which will close the window.
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent aEvent) {
				parent.dispose();
			}
		});
		cons.gridx = 4;
		contentPane.add(cancelButton, cons);
		return contentPane;
	}

	/**
	 * This method creates and initializes the left panel.
	 * @param aDependentPanel The panel which has values that depend on the selected
	 * value of this panel.
	 * @param aFactory Factory which will create the list panel.
	 * @return The left panel.
	 */
	private JPanel createLeftPanel(final JPanel aDependentPanel, final DOMListPanelFactory aFactory) {
		final DOMListPanel leftPanel = aFactory.createDOMFileListPanel(
					"/ComponentSets", "ComponentSet", "Component Sets", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		// TODO: This is very fragile.	
		leftPanel.getList(null).addListSelectionListener((DOMListModel)
					((DOMListPanel) aDependentPanel).getList(null).getModel());
		leftPanel.getList(null).getModel().addListDataListener((DOMListModel)
					((DOMListPanel) aDependentPanel).getList(null).getModel());
		return leftPanel;
	}

	/**
	 * This method creates and initializes the middle panel.
	 * @param aDependentPanel The panel which has values that depend on the selected
	 * value of this panel.
	 * @param aFactory Factory which will create the list panel.
	 * @return The middle panel.
	 */
	private JPanel createMiddlePanel(final JPanel aDependentPanel, final DOMListPanelFactory aFactory) {

		final DOMListPanel middlePanel = aFactory.createDOMFileListPanel(
					"ComponentSet", "FileSet", "File Sets", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			middlePanel.getList(null).addListSelectionListener((DOMListModel)
					((DOMListPanel)aDependentPanel).getList(null).getModel());
			middlePanel.getList(null).getModel().addListDataListener((DOMListModel)
					((DOMListPanel)aDependentPanel).getList(null).getModel());
		return middlePanel;
	}

	/**
	 * This method initializes the right panel.
	 * @param aFactory Factory which will create the list panel.
	 * @return The right panel.
	 */
	private JPanel createRightPanel(final DOMListPanelFactory aFactory) {
		final JPanel rightPanel = aFactory
					.createDOMFileListPanel(
							"FileSet", Messages.getString("BatchFileEditor.19"), Messages.getString("BatchFileEditor.20"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		return rightPanel;
	}

	/**
	 * This method initializes the main batch editor window.
	 * 
	 * @param aFileName
	 *            The name of the file to load or create.
	 * @param aIsNewFile
	 *            Whether the file should be created.
	 */
	private void initialize(final String aFileName, final boolean aIsNewFile) {
		if (aIsNewFile) {
			mDocument = createNewDocument(aFileName, this);
		} else {
			// Try and load the document
			mDocument = loadDocument(aFileName, this);
		}
		
		// Fire a property changed event that the document was switched.
		firePropertyChange("document-replaced", null, mDocument);
		
		// Don't use the default closer.
		this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		this.addWindowListener(new WindowCloseListener());
		this.setContentPane(createContentPane());
		this.setTitle(Messages.getString("BatchFileEditor.21")); //$NON-NLS-1$
	}
} // @jve:decl-index=0:visual-constraint="13,20"

