/*
 */
package configurationeditor;

import guicomponents.DOMListModel;
import guicomponents.DOMListPanel;
import guicomponents.DOMListPanelFactory;
import guihelpers.WindowCloseListener;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import org.w3c.dom.Document;

import utils.DOMUtils;
import utils.FileUtils;
import utils.Messages;

/**
 * Creates a window which has the capability to create or edit a batch file. The
 * selection of the batch file is done by the ConfigurationEditor and passed to
 * this class. The class is composed of three DOMFileListPanels, along with
 * confirmation and cancel buttons.
 * TODO: Make this extend panel and not editor.
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
	 * TODO: Pass in the File instead of the string.
	 */
	private void initialize(final String aFileName, final boolean aIsNewFile) {
		// Don't use the default closer.
		this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		this.addWindowListener(new WindowCloseListener());
		this.setContentPane(createContentPane());
		this.setTitle(Messages.getString("BatchFileEditor.21")); //$NON-NLS-1$
		
		// Load the document after the UI is created so all the listeners
		// are hooked up.
		final File newFile = new File(aFileName);
		if (aIsNewFile) {
			mDocument = FileUtils.createDocument(this, newFile, ROOT_ELEMENT_NAME);
		} else {
			// Try and load the document
			mDocument = FileUtils.loadDocument(this, newFile, ROOT_ELEMENT_NAME);
		}
		
		// Fire a property changed event that the document was switched.
		firePropertyChange("document-replaced", null, mDocument);
	}
}

