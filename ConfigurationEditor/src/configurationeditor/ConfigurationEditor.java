/**
 * 
 */
package configurationeditor;

import guicomponents.DOMButtonModel;
import guicomponents.DOMListPanelFactory;
import guicomponents.DOMTextFieldFactory;
import guicomponents.DOMTreeModel;
import guicomponents.FieldButtonEnabler;
import guicomponents.PopupMenuCreatorMouseAdapter;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.border.BevelBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.tree.TreeSelectionModel;

import org.w3c.dom.Document;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

import utils.Messages;
import utils.XMLFileFilter;
import actions.EditLogSettingsAction;
import actions.LoadAction;
import actions.NewAction;
import actions.QuitAction;
import actions.RunAction;
import actions.SaveAction;
import actions.ShowPreferencesAction;

/**
 * The class which creates the UI to edit a configuration file.
 * 
 * @author Josh Lurz
 */
public class ConfigurationEditor extends JFrame implements DOMDocumentEditor {
    /**
     * The name of the leaf elements containing configuration information.
     */
    private static final String ELEMENT_NAME = "Value";

    // TODO: Work out where invokeLater should be used.
    /**
     * Automatically generated unique class identifier.
     */
    private static final long serialVersionUID = -4743689341303656460L;

    /**
     * The current parsed XML document.
     */
    private transient Document mCurrentDocument = null;

    /**
     * The tree contained by the advanced panel which contains detailed
     * information about the configuration file.
     */
    private transient JTree mAdvancedTree = null;

    /**
     * The button on the toolbar which triggers the save action.
     */
    private transient JButton mSaveButton = null;

    /**
     * The button on the toolbar which triggers the run action.
     */
    private transient JButton mRunButton = null;

    /**
     * The Save file menu item.
     */
    private transient JMenuItem mSaveMenuItem = null;

    /**
     * The Save As file menu item.
     */
    private transient JMenuItem mSaveAsMenuItem = null;

    /**
     * The Run file menu item.
     */
    private transient JMenuItem mRunMenuItem = null;

    /**
     * The name of the root element which contains information in the DOM tree
     * about this object.
     */
    public static final String ROOT_ELEMENT_NAME = "Configuration"; //$NON-NLS-1$

    /**
     * A factory which creates and tracks text fields based on the current
     * document.
     */
    private transient DOMTextFieldFactory mTextFieldFactory = null;

    /**
     * A factory which creates list panels based on the current document.
     */
    private transient DOMListPanelFactory mListPanelFactory = null;

    /**
     * get rid of this advanced panel.
     */
    private transient JPanel mAdvancedPanel = null;

    // @jve:decl-index=0:visual-constraint=""
    /**
     * This is the default constructor
     */
    public ConfigurationEditor() {
        super();
        initialize();
    }

    /**
     * This method initializes the ConfigurationEditor.
     * 
     */
    private void initialize() {
        // Create the component factories.
        mTextFieldFactory = new DOMTextFieldFactory();
        mListPanelFactory = new DOMListPanelFactory();
        setJMenuBar(createMainMenuBar());
        setName(Messages.getString("ConfigurationEditor.0")); //$NON-NLS-1$
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new QuitAction(this));
        setContentPane(createMainWindow());
        setTitle(Messages.getString("ConfigurationEditor.1")); //$NON-NLS-1$
    }

    /**
     * Ask the user what action they would like to take to begin using the
     * editor. This is called from main.
     * 
     */
    public void askForInitialAction() {
        // First need to check if the preferences have been initializes.
        final File prefFile = new File(PropertiesInfo.PROPERTY_FILE);
        if (!prefFile.canRead()) {
            // Need to initialize the preferences window.
            new ShowPreferencesAction(this).actionPerformed(new ActionEvent(
                    this, ActionEvent.ACTION_PERFORMED, "ShowPreferences")); //$NON-NLS-1$
        }
        final Object[] options = {
                Messages.getString("ConfigurationEditor.58"), //$NON-NLS-1$
                Messages.getString("ConfigurationEditor.59"), Messages.getString("ConfigurationEditor.60") }; //$NON-NLS-1$ //$NON-NLS-2$
        final String message = Messages.getString("ConfigurationEditor.61"); //$NON-NLS-1$

        final int returnValue = JOptionPane.showOptionDialog(this, message,
                Messages.getString("ConfigurationEditor.62"), //$NON-NLS-1$
                JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null,
                options, options[0]);
        if (returnValue == 0) {
            new NewAction(this).actionPerformed(new ActionEvent(this,
                    ActionEvent.ACTION_PERFORMED, "New")); //$NON-NLS-1$
        } else if (returnValue == 1) {
            new LoadAction(this).actionPerformed(new ActionEvent(this,
                    ActionEvent.ACTION_PERFORMED, "Load")); //$NON-NLS-1$
        }

        // Check if for any reason the document is still not set and
        // warn the user that action is prevented until they load a
        // document or create a new one.
        if (mCurrentDocument == null) {
            final String warnMessage = Messages
                    .getString("ConfigurationEditor.64"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(this, warnMessage, Messages
                    .getString("ConfigurationEditor.65"), //$NON-NLS-1$
                    JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Set the current document.
     * 
     * @param aNewDocument
     *            The document to set as the current.
     */
    public void setDocument(final Document aNewDocument) {
        // Check for a blank document.
        if (aNewDocument == null) {
            Logger.global.log(Level.WARNING, "Tried to set a blank document.");
            return;
        }
        mCurrentDocument = aNewDocument;
        // Add an event handler which will listen for the document being
        // changed and set that the document needs to be saved.
        final EventTarget target = (EventTarget) mCurrentDocument
                .getDocumentElement();
        target.addEventListener("DOMSubtreeModified",
                new ConfigurationDocumentMutationListener(), true);

        // Update the user interface. Check if this is necessary.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // Set the document into the factories. This will
                // cause them to update the objects they created.
                mTextFieldFactory.setDocument(mCurrentDocument);
                mListPanelFactory.setDocument(mCurrentDocument);

                // Set the document into the advanced panel tree.
                createAdvancedTree();

                // enableBatchInputFields(mDoBatchModeCheckBox.isSelected());
                // Enable the run menu and button now that there is a document.
                getRunMenuItem().setEnabled(true);
                getRunButton().setEnabled(true);

                // Enable the save as menu item now that there is a document.
                getSaveAsMenuItem().setEnabled(true);
                repaint();
            }
        });
    }

    /**
     * Get the current document.
     * 
     * @return The current document.
     */
    public Document getDocument() {
        return mCurrentDocument;
    }

    /**
     * Returns that the user needs to be asked before the file is saved.
     * 
     * @return True, meaning that the user does need to be asked before saving
     *         the file.
     */
    public boolean askBeforeSaving() {
        return true;
    }

    /**
     * This method initializes the main window.
     * 
     * @return The main window.
     */
    private JPanel createMainWindow() {
        final JPanel mainWindow = new JPanel();
        mainWindow.setToolTipText(Messages.getString("ConfigurationEditor.3")); //$NON-NLS-1$
        mainWindow.setLayout(new BorderLayout());
        mainWindow.add(createMainToolBar(), BorderLayout.NORTH);
        mainWindow.add(createTabContainer(), BorderLayout.SOUTH);
        return mainWindow;
    }

    /**
     * This method initializes the main tab container.
     * 
     * @return The tabbed container.
     */
    private JTabbedPane createTabContainer() {
        final JTabbedPane tabContainer = new JTabbedPane();
        tabContainer
                .setToolTipText(Messages.getString("ConfigurationEditor.4")); //$NON-NLS-1$
        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.5"), null, createConfPanel(), Messages.getString("ConfigurationEditor.6")); //$NON-NLS-1$ //$NON-NLS-2$
        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.110"), null, createBatchPane(), Messages.getString("ConfigurationEditor.111")); //$NON-NLS-1$ //$NON-NLS-2$
        // Whats the difference between the two strings?
        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.7"), null, createAdvancedPanel(), Messages.getString("ConfigurationEditor.8")); //$NON-NLS-1$ //$NON-NLS-2$
        return tabContainer;
    }

    /**
     * This method initializes the configuration panel.
     * 
     * @return The configuration panel.
     */
    private JPanel createConfPanel() {
        final JPanel confPanel = new JPanel(new GridBagLayout());
        confPanel
                .setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));

        // Now setup the layout constraints.
        final GridBagConstraints cons = new GridBagConstraints();

        // Layout the items top to bottom, left to right.
        // Position the checkboxes in the first column.
        cons.gridx = 0;

        // Position components on the left side of their cells.
        cons.anchor = GridBagConstraints.WEST;

        // Add panels in increasing rows.
        cons.gridy = 0;

        // Put a border of 5 around the entire panel.
        cons.insets = new Insets(5, 5, 5, 5);

        confPanel.add(createCalibrationCheckBox(), cons);

        cons.gridy = GridBagConstraints.RELATIVE;
        confPanel.add(createCalcCostCheckBox(), cons);

        // Now add the labels in column 1.
        cons.gridx = 1;
        cons.gridy = 0;

        // Create a label for the scenario name field.
        final JLabel scenNameLabel = new JLabel(Messages
                .getString("ConfigurationEditor.112")); //$NON-NLS-1$
        confPanel.add(scenNameLabel, cons);

        // Put the labels in increasing rows.
        cons.gridy = GridBagConstraints.RELATIVE;

        // Create a label for the input file field.
        final JLabel inputFileLabel = new JLabel(Messages
                .getString("ConfigurationEditor.115")); //$NON-NLS-1$
        confPanel.add(inputFileLabel, cons);

        // Create a label for the output file field.
        final JLabel outputFileLabel = new JLabel(Messages
                .getString("ConfigurationEditor.118")); //$NON-NLS-1$
        confPanel.add(outputFileLabel, cons);

        // Add the text fields in column 2.
        cons.gridx = 2;
        cons.gridy = 0;

        // Allow the text field to resize with the items.
        cons.weightx = 1;
        cons.fill = GridBagConstraints.HORIZONTAL;

        // Add the scenario name field.
        final JTextField scenNameField = mTextFieldFactory.createTextField(
                "Strings", "scenarioName"); //$NON-NLS-1$ //$NON-NLS-2$
        scenNameField.setPreferredSize(new Dimension(200, 20));
        confPanel.add(scenNameField, cons);

        // Put the fields in increasing rows.
        cons.gridy = GridBagConstraints.RELATIVE;

        // Add the input field.
        final JTextField inputFileField = mTextFieldFactory.createTextField(
                "Files", "xmlInputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        inputFileField.setPreferredSize(new Dimension(200, 20));
        confPanel.add(inputFileField, cons);

        // Add the output file field.
        final JTextField outputFileField = mTextFieldFactory.createTextField(
                "Files", "xmlOutputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        outputFileField.setPreferredSize(new Dimension(200, 20));
        confPanel.add(outputFileField, cons);

        // Put the list panel in column 3.
        cons.gridx = 3;
        cons.gridy = 0;
        // Make the panel absorb 4 rows.
        cons.gridheight = 4;
        cons.weightx = 1;
        // Make the panel take up the entire height.
        cons.fill = GridBagConstraints.BOTH;
        // Make the panel absorb vertical space.
        cons.weighty = 1;

        // Add the list panel.
        final JPanel addOnPanel = mListPanelFactory
                .createDOMFileListPanel(
                        "/Configuration/ScenarioComponents", "ScenarioComponent", Messages.getString("ConfigurationEditor.151"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        confPanel.add(addOnPanel, cons);
        return confPanel;
    }

    /**
     * This method initializes the advanced preferences panel.
     * 
     * @return The advanced panel.
     */
    private JPanel createAdvancedPanel() {
        mAdvancedPanel = new JPanel();
        mAdvancedPanel.setBorder(BorderFactory
                .createBevelBorder(BevelBorder.RAISED));
        // Create a new tree using a DOM based model. The model is
        // initialized
        // with the root node of the document and name of leaf items. If
        // there
        // isn't a document yet this will be deferred.
        if (mCurrentDocument != null) {
            createAdvancedTree();
        }
        return mAdvancedPanel;
    }

    /**
     * Create and display the advanced editing tree.
     */
    private void createAdvancedTree() {
        // Remove the existing tree from the panel.
        if (mAdvancedTree != null) {
            mAdvancedPanel.remove(mAdvancedTree);
        }
        if (mCurrentDocument != null) {
            mAdvancedTree = new JTree(new DOMTreeModel(mCurrentDocument
                    .getDocumentElement(), ELEMENT_NAME));
            mAdvancedTree.setVisible(true);
            mAdvancedTree.setShowsRootHandles(true);
            mAdvancedTree.setRootVisible(true);
            mAdvancedTree.getSelectionModel().setSelectionMode(
                    TreeSelectionModel.SINGLE_TREE_SELECTION);

            // Add a mouse listener so that a menu can be
            // displayed on right click.
            mAdvancedTree.addMouseListener(new PopupMenuCreatorMouseAdapter());
            // Create a scroll pane to display the tree.
            final JScrollPane treeScroller = new JScrollPane(mAdvancedTree);
            treeScroller.setPreferredSize(new Dimension(600, 250));
            mAdvancedPanel.add(treeScroller);
        }
    }

    /**
     * This method initializes the do calibration checkbox.
     * 
     * @return The do calibration checkbox.
     */
    private JCheckBox createCalibrationCheckBox() {
        final JCheckBox calCheckBox = new JCheckBox(Messages
                .getString("ConfigurationEditor.13")); //$NON-NLS-1$
        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        calCheckBox.setModel(new DOMButtonModel(this, parentXPath,
                ELEMENT_NAME, "CalibrationActive", false)); //$NON-NLS-1$
        calCheckBox.setMnemonic(KeyEvent.VK_C);
        calCheckBox
                .setToolTipText(Messages.getString("ConfigurationEditor.14")); //$NON-NLS-1$
        return calCheckBox;
    }

    /**
     * This method initializes the run consts curves check box.
     * 
     * @return The run cost curves check box.
     */
    private JCheckBox createCalcCostCheckBox() {
        final JCheckBox calcCosts = new JCheckBox(Messages
                .getString("ConfigurationEditor.128")); //$NON-NLS-1$

        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        calcCosts.setModel(new DOMButtonModel(this, parentXPath, ELEMENT_NAME,
                "createCostCurve", false)); //$NON-NLS-1$
        calcCosts.setMnemonic(KeyEvent.VK_O);
        calcCosts.setToolTipText(Messages.getString("ConfigurationEditor.129")); //$NON-NLS-1$
        return calcCosts;
    }

    /**
     * This method initializes the do batch mode check box.
     * 
     * @return The batch mode checkbox.
     */
    private JCheckBox createBatchCheckBox() {
        final JCheckBox batchCheckBox = new JCheckBox(Messages
                .getString("ConfigurationEditor.16")); //$NON-NLS-1$

        final String parentXPath = "/" + ConfigurationEditor.ROOT_ELEMENT_NAME
                + "/Bools";
        batchCheckBox.setModel(new DOMButtonModel(this, parentXPath,
                ELEMENT_NAME, "BatchMode", false)); //$NON-NLS-1$
        batchCheckBox.setMnemonic(KeyEvent.VK_B);
        batchCheckBox.setToolTipText(Messages
                .getString("ConfigurationEditor.17")); //$NON-NLS-1$

        return batchCheckBox;
    }

    /**
     * This method initializes the main toolbar.
     * 
     * @return The main toolbar.
     */
    private JToolBar createMainToolBar() {
        final JToolBar mainToolBar = new JToolBar();
        mainToolBar
                .setToolTipText(Messages.getString("ConfigurationEditor.19")); //$NON-NLS-1$
        mainToolBar.add(createNewButton());
        mainToolBar.add(createLoadButton());
        mainToolBar.add(getSaveButton());
        mainToolBar.add(getRunButton());
        return mainToolBar;
    }

    /**
     * This method initializes the main menubar.
     * 
     * @return The main menu bar.
     */
    private JMenuBar createMainMenuBar() {
        final JMenuBar mainMenuBar = new JMenuBar();
        mainMenuBar
                .setToolTipText(Messages.getString("ConfigurationEditor.21")); //$NON-NLS-1$
        mainMenuBar.add(createFileMenu());
        mainMenuBar.add(createEditMenu());
        mainMenuBar.add(createHelpMenu());
        return mainMenuBar;
    }

    /**
     * This method initializes the file menu.
     * 
     * @return The file menu.
     */
    private JMenu createFileMenu() {
        final JMenu fileMenu = new JMenu(Messages
                .getString("ConfigurationEditor.24")); //$NON-NLS-1$
        fileMenu.setToolTipText(Messages.getString("ConfigurationEditor.25")); //$NON-NLS-1$
        fileMenu.setMnemonic(KeyEvent.VK_F);

        // Add the menu items.
        fileMenu.add(createNewMenuItem());
        fileMenu.add(createLoadMenuItem());
        fileMenu.add(getSaveMenuItem());
        fileMenu.add(getSaveAsMenuItem());
        fileMenu.add(getRunMenuItem());
        fileMenu.add(createMergeMenuItem());
        fileMenu.add(createQuitMenuItem());
        return fileMenu;
    }

    /**
     * This method initializes the save button.
     * 
     * @return The save button.
     */
    private JButton getSaveButton() {
        if (mSaveButton == null) {
            mSaveButton = new JButton();
            mSaveButton.setActionCommand("Save"); //$NON-NLS-1$
            mSaveButton.setAction(new SaveAction(this));
            mSaveButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.29")); //$NON-NLS-1$
            mSaveButton.setText(Messages.getString("ConfigurationEditor.30")); //$NON-NLS-1$
            // Initially disable the save button until a change has been made.
            mSaveButton.setEnabled(false);
        }
        return mSaveButton;
    }

    /**
     * This method initializes the new button.
     * 
     * @return The new button.
     */
    private JButton createNewButton() {
        final JButton newButton = new JButton(Messages
                .getString("ConfigurationEditor.32")); //$NON-NLS-1$
        newButton.setAction(new NewAction(this));
        newButton.setToolTipText(Messages.getString("ConfigurationEditor.33")); //$NON-NLS-1$
        newButton.setMnemonic(KeyEvent.VK_N);
        return newButton;
    }

    /**
     * This method initializes the load button.
     * 
     * @return The load button.
     */
    private JButton createLoadButton() {
        final JButton loadButton = new JButton(Messages
                .getString("ConfigurationEditor.36")); //$NON-NLS-1$
        loadButton.setAction(new LoadAction(this));
        loadButton.setToolTipText(Messages.getString("ConfigurationEditor.37")); //$NON-NLS-1$
        loadButton.setMnemonic(KeyEvent.VK_O);
        return loadButton;
    }

    /**
     * This method initializes the run button.
     * 
     * @return The run button.
     */
    private JButton getRunButton() {
        if (mRunButton == null) {
            mRunButton = new JButton();
            mRunButton.setAction(new RunAction(this));
            mRunButton.setMnemonic(KeyEvent.VK_R);
            mRunButton.setText(Messages.getString("ConfigurationEditor.38")); //$NON-NLS-1$
            mRunButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.39")); //$NON-NLS-1$
            // Disable the run button until a document is loaded.
            mRunButton.setEnabled(false);
        }
        return mRunButton;
    }

    /**
     * This method initializes the quit menu item.
     * 
     * @return The quit menu item.
     */
    private JMenuItem createQuitMenuItem() {
        final JMenuItem quitItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.41"));
        quitItem.setAction(new QuitAction(this));
        quitItem.setMnemonic(KeyEvent.VK_Q);
        quitItem.setToolTipText(Messages.getString("ConfigurationEditor.42")); //$NON-NLS-1$
        return quitItem;
    }

    /**
     * This method initializes the new menu item.
     * 
     * @return The new menu item.
     */
    private JMenuItem createNewMenuItem() {
        final JMenuItem newItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.44"));
        newItem.setAction(new NewAction(this));
        newItem.setMnemonic(KeyEvent.VK_N);
        newItem.setToolTipText(Messages.getString("ConfigurationEditor.45")); //$NON-NLS-1$
        return newItem;
    }

    /**
     * This method initializes the load menu item.
     * 
     * @return The load menu item.
     */
    private JMenuItem createLoadMenuItem() {
        final JMenuItem loadItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.48"));
        loadItem.setAction(new LoadAction(this));
        loadItem.setToolTipText(Messages.getString("ConfigurationEditor.49")); //$NON-NLS-1$
        loadItem.setMnemonic(KeyEvent.VK_O);
        return loadItem;
    }

    /**
     * This method initializes the save menu item.
     * 
     * @return The save menu item.
     */
    private JMenuItem getSaveMenuItem() {
        if (mSaveMenuItem == null) {
            mSaveMenuItem = new JMenuItem();
            mSaveMenuItem.setActionCommand("Save"); //$NON-NLS-1$
            mSaveMenuItem.setMnemonic(KeyEvent.VK_S);
            mSaveMenuItem.setAction(new SaveAction(this));
            mSaveMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.50")); //$NON-NLS-1$
            mSaveMenuItem.setEnabled(false);
            mSaveMenuItem.setText(Messages.getString("ConfigurationEditor.51")); //$NON-NLS-1$
        }
        return mSaveMenuItem;
    }

    /**
     * This method initializes the save as menu item.
     * 
     * @return The save as menu item.
     */
    private JMenuItem getSaveAsMenuItem() {
        if (mSaveAsMenuItem == null) {
            mSaveAsMenuItem = new JMenuItem();
            mSaveAsMenuItem.setAction(new SaveAction(this));
            mSaveAsMenuItem.setText(Messages
                    .getString("ConfigurationEditor.53")); //$NON-NLS-1$
            mSaveAsMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.54")); //$NON-NLS-1$
            mSaveAsMenuItem.setActionCommand("SaveAs"); //$NON-NLS-1$
            mSaveAsMenuItem.setMnemonic(KeyEvent.VK_A);
            // Initially disable the save as menu item until the user
            // loads a file.
            mSaveAsMenuItem.setEnabled(false);
        }
        return mSaveAsMenuItem;
    }

    /**
     * This method initializes the run menu item.
     * 
     * @return The run menu item.
     */
    private JMenuItem getRunMenuItem() {
        if (mRunMenuItem == null) {
            mRunMenuItem = new JMenuItem();
            mRunMenuItem.setAction(new RunAction(this));
            mRunMenuItem.setText(Messages.getString("ConfigurationEditor.56")); //$NON-NLS-1$
            mRunMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.57")); //$NON-NLS-1$
            mRunMenuItem.setMnemonic(KeyEvent.VK_R);
            // Initially disable the run menu item until the user
            // loads a file.
            mRunMenuItem.setEnabled(false);
        }
        return mRunMenuItem;
    }

    /**
     * This method creates and initializes the edit menu.
     * 
     * @return The edit menu.
     */
    private JMenu createEditMenu() {
        final JMenu helpMenu = new JMenu(Messages
                .getString("ConfigurationEditor.73")); //$NON-NLS-1$
        helpMenu.setMnemonic(KeyEvent.VK_E);
        helpMenu.setToolTipText(Messages.getString("ConfigurationEditor.74")); //$NON-NLS-1$
        helpMenu.add(createPreferencesMenuItem());
        helpMenu.add(createEditLogsMenuItem());
        return helpMenu;
    }

    /**
     * This method creates and initializes the preferences menu item.
     * 
     * @return The preferences menu item.
     */
    private JMenuItem createPreferencesMenuItem() {
        final JMenuItem prefItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.77")); //$NON-NLS-1$
        prefItem.setActionCommand("ShowPreferences"); //$NON-NLS-1$
        prefItem.setToolTipText(Messages.getString("ConfigurationEditor.78")); //$NON-NLS-1$
        prefItem.setMnemonic(KeyEvent.VK_P);
        return prefItem;
    }

    /**
     * This method initializes the edit logs menu item.
     * 
     * @return The edit logs menu item.
     */
    private JMenuItem createEditLogsMenuItem() {
        final JMenuItem editLogs = new JMenuItem(Messages
                .getString("ConfigurationEditor.136")); //$NON-NLS-1$
        editLogs.setAction(new EditLogSettingsAction());
        editLogs.setToolTipText(Messages.getString("ConfigurationEditor.137")); //$NON-NLS-1$
        editLogs.setMnemonic(KeyEvent.VK_L);
        return editLogs;
    }

    /**
     * This method initializes the batch editor pane.
     * 
     * @return The pane containing all batch configuration information.
     */
    private JPanel createBatchPane() {
        final JPanel batchPanel = new JPanel();
        batchPanel.setBorder(BorderFactory
                .createBevelBorder(BevelBorder.RAISED));
        batchPanel
                .setToolTipText(Messages.getString("ConfigurationEditor.138")); //$NON-NLS-1$
        // Create all the batch file elements so the listener
        // can access them.
        final JLabel label = new JLabel(Messages
                .getString("ConfigurationEditor.124")); //$NON-NLS-1$

        final JTextField fileField = createBatchFileField();
        final JButton editButton = createBatchFileEditButton(fileField);
        // Add an action listener to enable the edit button. This
        // has to be done outside the create method to avoid a cycle.
        fileField.getDocument().addDocumentListener(
                new FieldButtonEnabler(editButton));
        final JButton selectButton = createBatchFileSelectButton(fileField);
        final JButton newButton = createBatchFileNewButton(fileField);
        final JCheckBox doBatchMode = createBatchCheckBox();

        // Add a listener which will enable and disable the batch
        // mode fields.
        doBatchMode.addChangeListener(new ChangeListener() {
            public void stateChanged(final ChangeEvent aEvent) {
                // Enable the fields according to the value of the checkbox.
                final boolean enabled = doBatchMode.isSelected();
                label.setEnabled(enabled);
                fileField.setEnabled(enabled);
                selectButton.setEnabled(enabled);
                newButton.setEnabled(enabled);
                // Only enable the batch file edit button if a file name has
                // been
                // set.
                editButton.setEnabled(enabled
                        && fileField.getText().length() > 0);
            }

        });

        // Add the items.
        // TODO: Layout these.
        batchPanel.add(doBatchMode, null);
        batchPanel.add(label, null);
        batchPanel.add(fileField, null);
        batchPanel.add(selectButton, null);
        batchPanel.add(editButton, null);
        batchPanel.add(newButton, null);

        return batchPanel;
    }

    /**
     * This method initializes the merge menu item.
     * 
     * @return The merge menu item.
     */
    private JMenuItem createMergeMenuItem() {
        final JMenuItem mergeItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.98")); //$NON-NLS-1$
        mergeItem.setToolTipText(Messages.getString("ConfigurationEditor.99")); //$NON-NLS-1$
        mergeItem.setEnabled(false);
        mergeItem.setMnemonic(KeyEvent.VK_M);
        return mergeItem;
    }

    /**
     * This method initializes the help menu.
     * 
     * @return The help menu.
     */
    private JMenu createHelpMenu() {
        final JMenu helpMenu = new JMenu(Messages
                .getString("ConfigurationEditor.100")); //$NON-NLS-1$
        helpMenu.setMnemonic(KeyEvent.VK_H);
        helpMenu.setToolTipText(Messages.getString("ConfigurationEditor.101")); //$NON-NLS-1$
        return helpMenu;
    }

    /**
     * Dispatch the save event. This isn't good.
     */
    public void dispatchSave() {
        new SaveAction(this).actionPerformed(new ActionEvent(this,
                ActionEvent.ACTION_PERFORMED, Messages
                        .getString("ConfigurationEditor.103"))); //$NON-NLS-1$
    }

    /**
     * This method initializes the batch file text field.
     * 
     * @return The batch file text field.
     */
    private JTextField createBatchFileField() {
        final JTextField fileField = mTextFieldFactory.createTextField(
                "Files", "BatchFileName"); //$NON-NLS-1$ //$NON-NLS-2$
        fileField.setPreferredSize(new Dimension(200, 20));
        fileField.setToolTipText(Messages.getString("ConfigurationEditor.141")); //$NON-NLS-1$
        return fileField;
    }

    /**
     * This method initializes the batch file select button.
     * 
     * @param aFileField
     *            The file field for which this button is selection.
     * @return The batch file select button.
     */
    private JButton createBatchFileSelectButton(final JTextField aFileField) {
        final JButton selectButton = new JButton(Messages
                .getString("ConfigurationEditor.142")); //$NON-NLS-1$
        selectButton.setToolTipText(Messages
                .getString("ConfigurationEditor.143")); //$NON-NLS-1$
        selectButton.setMnemonic(KeyEvent.VK_S);
        final Frame parentWindow = this;
        selectButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                final JFileChooser batchFileChooser = new JFileChooser();
                batchFileChooser.setFileFilter(new XMLFileFilter());
                final int returnValue = batchFileChooser.showDialog(
                        parentWindow, Messages
                                .getString("ConfigurationEditor.144")); //$NON-NLS-1$
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    final File selectedFile = batchFileChooser
                            .getSelectedFile();
                    if (selectedFile != null) {
                        // Set the text field.
                        aFileField.setText(selectedFile.getAbsolutePath());
                    }
                }
            }
        });
        return selectButton;
    }

    /**
     * This method initializes the new batch file button.
     * 
     * @param aTextField
     *            Text field in which to save the new file selected.
     * @return The new batch file button.
     */
    private JButton createBatchFileNewButton(final JTextField aTextField) {
        final JButton newButton = new JButton(Messages
                .getString("ConfigurationEditor.145")); //$NON-NLS-1$
        newButton.setToolTipText(Messages.getString("ConfigurationEditor.146")); //$NON-NLS-1$
        newButton.setMnemonic(KeyEvent.VK_N);

        final Frame parent = this;
        // Create an input field, dispatch the editor.
        newButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                // Create a file chooser and add an XML filter.
                final JFileChooser chooser = new JFileChooser();
                chooser.setFileFilter(new XMLFileFilter());

                // Show the file chooser.
                final int returnValue = chooser.showSaveDialog(parent);
                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    // TODO: Threading here is wrong.
                    final File newFile = chooser.getSelectedFile();
                    aTextField.setText(newFile.getAbsolutePath());
                    SwingUtilities.invokeLater(new BatchEditorCreator(newFile,
                            true));
                }
            }
        });
        return newButton;
    }

    /**
     * This method initializes the batch file edit button.
     * 
     * @param aTextField
     *            Text field in which to save the new file selected.
     * @return The batch file edit button.
     */
    private JButton createBatchFileEditButton(final JTextField aTextField) {
        final JButton editButton = new JButton(Messages
                .getString("ConfigurationEditor.149")); //$NON-NLS-1$
        editButton
                .setToolTipText(Messages.getString("ConfigurationEditor.150")); //$NON-NLS-1$
        editButton.setMnemonic(KeyEvent.VK_E);

        editButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent aEvent) {
                // Create a new batch file editor.
                final File batchFile = new File(aTextField.getText());
                SwingUtilities.invokeLater(new BatchEditorCreator(batchFile,
                        false));
            }
        });
        return editButton;
    }

    /**
     * Runnable that creates and displays the batch file editor.
     * 
     * @author Josh Lurz
     */
    private final class BatchEditorCreator implements Runnable {

        /**
         * The file for the batch file editor to open.
         */
        private transient final File mFile;

        /**
         * Whether to launch the batch file editor in new file creation mode.
         */
        private transient final boolean mCreateNewFile;

        /**
         * Constructor
         * 
         * @param aFile
         *            File to edit.
         * @param aCreateNewFile
         *            Whether to launch the batch file editor in new file
         *            creation mode.
         */
        public BatchEditorCreator(File aFile, final boolean aCreateNewFile) {
            super();
            mFile = aFile;
            mCreateNewFile = aCreateNewFile;
        }

        /**
         * Run method which launches the editor if the file is valid.
         * 
         * @see java.lang.Runnable#run()
         */
        public void run() {
            final BatchFileEditor batchEditor = new BatchFileEditor(mFile
                    .getAbsolutePath(), mCreateNewFile);
            // Check if the batch file editor can be shown.
            if (batchEditor.isValidEditor()) {
                batchEditor.pack();
                batchEditor.setVisible(true);
            }
        }
    }

    /**
     * This mutation listener watches for events which change the underlying
     * document. When a mutation event is received the dirty attribute is set on
     * the document and the save menu and button are activated. TODO: Use this
     * elsewhere.
     * 
     * @author Josh Lurz
     */
    private final class ConfigurationDocumentMutationListener implements
            EventListener {
        /**
         * Constructor
         */
        ConfigurationDocumentMutationListener() {
            super();
        }

        /**
         * Method called when mutation events from the configuration document
         * are received.
         * 
         * @param aEvent
         *            The mutation event received.
         */
        public void handleEvent(final Event aEvent) {
            // This doesn't recursively send another event,
            // not sure why but it works.
            mCurrentDocument.getDocumentElement().setAttribute(
                    "needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            // Update the user interface. Check if this is necessary.
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    // Enable the save menu item.
                    getSaveMenuItem().setEnabled(true);
                    getSaveButton().setEnabled(true);
                }
            });
        }
    }
} // @jve:decl-index=0:visual-constraint="50,28"

