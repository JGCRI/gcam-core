/**
 * 
 */
package configurationeditor;

import guicomponents.DOMButtonModel;
import guicomponents.DOMListPanel;
import guicomponents.DOMListPanelFactory;
import guicomponents.DOMTextFieldFactory;
import guicomponents.DOMTreeModel;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

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
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

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

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JTextField;
import javax.swing.border.BevelBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * The class which creates the UI to edit a configuration file.
 * 
 * @author Josh Lurz
 */
public class ConfigurationEditor extends JFrame implements DOMDocumentEditor {
    // TODO: Work out where invokeLater should be used.
    /**
     * Automatically generated unique class identifier.
     */
    private static final long serialVersionUID = -4743689341303656460L;

    /**
     * The current parsed XML document.
     */
    private Document mCurrentDocument = null;

    /**
     * The main window of the configuration editor.
     */
    private JPanel mMainWindow = null;

    /**
     * The container which holds the various tabs of the configuration editor.
     */
    private JTabbedPane mMainTabContainer = null;

    /**
     * The panel which contains the interface elements to modify the main
     * configuration options.
     */
    private JPanel mConfPanel = null;

    /**
     * The panel which contains the interface elements to modify the advanced
     * configuration settings.
     */
    private JPanel mAdvancedPanel = null;

    /**
     * The tree contained by the advanced panel which contains detailed
     * information about the configuration file.
     */
    private JTree mAdvancedTree = null;
    
    /**
     * Checkbox representing whether to perform calibration.
     */
    private JCheckBox mDoCalibrationCheckBox = null;

    /**
     * Checkbox representing whether to calculate total social cost.
     */
    private JCheckBox mRunCostCurvesCheckBox = null;

    /**
     * Checkbox representing whether to run the model in batch mode.
     */
    private JCheckBox mDoBatchModeCheckBox = null;

    /**
     * The toolbad which contains buttons to execute the available actions.
     */
    private JToolBar mMainToolBar = null;

    /**
     * The menu bar containing the File, Edit and Help menus.
     */
    private JMenuBar mMainMenuBar = null;

    /**
     * The File menu.
     */
    private JMenu mFileMenu = null;

    /**
     * The panel which contains user interface elements for modifying the add on
     * files.
     */
    private DOMListPanel mAddOnFilesPanel = null;

    /**
     * The button on the toolbar which triggers the save action.
     */
    private JButton mSaveButton = null;

    /**
     * The button on the toolbar which triggers the new action.
     */
    private JButton mNewButton = null;

    /**
     * The button on the toolbar which triggers the load action.
     */
    private JButton mLoadButton = null;

    /**
     * The button on the toolbar which triggers the run action.
     */
    private JButton mRunButton = null;

    /**
     * The Quit file menu item.
     */
    private JMenuItem mQuitMenuItem = null;

    /**
     * The New file menu item.
     */
    private JMenuItem mNewMenuItem = null;

    /**
     * The Load file menu item.
     */
    private JMenuItem mLoadMenuItem = null;

    /**
     * The Save file menu item.
     */
    private JMenuItem mSaveMenuItem = null;

    /**
     * The Save As file menu item.
     */
    private JMenuItem mSaveAsMenuItem = null;

    /**
     * The Run file menu item.
     */
    private JMenuItem mRunMenuItem = null;

    /**
     * The file menu item which triggers the merge action.
     */
    private JMenuItem mMergeMenuItem = null;

    /**
     * The top level edit menu.
     */
    private JMenu mEditMenu = null;

    /**
     * The edit menu item which displays a pane for editing the preferences for
     * the Configuration Editor.
     */
    private JMenuItem mPreferencesMenuItem = null;

    /**
     * The edit menu item which displays the pane for editing log settings.
     */
    private JMenuItem mEditLogsMenuItem = null;

    /**
     * Pane containing all batch configuration information.
     */
    private JPanel mBatchPane = null;

    /**
     * The menu containing help options.
     */
    private JMenu mHelpMenu = null;

    /**
     * The name of the root element which contains information in the DOM tree
     * about this object.
     */
    public static final String ROOT_ELEMENT_NAME = "Configuration"; //$NON-NLS-1$

    /**
     * The label for the batch file field. TODO: Make this a local variable.
     */
    private JLabel mBatchFileLabel = null;

    /**
     * The text field containing the location of the batch file.
     */
    private JTextField mBatchFileField = null;

    /**
     * The button which allows selection of the location of the batch file
     * field.
     */
    private JButton mBatchFileSelectButton = null;

    /**
     * The button which opens an editor to modify the batch file.
     */
    private JButton mBatchFileEditButton = null;

    /**
     * Button which prompts the user to create a new batch file.
     */
    private JButton mBatchFileNewButton = null;

    /**
     * A factory which creates and tracks text fields based on the current
     * document.
     */
    private DOMTextFieldFactory mTextFieldFactory = null;

    /**
     * A factory which creates list panels based on the current document.
     */
    private DOMListPanelFactory mFileListPanelFactory = null;

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
        mFileListPanelFactory = new DOMListPanelFactory();

        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                getTopLevelUI().setJMenuBar(getMainMenuBar());
                getTopLevelUI().setName(
                        Messages.getString("ConfigurationEditor.0")); //$NON-NLS-1$
                getTopLevelUI().setDefaultCloseOperation(
                        WindowConstants.DO_NOTHING_ON_CLOSE);
                getTopLevelUI().addWindowListener(
                        new QuitAction((ConfigurationEditor) getTopLevelUI()));
                getTopLevelUI().setContentPane(getMainWindow());
                getTopLevelUI().setTitle(
                        Messages.getString("ConfigurationEditor.1")); //$NON-NLS-1$
            }
        });
    }

    /**
     * Return the top level UI window.
     * 
     * @return The top level window. TODO: Try to remove this, check for a
     *         duplicate function.
     */
    private JFrame getTopLevelUI() {
        return this;
    }

    /**
     * Ask the user what action they would like to take to begin using the
     * editor. This is called from main.
     * 
     */
    public void askForInitialAction() {
        // First need to check if the preferences have been initializes.
        final File prefFile = new File(PropertiesInfo.PROPERTY_FILE);
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                if (!prefFile.canRead()) {
                    // Need to initialize the preferences window.
                    new ShowPreferencesAction(
                            (ConfigurationEditor) getTopLevelUI())
                            .actionPerformed(new ActionEvent(getTopLevelUI(),
                                    ActionEvent.ACTION_PERFORMED,
                                    "ShowPreferences")); //$NON-NLS-1$
                }
                Object[] options = {
                        Messages.getString("ConfigurationEditor.58"), //$NON-NLS-1$
                        Messages.getString("ConfigurationEditor.59"), Messages.getString("ConfigurationEditor.60") }; //$NON-NLS-1$ //$NON-NLS-2$
                String message = Messages.getString("ConfigurationEditor.61"); //$NON-NLS-1$

                int rv = JOptionPane
                        .showOptionDialog(getTopLevelUI(), message, Messages
                                .getString("ConfigurationEditor.62"), //$NON-NLS-1$
                                JOptionPane.DEFAULT_OPTION,
                                JOptionPane.QUESTION_MESSAGE, null, options,
                                options[0]);
                if (rv == 0) {
                    new NewAction(((ConfigurationEditor) getTopLevelUI()))
                            .actionPerformed(new ActionEvent(getTopLevelUI(),
                                    ActionEvent.ACTION_PERFORMED, "New")); //$NON-NLS-1$
                } else if (rv == 1) {
                    new LoadAction(((ConfigurationEditor) getTopLevelUI()))
                            .actionPerformed(new ActionEvent(getTopLevelUI(),
                                    ActionEvent.ACTION_PERFORMED, "Load")); //$NON-NLS-1$
                }

                // Check if for any reason the document is still not set and
                // warn the user that action is prevented until they load a
                // document or create a new one.
                if (mCurrentDocument == null) {
                    String warnMessage = Messages
                            .getString("ConfigurationEditor.64"); //$NON-NLS-1$
                    JOptionPane.showMessageDialog(getTopLevelUI(), warnMessage,
                            Messages.getString("ConfigurationEditor.65"), //$NON-NLS-1$
                            JOptionPane.WARNING_MESSAGE);
                }
            }
        });
    }

    /**
     * Set the current document.
     * 
     * @param aNewDocument
     *            The document to set as the current.
     */
    public void setDocument(Document aNewDocument) {
        // Check for a blank document.
        if (aNewDocument == null) {
            Logger.global.log(Level.WARNING, "Tried to set a blank document.");
            return;
        }
        mCurrentDocument = aNewDocument;
        // Add an event handler which will listen for the document being
        // changed and set that the document needs to be saved.
        EventTarget target = (EventTarget) mCurrentDocument
                .getDocumentElement();
        target.addEventListener("DOMSubtreeModified",
                new ConfigurationDocumentMutationListener(), true);



        // Update the user interface. Check if this is necessary.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                // Set the document into the factories. This will
                // cause them to update the objects they created.
                mTextFieldFactory.setDocument(mCurrentDocument);
                mFileListPanelFactory.setDocument(mCurrentDocument);
                
                // Set the document into the advanced panel tree.
                ((DOMTreeModel)mAdvancedTree.getModel()).setDocument(mCurrentDocument);
                
                enableBatchInputFields(mDoBatchModeCheckBox.isSelected());
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
     * Returns that the user needs to be asked before
     * the file is saved.
     * @return True, meaning that the user does need to be 
     * asked before saving the file.
     */
    public boolean askBeforeSaving() {
        return true;
    }
    
    /**
     * This method initializes the main window.
     * 
     * @return The main window.
     */
    private JPanel getMainWindow() {
        if (mMainWindow == null) {
            mMainWindow = new JPanel();
            mMainWindow.setToolTipText(Messages
                    .getString("ConfigurationEditor.3")); //$NON-NLS-1$
            mMainWindow.setLayout(new BorderLayout());
            mMainWindow.add(getMainToolBar(), BorderLayout.NORTH);
            mMainWindow.add(getMainTabContainer(), BorderLayout.SOUTH);

        }
        return mMainWindow;
    }

    /**
     * This method initializes the main tab container.
     * 
     * @return The tabbed container.
     */
    private JTabbedPane getMainTabContainer() {
        if (mMainTabContainer == null) {
            mMainTabContainer = new JTabbedPane();
            mMainTabContainer.setToolTipText(Messages
                    .getString("ConfigurationEditor.4")); //$NON-NLS-1$
            mMainTabContainer
                    .addTab(
                            Messages.getString("ConfigurationEditor.5"), null, getConfPanel(), Messages.getString("ConfigurationEditor.6")); //$NON-NLS-1$ //$NON-NLS-2$
            mMainTabContainer
                    .addTab(
                            Messages.getString("ConfigurationEditor.110"), null, getBatchPane(), Messages.getString("ConfigurationEditor.111")); //$NON-NLS-1$ //$NON-NLS-2$
            // Whats the difference between the two strings?
            mMainTabContainer
                    .addTab(
                            Messages.getString("ConfigurationEditor.7"), null, getAdvancedPanel(), Messages.getString("ConfigurationEditor.8")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return mMainTabContainer;
    }

    /**
     * This method initializes the configuration panel.
     * 
     * @return The configuration panel.
     */
    private JPanel getConfPanel() {
        if (mConfPanel == null) {
            mConfPanel = new JPanel();
            mConfPanel.setLayout(new GridBagLayout());
            mConfPanel.setBorder(BorderFactory
                    .createBevelBorder(BevelBorder.RAISED));

            // Now setup the layout constraints.
            GridBagConstraints cons = new GridBagConstraints();

            // Layout the items top to bottom, left to right.
            // Position the checkboxes in the first column.
            cons.gridx = 0;

            // Position components on the left side of their cells.
            cons.anchor = GridBagConstraints.WEST;

            // Add panels in increasing rows.
            cons.gridy = 0;

            // Put a border of 5 around the entire panel.
            cons.insets = new Insets(5, 5, 5, 5);

            mConfPanel.add(getDoCalibrationCheckBox(), cons);

            cons.gridy = GridBagConstraints.RELATIVE;
            mConfPanel.add(getRunCostCurvesCheckBox(), cons);

            // Now add the labels in column 1.
            cons.gridx = 1;
            cons.gridy = 0;

            // Create a label for the scenario name field.
            JLabel scenNameLabel = new JLabel(Messages
                    .getString("ConfigurationEditor.112")); //$NON-NLS-1$
            mConfPanel.add(scenNameLabel, cons);

            // Put the labels in increasing rows.
            cons.gridy = GridBagConstraints.RELATIVE;

            // Create a label for the input file field.
            JLabel inputFileLabel = new JLabel(Messages
                    .getString("ConfigurationEditor.115")); //$NON-NLS-1$
            mConfPanel.add(inputFileLabel, cons);

            // Create a label for the output file field.
            JLabel outputFileLabel = new JLabel(Messages
                    .getString("ConfigurationEditor.118")); //$NON-NLS-1$
            mConfPanel.add(outputFileLabel, cons);

            // Add the text fields in column 2.
            cons.gridx = 2;
            cons.gridy = 0;

            // Allow the text field to resize with the items.
            cons.weightx = 1;
            cons.fill = GridBagConstraints.HORIZONTAL;

            // Add the scenario name field.
            JTextField scenNameField = mTextFieldFactory.createTextField(
                    "Strings", "scenarioName"); //$NON-NLS-1$ //$NON-NLS-2$
            scenNameField.setPreferredSize(new Dimension(200, 20));
            mConfPanel.add(scenNameField, cons);

            // Put the fields in increasing rows.
            cons.gridy = GridBagConstraints.RELATIVE;

            // Add the input field.
            JTextField inputFileField = mTextFieldFactory.createTextField(
                    "Files", "xmlInputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
            inputFileField.setPreferredSize(new Dimension(200, 20));
            mConfPanel.add(inputFileField, cons);

            // Add the output file field.
            JTextField outputFileField = mTextFieldFactory.createTextField(
                    "Files", "xmlOutputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
            outputFileField.setPreferredSize(new Dimension(200, 20));
            mConfPanel.add(outputFileField, cons);

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
            mAddOnFilesPanel = mFileListPanelFactory
                    .createDOMFileListPanel(
                            "/Configuration/ScenarioComponents", "ScenarioComponent", Messages.getString("ConfigurationEditor.151"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            mConfPanel.add(mAddOnFilesPanel, cons);
        }
        return mConfPanel;
    }

    /**
     * This method initializes the batch file label.
     * 
     * @return The batch file label. TODO: Move this method to the right place.
     */
    private JLabel getBatchFileLabel() {
        if (mBatchFileLabel == null) {
            mBatchFileLabel = new JLabel();
            mBatchFileLabel.setText(Messages
                    .getString("ConfigurationEditor.124")); //$NON-NLS-1$
        }
        return mBatchFileLabel;
    }

    /**
     * This method initializes the advanced preferences panel.
     * 
     * @return The advanced panel.
     */
    private JPanel getAdvancedPanel() {
        if (mAdvancedPanel == null) {
            mAdvancedPanel = new JPanel();
            mAdvancedPanel.setBorder(javax.swing.BorderFactory
                    .createBevelBorder(javax.swing.border.BevelBorder.RAISED));
            // Create a new tree using a DOM based model. The model is initialized
            // with the root node of the document and name of leaf items.
            mAdvancedTree = new JTree(new DOMTreeModel(mCurrentDocument, "Value"));
            mAdvancedTree.setVisible(true);
            mAdvancedTree.setShowsRootHandles(true);
            mAdvancedTree.setVisibleRowCount(10);
            mAdvancedTree.setPreferredSize(new Dimension(400,400));
            mAdvancedTree.setRootVisible(true);
            mAdvancedPanel.add(mAdvancedTree);
        }
        return mAdvancedPanel;
    }

    /**
     * This method initializes the do calibration checkbox.
     * 
     * @return The do calibration checkbox.
     */
    private JCheckBox getDoCalibrationCheckBox() {
        if (mDoCalibrationCheckBox == null) {
            mDoCalibrationCheckBox = new JCheckBox();
            final String parentXPath = "/"
                    + ConfigurationEditor.ROOT_ELEMENT_NAME + "/Bools";
            mDoCalibrationCheckBox.setModel(new DOMButtonModel(this,
                    parentXPath, "Value", "CalibrationActive", false)); //$NON-NLS-1$
            mDoCalibrationCheckBox.setMnemonic(KeyEvent.VK_C);
            mDoCalibrationCheckBox.setText(Messages
                    .getString("ConfigurationEditor.13")); //$NON-NLS-1$
            mDoCalibrationCheckBox.setToolTipText(Messages
                    .getString("ConfigurationEditor.14")); //$NON-NLS-1$
        }
        return mDoCalibrationCheckBox;
    }

    /**
     * This method initializes the run consts curves check box.
     * 
     * @return The run cost curves check box.
     */
    private JCheckBox getRunCostCurvesCheckBox() {
        if (mRunCostCurvesCheckBox == null) {
            mRunCostCurvesCheckBox = new JCheckBox();
            final String parentXPath = "/"
                    + ConfigurationEditor.ROOT_ELEMENT_NAME + "/Bools";
            mRunCostCurvesCheckBox.setModel(new DOMButtonModel(this,
                    parentXPath, "Value", "createCostCurve", false)); //$NON-NLS-1$
            mRunCostCurvesCheckBox.setMnemonic(KeyEvent.VK_O);
            mRunCostCurvesCheckBox.setText(Messages
                    .getString("ConfigurationEditor.128")); //$NON-NLS-1$
            mRunCostCurvesCheckBox.setToolTipText(Messages
                    .getString("ConfigurationEditor.129")); //$NON-NLS-1$
        }
        return mRunCostCurvesCheckBox;
    }

    /**
     * This method initializes the do batch mode check box.
     * 
     * @return The batch mode checkbox.
     */
    private JCheckBox getDoBatchModeCheckBox() {
        if (mDoBatchModeCheckBox == null) {
            mDoBatchModeCheckBox = new JCheckBox();
            final String parentXPath = "/"
                    + ConfigurationEditor.ROOT_ELEMENT_NAME + "/Bools";
            mDoBatchModeCheckBox.setModel(new DOMButtonModel(this, parentXPath,
                    "Value", "BatchMode", false)); //$NON-NLS-1$
            mDoBatchModeCheckBox.setMnemonic(KeyEvent.VK_B);
            mDoBatchModeCheckBox.setText(Messages
                    .getString("ConfigurationEditor.16")); //$NON-NLS-1$
            mDoBatchModeCheckBox.setToolTipText(Messages
                    .getString("ConfigurationEditor.17")); //$NON-NLS-1$

            // Add a listener to enable and disable the related options.
            mDoBatchModeCheckBox.addChangeListener(new ChangeListener() {
                public void stateChanged(ChangeEvent aEvent) {
                    // How the state changed is unknown. Check whether
                    // the checkbox is currently set and enable or disable
                    // the related fields.
                    enableBatchInputFields(mDoBatchModeCheckBox.isSelected());
                }
            });
        }
        return mDoBatchModeCheckBox;
    }

    /**
     * This method initializes the main toolbar.
     * 
     * @return The main toolbar.
     */
    private JToolBar getMainToolBar() {
        if (mMainToolBar == null) {
            mMainToolBar = new JToolBar();
            mMainToolBar.setToolTipText(Messages
                    .getString("ConfigurationEditor.19")); //$NON-NLS-1$
            mMainToolBar.setName(Messages.getString("ConfigurationEditor.131")); //$NON-NLS-1$
            mMainToolBar.add(getNewButton());
            mMainToolBar.add(getLoadButton());
            mMainToolBar.add(getSaveButton());
            mMainToolBar.add(getRunButton());
        }
        return mMainToolBar;
    }

    /**
     * This method initializes the main menubar.
     * 
     * @return The main menu bar.
     */
    private JMenuBar getMainMenuBar() {
        if (mMainMenuBar == null) {
            mMainMenuBar = new JMenuBar();
            mMainMenuBar.setToolTipText(Messages
                    .getString("ConfigurationEditor.21")); //$NON-NLS-1$
            mMainMenuBar.add(getFileMenu());
            mMainMenuBar.add(getEditMenu());
            mMainMenuBar.add(getHelpMenu());
        }
        return mMainMenuBar;
    }

    /**
     * This method initializes the file menu.
     * 
     * @return The file menu.
     */
    private JMenu getFileMenu() {
        if (mFileMenu == null) {
            mFileMenu = new JMenu();
            mFileMenu.setText(Messages.getString("ConfigurationEditor.24")); //$NON-NLS-1$
            mFileMenu.setToolTipText(Messages
                    .getString("ConfigurationEditor.25")); //$NON-NLS-1$
            mFileMenu.setMnemonic(KeyEvent.VK_F);

            // Add the menu items.
            mFileMenu.add(getNewMenuItem());
            mFileMenu.add(getLoadMenuItem());
            mFileMenu.add(getSaveMenuItem());
            mFileMenu.add(getSaveAsMenuItem());
            mFileMenu.add(getRunMenuItem());
            mFileMenu.add(getMergeMenuItem());
            mFileMenu.add(getQuitMenuItem());
        }
        return mFileMenu;
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
    private JButton getNewButton() {
        if (mNewButton == null) {
            mNewButton = new JButton();
            mNewButton.setAction(new NewAction(this));
            mNewButton.setText(Messages.getString("ConfigurationEditor.32")); //$NON-NLS-1$
            mNewButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.33")); //$NON-NLS-1$
            mNewButton.setActionCommand("New"); //$NON-NLS-1$
            mNewButton.setMnemonic(KeyEvent.VK_N);
        }
        return mNewButton;
    }

    /**
     * This method initializes the load button.
     * 
     * @return The load button.
     */
    private JButton getLoadButton() {
        if (mLoadButton == null) {
            mLoadButton = new JButton();
            mLoadButton.setAction(new LoadAction(this));
            mLoadButton.setText(Messages.getString("ConfigurationEditor.36")); //$NON-NLS-1$
            mLoadButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.37")); //$NON-NLS-1$
            mLoadButton.setMnemonic(KeyEvent.VK_O);
        }
        return mLoadButton;
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
    private JMenuItem getQuitMenuItem() {
        if (mQuitMenuItem == null) {
            mQuitMenuItem = new JMenuItem();
            mQuitMenuItem.setAction(new QuitAction(this));
            mQuitMenuItem.setMnemonic(KeyEvent.VK_Q);
            mQuitMenuItem.setText(Messages.getString("ConfigurationEditor.41")); //$NON-NLS-1$
            mQuitMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.42")); //$NON-NLS-1$
            mQuitMenuItem.setActionCommand("Quit"); //$NON-NLS-1$
        }
        return mQuitMenuItem;
    }

    /**
     * This method initializes the new menu item.
     * 
     * @return The new menu item.
     */
    private JMenuItem getNewMenuItem() {
        if (mNewMenuItem == null) {
            mNewMenuItem = new JMenuItem();
            mNewMenuItem.setActionCommand("New"); //$NON-NLS-1$
            mNewMenuItem.setAction(new NewAction(this));
            mNewMenuItem.setMnemonic(KeyEvent.VK_N);
            mNewMenuItem.setText(Messages.getString("ConfigurationEditor.44")); //$NON-NLS-1$
            mNewMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.45")); //$NON-NLS-1$
        }
        return mNewMenuItem;
    }

    /**
     * This method initializes the load menu item.
     * 
     * @return The load menu item.
     */
    private JMenuItem getLoadMenuItem() {
        if (mLoadMenuItem == null) {
            mLoadMenuItem = new JMenuItem();
            mLoadMenuItem.setActionCommand("Load"); //$NON-NLS-1$
            mLoadMenuItem.setAction(new LoadAction(this));
            mLoadMenuItem.setText(Messages.getString("ConfigurationEditor.48")); //$NON-NLS-1$
            mLoadMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.49")); //$NON-NLS-1$
            mLoadMenuItem.setMnemonic(KeyEvent.VK_O);
        }
        return mLoadMenuItem;
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
     * This method initializes the edit menu.
     * 
     * @return The edit menu.
     */
    private JMenu getEditMenu() {
        if (mEditMenu == null) {
            mEditMenu = new JMenu();
            mEditMenu.setMnemonic(KeyEvent.VK_E);
            mEditMenu.setText(Messages.getString("ConfigurationEditor.73")); //$NON-NLS-1$
            mEditMenu.setToolTipText(Messages
                    .getString("ConfigurationEditor.74")); //$NON-NLS-1$
            mEditMenu.add(getPreferencesMenuItem());
            mEditMenu.add(getEditLogsMenuItem());
        }
        return mEditMenu;
    }

    /**
     * This method initializes the preferences menu item.
     * 
     * @return The preferences menu item.
     */
    private JMenuItem getPreferencesMenuItem() {
        if (mPreferencesMenuItem == null) {
            mPreferencesMenuItem = new JMenuItem();
            mPreferencesMenuItem.setActionCommand("ShowPreferences"); //$NON-NLS-1$
            mPreferencesMenuItem.setAction(new ShowPreferencesAction(this));
            mPreferencesMenuItem.setText(Messages
                    .getString("ConfigurationEditor.77")); //$NON-NLS-1$
            mPreferencesMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.78")); //$NON-NLS-1$
            mPreferencesMenuItem.setMnemonic(KeyEvent.VK_P);
        }
        return mPreferencesMenuItem;
    }

    /**
     * This method initializes the edit logs menu item.
     * 
     * @return The edit logs menu item.
     */
    private JMenuItem getEditLogsMenuItem() {
        if (mEditLogsMenuItem == null) {
            mEditLogsMenuItem = new JMenuItem();
            mEditLogsMenuItem.setActionCommand("EditLogs"); //$NON-NLS-1$
            mEditLogsMenuItem.setAction(new EditLogSettingsAction());
            mEditLogsMenuItem.setText(Messages
                    .getString("ConfigurationEditor.136")); //$NON-NLS-1$
            mEditLogsMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.137")); //$NON-NLS-1$
            mEditLogsMenuItem.setMnemonic(KeyEvent.VK_L);
        }
        return mEditLogsMenuItem;
    }

    /**
     * This method initializes the batch editor pane.
     * 
     * @return The pane containing all batch configuration information.
     */
    private JPanel getBatchPane() {
        if (mBatchPane == null) {
            mBatchPane = new JPanel();
            mBatchPane.setBorder(BorderFactory
                    .createBevelBorder(BevelBorder.RAISED));
            mBatchPane.setToolTipText(Messages
                    .getString("ConfigurationEditor.138")); //$NON-NLS-1$
            mBatchPane.add(getDoBatchModeCheckBox(), null);
            mBatchPane.add(getBatchFileLabel(), null);
            mBatchPane.add(getBatchFileField(), null);
            mBatchPane.add(getBatchFileSelectButton(), null);
            mBatchPane.add(getBatchFileEditButton(), null);
            mBatchPane.add(getBatchFileNewButton(), null);
            // Enable the fields according to the value of the checkbox.
            enableBatchInputFields(getDoBatchModeCheckBox().isSelected());
        }
        return mBatchPane;
    }

    /**
     * This method initializes the merge menu item.
     * 
     * @return The merge menu item.
     */
    private JMenuItem getMergeMenuItem() {
        if (mMergeMenuItem == null) {
            mMergeMenuItem = new JMenuItem();
            mMergeMenuItem
                    .setText(Messages.getString("ConfigurationEditor.98")); //$NON-NLS-1$
            mMergeMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.99")); //$NON-NLS-1$
            mMergeMenuItem.setEnabled(false);
            mMergeMenuItem.setMnemonic(java.awt.event.KeyEvent.VK_M);
        }
        return mMergeMenuItem;
    }

    /**
     * This method initializes the help menu.
     * 
     * @return The help menu.
     */
    private JMenu getHelpMenu() {
        if (mHelpMenu == null) {
            mHelpMenu = new JMenu();
            mHelpMenu.setMnemonic(java.awt.event.KeyEvent.VK_H);
            mHelpMenu.setText(Messages.getString("ConfigurationEditor.100")); //$NON-NLS-1$
            mHelpMenu.setToolTipText(Messages
                    .getString("ConfigurationEditor.101")); //$NON-NLS-1$
        }
        return mHelpMenu;
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
     * Enable or disable the group of batch file input objects.
     * 
     * @param aEnable
     *            Whether to enable or disable.
     */
    private void enableBatchInputFields(boolean aEnable) {
        getBatchFileLabel().setEnabled(aEnable);
        getBatchFileField().setEnabled(aEnable);
        getBatchFileSelectButton().setEnabled(aEnable);
        getBatchFileNewButton().setEnabled(aEnable);
        // Only enable the batch file edit button if a file name has been
        // set.
        getBatchFileEditButton().setEnabled(
                getBatchFileField().getText().length() > 0);
    }

    /**
     * This method initializes the batch file text field.
     * 
     * @return The batch file text field.
     */
    private JTextField getBatchFileField() {
        if (mBatchFileField == null) {
            mBatchFileField = mTextFieldFactory.createTextField(
                    "Files", "BatchFileName"); //$NON-NLS-1$ //$NON-NLS-2$
            mBatchFileField.setPreferredSize(new Dimension(200, 20));
            mBatchFileField.setToolTipText(Messages
                    .getString("ConfigurationEditor.141")); //$NON-NLS-1$
            // Add an action listener to enable the edit button.
            mBatchFileField.addKeyListener(new KeyListener() {
                public void keyTyped(KeyEvent aEvent) {
                    // Activate the edit button if there is text in the field.
                    getBatchFileEditButton().setEnabled(
                            mBatchFileField.getText().length() > 0);
                }

                public void keyPressed(KeyEvent aEvent) {
                    // Ignore this event.
                }

                public void keyReleased(KeyEvent aEvent) {
                    // Ignore this event.
                }

            });
        }
        return mBatchFileField;
    }

    /**
     * This method initializes the batch file select button.
     * 
     * @return The batch file select button.
     */
    private JButton getBatchFileSelectButton() {
        if (mBatchFileSelectButton == null) {
            mBatchFileSelectButton = new JButton();
            mBatchFileSelectButton.setText(Messages
                    .getString("ConfigurationEditor.142")); //$NON-NLS-1$
            mBatchFileSelectButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.143")); //$NON-NLS-1$
            mBatchFileSelectButton.setMnemonic(KeyEvent.VK_S);
            mBatchFileSelectButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    JFileChooser batchFileChooser = new JFileChooser();
                    batchFileChooser.setFileFilter(new XMLFileFilter());
                    int rv = batchFileChooser.showDialog(getMainWindow(),
                            Messages.getString("ConfigurationEditor.144")); //$NON-NLS-1$
                    if (rv == JFileChooser.APPROVE_OPTION) {
                        File selectedFile = batchFileChooser.getSelectedFile();
                        if (selectedFile != null) {
                            // Set the text field.
                            getBatchFileField().setText(
                                    selectedFile.getAbsolutePath());
                            // Enable the edit button.
                            getBatchFileEditButton().setEnabled(true);
                        }
                    }
                }
            });
        }
        return mBatchFileSelectButton;
    }

    /**
     * This method initializes the new batch file button.
     * 
     * @return The new batch file button.
     */
    private JButton getBatchFileNewButton() {
        if (mBatchFileNewButton == null) {
            mBatchFileNewButton = new JButton();
            mBatchFileNewButton.setText(Messages
                    .getString("ConfigurationEditor.145")); //$NON-NLS-1$
            mBatchFileNewButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.146")); //$NON-NLS-1$
            mBatchFileNewButton.setMnemonic(KeyEvent.VK_N);
            // Create an input field, dispatch the editor.
            mBatchFileNewButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    // Create a file chooser and add an XML filter.
                    JFileChooser chooser = new JFileChooser();
                    chooser.setFileFilter(new XMLFileFilter());

                    // Show the file chooser.
                    int rv = chooser.showSaveDialog(getMainWindow());
                    if (rv == JFileChooser.APPROVE_OPTION) {
                        File newFile = chooser.getSelectedFile();
                        // TODO: Check if the file already exists.
                        getBatchFileField().setText(newFile.getAbsolutePath());
                        // Check if the new file already exists.
                        if (newFile.exists()) {
                            String errorMessage = Messages
                                    .getString("ConfigurationEditor.147"); //$NON-NLS-1$
                            String errorTitle = Messages
                                    .getString("ConfigurationEditor.148"); //$NON-NLS-1$
                            JOptionPane.showMessageDialog(getMainWindow(),
                                    errorMessage, errorTitle,
                                    JOptionPane.ERROR_MESSAGE);
                            return;
                        }
                        BatchFileEditor batchEditor = new BatchFileEditor(
                                newFile.getAbsolutePath(), true);
                        batchEditor.pack();
                        batchEditor.setVisible(true);
                    }
                }
            });
        }
        return mBatchFileNewButton;
    }

    /**
     * This method initializes the batch file edit button.
     * 
     * @return The batch file edit button.
     */
    private JButton getBatchFileEditButton() {
        if (mBatchFileEditButton == null) {
            mBatchFileEditButton = new JButton();
            mBatchFileEditButton.setText(Messages
                    .getString("ConfigurationEditor.149")); //$NON-NLS-1$
            mBatchFileEditButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.150")); //$NON-NLS-1$
            mBatchFileEditButton.setMnemonic(KeyEvent.VK_E);
            mBatchFileEditButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    // Create a new batch file editor.
                    String batchFile = getBatchFileField().getText();
                    BatchFileEditor batchEditor = new BatchFileEditor(
                            batchFile, false);
                    batchEditor.pack();
                    batchEditor.setVisible(true);
                }
            });
        }
        return mBatchFileEditButton;
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
         * Method called when mutation events from the configuration document
         * are received.
         * 
         * @param aEvent
         *            The mutation event received.
         */
        public void handleEvent(Event aEvent) {
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

