/**
 * 
 */
package configurationeditor;

import interfaceutils.DOMButtonModel;
import interfaceutils.DOMListPanel;
import interfaceutils.DOMListPanelFactory;
import interfaceutils.DOMTextFieldFactory;
import interfaceutils.XMLFileFilter;

import java.awt.BorderLayout;
import java.awt.Dimension;
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
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.w3c.dom.Document;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;

import com.zookitec.layout.ContainerEF;
import com.zookitec.layout.ExplicitConstraints;
import com.zookitec.layout.ExplicitLayout;

import batchcreator.BatchFileEditor;

import actions.EditLogSettingsAction;
import actions.LoadAction;
import actions.NewAction;
import actions.QuitAction;
import actions.RunAction;
import actions.SaveAction;
import actions.ShowPreferencesAction;

import java.io.File;

import javax.swing.JTextField;
import javax.swing.border.BevelBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


/**
 * The class which creates the UI to edit a configuration file. 
 * @author Josh Lurz
 */
public class ConfigurationEditor extends JFrame {
    /**
	 * Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = -4743689341303656460L;
    
    /**
     * The current parsed XML document.
     */
    private Document mCurrentDocument = null;
    
    private JPanel mMainWindow = null;

    private JTabbedPane mMainTabContainer = null;

    private JPanel mConfPanel = null;

    private JPanel mAdvancedPanel = null;

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
    
    private JToolBar mMainToolBar = null;

    private JMenuBar mMainMenuBar = null;

    private JMenu mFileMenu = null;
    
    private DOMListPanel mListPanel = null;

    private JButton mSaveButton = null;

    private JButton mNewButton = null;

    private JButton mOpenButton = null;

    private JButton mRunButton = null;

    private JMenuItem mQuitMenuItem = null;

    private JMenuItem mNewMenuItem = null;

    private JMenuItem mOpenMenuItem = null;

    private JMenuItem mSaveMenuItem = null;

    private JMenuItem mSaveAsMenuItem = null;

    private JMenuItem mRunMenuItem = null;

    /**
     * The top level edit menu.
     */
    private JMenu mEditMenu = null;
    
    /**
     * The edit menu item which displays a pane for editing the preferences
     * for the Configuration Editor.
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

    private JMenuItem mMergeMenuItem = null;

    private JMenu mHelpMenu = null;
    
    /**
     * The name of the root element which contains information
     * in the DOM tree about this object.
     */
    public static final String ROOT_ELEMENT_NAME = "Configuration"; //$NON-NLS-1$
    
    private JLabel mBatchFileLabel = null;

    private JTextField mBatchFileField = null;

    private JButton mBatchFileSelectButton = null;

    private JButton mBatchFileEditButton = null;
    
    /**
     * Button which prompts the user to create a new batch file.
     */
    private JButton mBatchFileNewButton = null;
    
    private DOMTextFieldFactory mTextFieldFactory = null;
    
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
     * This method initializes this
     * 
     */
    private void initialize() {
    	mTextFieldFactory = new DOMTextFieldFactory();
    	mFileListPanelFactory = new DOMListPanelFactory();
        this.setJMenuBar(getMainMenuBar());
        this.setName(Messages.getString("ConfigurationEditor.0")); //$NON-NLS-1$
        this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new QuitAction(this));
        this.setContentPane(getMainWindow());
        this.setTitle(Messages.getString("ConfigurationEditor.1")); //$NON-NLS-1$
    }

    /**
     * Ask the user what action they would like to take to begin using the
     * editor.
     * 
     */
    public void askForInitialAction() {
        // First need to check if the preferences have been initializes.
        File prefFile = new File(PropertiesInfo.PROPERTY_FILE);
        if(!prefFile.canRead()) {
            // Need to initialize the preferences window.
            new ShowPreferencesAction(this).actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "ShowPreferences")); //$NON-NLS-1$
        }
        Object[] options = { Messages.getString("ConfigurationEditor.58"), //$NON-NLS-1$
                Messages.getString("ConfigurationEditor.59"), Messages.getString("ConfigurationEditor.60") }; //$NON-NLS-1$ //$NON-NLS-2$
        String message = Messages.getString("ConfigurationEditor.61"); //$NON-NLS-1$
        int rv = JOptionPane.showOptionDialog(this, message, Messages.getString("ConfigurationEditor.62"), //$NON-NLS-1$
                JOptionPane.DEFAULT_OPTION, JOptionPane.QUESTION_MESSAGE, null,
                options, options[0]);
        if (rv == 0) {
            new NewAction(this).actionPerformed(
                    new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "New")); //$NON-NLS-1$
        } else if (rv == 1) {
            new LoadAction(this).actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, "Load")); //$NON-NLS-1$
        } 
        
        // Check if for any reason the document is still not set and 
        // warn the user that action is prevented until they load a 
        // document or create a new one.
        if( mCurrentDocument == null ) {
            String warnMessage = Messages.getString("ConfigurationEditor.64"); //$NON-NLS-1$
            JOptionPane.showMessageDialog(this, warnMessage, Messages.getString("ConfigurationEditor.65"), //$NON-NLS-1$
                    JOptionPane.WARNING_MESSAGE);
        }
    }
    
    /**
     * Set the current document.
     * 
     * @param aNewDocument
     *            The document to set as the current.
     */
    public void setDocument(Document aNewDocument) {
        mCurrentDocument = aNewDocument;
        // Add an event handler which will listen for the document being
        // changed and set that the document needs to be saved.
        EventTarget target = (EventTarget)mCurrentDocument.getDocumentElement();
        target.addEventListener("DOMSubtreeModified", new EventListener() { //$NON-NLS-1$
            public void handleEvent(Event aEvent) {
                // This doesn't recursively send another event,
                // not sure why but it works.
                mCurrentDocument.getDocumentElement().setAttribute("needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
                // Enable the save menu item.
                getSaveMenuItem().setEnabled(true);
            }
        }, true );
        mTextFieldFactory.setDocument(aNewDocument);
        mFileListPanelFactory.setDocument(aNewDocument);
        // Update the user interface.
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                enableBatchInputFields(mDoBatchModeCheckBox.isSelected());
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
     * This method initializes mMainWindow
     * 
     * @return The main window.
     */
    private JPanel getMainWindow() {
        if (mMainWindow == null) {
            mMainWindow = new JPanel();
            mMainWindow.setToolTipText(Messages.getString("ConfigurationEditor.3")); //$NON-NLS-1$
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
            .addTab( Messages.getString("ConfigurationEditor.110"), null, getBatchPane(), Messages.getString("ConfigurationEditor.111")); //$NON-NLS-1$ //$NON-NLS-2$
            // Whats the difference between the two strings? 
            mMainTabContainer
                    .addTab(
                            Messages.getString("ConfigurationEditor.7"), null, getAdvancedPanel(), Messages.getString("ConfigurationEditor.8")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return mMainTabContainer;
    }

    /**
     * This method initializes mConfPanel
     * 
     * @return The configuration panel.
     */
    private JPanel getConfPanel() {
        if (mConfPanel == null) {
            mConfPanel = new JPanel();
            mConfPanel.setPreferredSize(new Dimension(915, 325));
            mConfPanel.setLayout(new ExplicitLayout());
            mConfPanel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
            
            mConfPanel.add(getDoCalibrationCheckBox(),
                    new ExplicitConstraints(getDoCalibrationCheckBox(), 
                    ContainerEF.left(this).add(ContainerEF.left(this)).add(10), 
                    ContainerEF.top(this).add(10)));
            
            mConfPanel.add(getRunCostCurvesCheckBox(),
                           new ExplicitConstraints(getRunCostCurvesCheckBox(), 
                           ContainerEF.left(this).add(10), 
                           ContainerEF.top(this).add(40)));
            
            // Create a label for the scenario name field.
            JLabel scenNameLabel = new JLabel(Messages.getString("ConfigurationEditor.112")); //$NON-NLS-1$
            mConfPanel.add(scenNameLabel, new ExplicitConstraints(
                    scenNameLabel, ContainerEF.left(this).add(250), ContainerEF.top(this).add(10)));
            
            // Add the scenario name field.
            JTextField scenNameField = mTextFieldFactory.createTextField(
                    "Strings", "scenarioName"); //$NON-NLS-1$ //$NON-NLS-2$
            scenNameField.setPreferredSize(new Dimension(200,20));
            mConfPanel.add(scenNameField, new ExplicitConstraints(
                    scenNameField, ContainerEF.left(this).add(350), ContainerEF.top(this).add(10)));
            
            // Create a label for the input file field.
            JLabel inputFileLabel = new JLabel(Messages.getString("ConfigurationEditor.115")); //$NON-NLS-1$
            mConfPanel.add(inputFileLabel, new ExplicitConstraints(
                    inputFileLabel, ContainerEF.left(this).add(250), ContainerEF.top(
                                    this).add(40)));
            
            // Add the input field.
            JTextField inputFileField = mTextFieldFactory.createTextField(
                    "Files", "xmlInputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
            inputFileField.setPreferredSize(new Dimension(200,20));
            mConfPanel.add(inputFileField, new ExplicitConstraints(
                    inputFileField, ContainerEF.left(this).add(350), ContainerEF.top(this).add(40)));
            
            // Create a label for the output file field.
            JLabel outputFileLabel = new JLabel(Messages.getString("ConfigurationEditor.118")); //$NON-NLS-1$
            mConfPanel.add(outputFileLabel, new ExplicitConstraints(
                    outputFileLabel, ContainerEF.left(this).add(250), ContainerEF.top(this).add(70)));
            
            // Add the output file field.
            JTextField outputFileField = mTextFieldFactory.createTextField("Files", "xmlOutputFileName"); //$NON-NLS-1$ //$NON-NLS-2$
            outputFileField.setPreferredSize(new Dimension(200,20));
            mConfPanel.add(outputFileField, new ExplicitConstraints(
                    outputFileField, ContainerEF.left(this).add(350), ContainerEF.top(this).add(70)));
            
            // Add the list panel.
            mListPanel = mFileListPanelFactory.createDOMFileListPanel("/Configuration/ScenarioComponents", "ScenarioComponent", Messages.getString("ConfigurationEditor.151"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            mConfPanel.add(mListPanel, new ExplicitConstraints(
                            mListPanel, ContainerEF.left(this).add(600), ContainerEF.top(this).add(10)));
        }
        return mConfPanel;
    }
    
    /**
     * This method initializes the batch file label.
     * @return The batch file label.
     * TODO: Move this method to the right place.
     */
    private JLabel getBatchFileLabel() {
        if(mBatchFileLabel == null) {
            mBatchFileLabel = new JLabel();
            mBatchFileLabel.setText(Messages.getString("ConfigurationEditor.124")); //$NON-NLS-1$
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
            mAdvancedPanel.add(new JLabel(Messages.getString("ConfigurationEditor.125"))); //$NON-NLS-1$
        }
        return mAdvancedPanel;
    }

    /**
     * This method initializes doCalibration
     * 
     * @return javax.swing.JCheckBox
     */
    private JCheckBox getDoCalibrationCheckBox() {
        if (mDoCalibrationCheckBox == null) {
            mDoCalibrationCheckBox = new JCheckBox();
            mDoCalibrationCheckBox.setModel(new DOMButtonModel(this, "CalibrationActive")); //$NON-NLS-1$
            mDoCalibrationCheckBox.setMnemonic(KeyEvent.VK_C);
            mDoCalibrationCheckBox.setText(Messages.getString("ConfigurationEditor.13")); //$NON-NLS-1$
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
    	if(mRunCostCurvesCheckBox == null){
    		mRunCostCurvesCheckBox = new JCheckBox();
    		mRunCostCurvesCheckBox.setModel(new DOMButtonModel(this, "createCostCurve")); //$NON-NLS-1$
    		mRunCostCurvesCheckBox.setMnemonic(KeyEvent.VK_O);
    		mRunCostCurvesCheckBox.setText(Messages.getString("ConfigurationEditor.128")); //$NON-NLS-1$
    		mRunCostCurvesCheckBox.setToolTipText(Messages.getString("ConfigurationEditor.129")); //$NON-NLS-1$
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
            mDoBatchModeCheckBox.setModel(new DOMButtonModel(this, "BatchMode")); //$NON-NLS-1$
            mDoBatchModeCheckBox.setMnemonic(KeyEvent.VK_B);
            mDoBatchModeCheckBox.setText(Messages.getString("ConfigurationEditor.16")); //$NON-NLS-1$
            mDoBatchModeCheckBox.setToolTipText(Messages
                    .getString("ConfigurationEditor.17")); //$NON-NLS-1$
            
            // Add a listener to enable and disable the related options.
            mDoBatchModeCheckBox.addChangeListener(new ChangeListener(){
            	public void stateChanged(ChangeEvent aEvent){
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
            mMainToolBar.add(getOpenButton());
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
            mFileMenu.add(getOpenMenuItem());
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
        }
        return mSaveButton;
    }

    /**
     * This method initializes mNewButton
     * 
     * @return javax.swing.JButton
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
     * This method initializes mOpenButton
     * 
     * @return javax.swing.JButton
     */
    private JButton getOpenButton() {
        if (mOpenButton == null) {
            mOpenButton = new JButton();
            mOpenButton.setAction(new LoadAction(this));
            mOpenButton.setText(Messages.getString("ConfigurationEditor.36")); //$NON-NLS-1$
            mOpenButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.37")); //$NON-NLS-1$
            mOpenButton.setMnemonic(KeyEvent.VK_O);
        }
        return mOpenButton;
    }

    /**
     * This method initializes mRunButton
     * 
     * @return javax.swing.JButton
     */
    private JButton getRunButton() {
        if (mRunButton == null) {
            mRunButton = new JButton();
            mRunButton.setAction(new RunAction(this));
            mRunButton.setMnemonic(KeyEvent.VK_R);
            mRunButton.setText(Messages.getString("ConfigurationEditor.38")); //$NON-NLS-1$
            mRunButton.setToolTipText(Messages
                    .getString("ConfigurationEditor.39")); //$NON-NLS-1$
        }
        return mRunButton;
    }

    /**
     * This method initializes mQuitMenuItem
     * 
     * @return javax.swing.JMenuItem
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
     * This method initializes mNewMenuItem
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
     * This method initializes mOpenMenuItem
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getOpenMenuItem() {
        if (mOpenMenuItem == null) {
            mOpenMenuItem = new JMenuItem();
            mOpenMenuItem.setActionCommand("Load"); //$NON-NLS-1$
            mOpenMenuItem.setAction(new LoadAction(this));
            mOpenMenuItem.setText(Messages.getString("ConfigurationEditor.48")); //$NON-NLS-1$
            mOpenMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.49")); //$NON-NLS-1$
            mOpenMenuItem.setMnemonic(KeyEvent.VK_O);
        }
        return mOpenMenuItem;
    }

    /**
     * This method initializes mSaveMenuItem
     * 
     * @return javax.swing.JMenuItem
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
     * This method initializes mSaveAsMenuItem
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getSaveAsMenuItem() {
        if (mSaveAsMenuItem == null) {
            mSaveAsMenuItem = new JMenuItem();
            mSaveAsMenuItem.setAction(new SaveAction(this));
            mSaveAsMenuItem
                    .setText(Messages.getString("ConfigurationEditor.53")); //$NON-NLS-1$
            mSaveAsMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.54")); //$NON-NLS-1$
            mSaveAsMenuItem.setActionCommand("SaveAs"); //$NON-NLS-1$
            mSaveAsMenuItem.setMnemonic(KeyEvent.VK_A);
        }
        return mSaveAsMenuItem;
    }

    /**
     * This method initializes mRunMenuItem
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getRunMenuItem() {
        if (mRunMenuItem == null) {
            mRunMenuItem = new JMenuItem();
            mRunMenuItem.setActionCommand("Run"); //$NON-NLS-1$
            mRunMenuItem.setAction(new RunAction(this));
            mRunMenuItem.setText(Messages.getString("ConfigurationEditor.56")); //$NON-NLS-1$
            mRunMenuItem.setToolTipText(Messages
                    .getString("ConfigurationEditor.57")); //$NON-NLS-1$
            mRunMenuItem.setMnemonic(KeyEvent.VK_R);
        }
        return mRunMenuItem;
    }

    /**
     * This method initializes mEditMenu
     * 
     * @return javax.swing.JMenu
     */
    private JMenu getEditMenu() {
        if (mEditMenu == null) {
            mEditMenu = new JMenu();
            mEditMenu.setMnemonic(KeyEvent.VK_E);
            mEditMenu.setText(Messages.getString("ConfigurationEditor.73")); //$NON-NLS-1$
            mEditMenu.setToolTipText(Messages.getString("ConfigurationEditor.74")); //$NON-NLS-1$
            mEditMenu.add(getPreferencesMenuItem());
            mEditMenu.add(getEditLogsMenuItem());
        }
        return mEditMenu;
    }

    /**
     * This method initializes mPreferencesMenuItem
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getPreferencesMenuItem() {
        if (mPreferencesMenuItem == null) {
            mPreferencesMenuItem = new JMenuItem();
            mPreferencesMenuItem.setActionCommand("ShowPreferences"); //$NON-NLS-1$
            mPreferencesMenuItem.setAction(new ShowPreferencesAction(this));
            mPreferencesMenuItem.setText(Messages.getString("ConfigurationEditor.77")); //$NON-NLS-1$
            mPreferencesMenuItem.setToolTipText(Messages.getString("ConfigurationEditor.78")); //$NON-NLS-1$
            mPreferencesMenuItem.setMnemonic(KeyEvent.VK_P);
        }
        return mPreferencesMenuItem;
    }

    /**
     * This method initializes mEditLogsMenuItem
     * 
     * @return javax.swing.JMenuItem
     */
    private JMenuItem getEditLogsMenuItem() {
        if (mEditLogsMenuItem == null) {
        	mEditLogsMenuItem = new JMenuItem();
        	mEditLogsMenuItem.setActionCommand("EditLogs"); //$NON-NLS-1$
        	mEditLogsMenuItem.setAction(new EditLogSettingsAction(this));
        	mEditLogsMenuItem.setText(Messages.getString("ConfigurationEditor.136")); //$NON-NLS-1$
        	mEditLogsMenuItem.setToolTipText(Messages.getString("ConfigurationEditor.137")); //$NON-NLS-1$
        	mEditLogsMenuItem.setMnemonic(KeyEvent.VK_L);
        }
        return mEditLogsMenuItem;
    }
    
    /**
     * This method initializes mBatchPane
     * 
     * @return The pane containing all batch configuration information.
     */
    private JPanel getBatchPane() {
        if (mBatchPane == null) {
            mBatchPane = new JPanel();
            mBatchPane.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
            mBatchPane.setToolTipText(Messages.getString("ConfigurationEditor.138")); //$NON-NLS-1$
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
            mMergeMenuItem.setText(Messages.getString("ConfigurationEditor.98")); //$NON-NLS-1$
            mMergeMenuItem
                    .setToolTipText(Messages.getString("ConfigurationEditor.99")); //$NON-NLS-1$
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
            mHelpMenu.setToolTipText(Messages.getString("ConfigurationEditor.101")); //$NON-NLS-1$
        }
        return mHelpMenu;
    }
    
    /**
     * Dispatch the save event. This isn't good.
     */
    public void dispatchSave() {
        new SaveAction(this).actionPerformed(
                new ActionEvent(this, ActionEvent.ACTION_PERFORMED, Messages.getString("ConfigurationEditor.103"))); //$NON-NLS-1$
    }
    
    /** 
     * Enable or disable the group of batch file input objects.
     * @param aEnable Whether to enable or disable.
     */
    private void enableBatchInputFields(boolean aEnable){
        getBatchFileLabel().setEnabled(aEnable);
    	getBatchFileField().setEnabled(aEnable);
    	getBatchFileSelectButton().setEnabled(aEnable);
        getBatchFileNewButton().setEnabled(aEnable);
        // Only enable the batch file edit button if a file name has been
        // set.
    	getBatchFileEditButton().setEnabled(getBatchFileField().getText().length() > 0);
    }
    
    /**
     * This method initializes the batch file text field.
     * 	
     * @return The batch file text field.
     */    
    private JTextField getBatchFileField() {
    	if (mBatchFileField == null) {
    		mBatchFileField = mTextFieldFactory.createTextField("Files", "BatchFileName"); //$NON-NLS-1$ //$NON-NLS-2$
    		mBatchFileField.setPreferredSize(new Dimension(200,20));
    		mBatchFileField.setToolTipText(Messages.getString("ConfigurationEditor.141")); //$NON-NLS-1$
            // Add an action listener to enable the edit button.
            mBatchFileField.addKeyListener(new KeyListener() {
                public void keyTyped(KeyEvent aEvent) {
                    // Activate the edit button if there is text in the field.
                    getBatchFileEditButton().setEnabled(mBatchFileField.getText().length() > 0);
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
    		mBatchFileSelectButton.setText(Messages.getString("ConfigurationEditor.142")); //$NON-NLS-1$
    		mBatchFileSelectButton.setToolTipText(Messages.getString("ConfigurationEditor.143")); //$NON-NLS-1$
    		mBatchFileSelectButton.setMnemonic(KeyEvent.VK_S);
            mBatchFileSelectButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    JFileChooser batchFileChooser  = new JFileChooser();
                    batchFileChooser.setFileFilter(new XMLFileFilter());
                    int rv = batchFileChooser.showDialog(getMainWindow(), Messages.getString("ConfigurationEditor.144")); //$NON-NLS-1$
                    if (rv == JFileChooser.APPROVE_OPTION) {
                        File selectedFile = batchFileChooser.getSelectedFile();
                        if( selectedFile != null ) {
                            // Set the text field.
                            getBatchFileField().setText(selectedFile.getAbsolutePath());
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
     * @return The new batch file button.
     */
    private JButton getBatchFileNewButton() {
        if(mBatchFileNewButton == null) {
            mBatchFileNewButton = new JButton();
            mBatchFileNewButton.setText(Messages.getString("ConfigurationEditor.145")); //$NON-NLS-1$
            mBatchFileNewButton.setToolTipText(Messages.getString("ConfigurationEditor.146")); //$NON-NLS-1$
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
                        if(newFile.exists()){
                        	String errorMessage = Messages.getString("ConfigurationEditor.147"); //$NON-NLS-1$
                        	String errorTitle = Messages.getString("ConfigurationEditor.148"); //$NON-NLS-1$
                            JOptionPane.showMessageDialog(getMainWindow(), errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE );
                            return;
                        }
                        BatchFileEditor batchEditor = new BatchFileEditor(newFile.getAbsolutePath(), true);
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
    		mBatchFileEditButton.setText(Messages.getString("ConfigurationEditor.149")); //$NON-NLS-1$
    		mBatchFileEditButton.setToolTipText(Messages.getString("ConfigurationEditor.150")); //$NON-NLS-1$
    		mBatchFileEditButton.setMnemonic(KeyEvent.VK_E);
            mBatchFileEditButton.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent aEvent) {
                    // Create a new batch file editor.
                    String batchFile = getBatchFileField().getText();
                    BatchFileEditor batchEditor = new BatchFileEditor(batchFile, false);
                    batchEditor.pack();
                    batchEditor.setVisible(true);
                }
            });
    	}
    	return mBatchFileEditButton;
    }

} // @jve:decl-index=0:visual-constraint="50,28"

