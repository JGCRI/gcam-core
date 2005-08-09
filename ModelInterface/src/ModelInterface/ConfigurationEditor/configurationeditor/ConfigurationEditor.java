/**
 * 
 */
package ModelInterface.ConfigurationEditor.configurationeditor;

import ModelInterface.ConfigurationEditor.guihelpers.ComponentEnabler;
import ModelInterface.ConfigurationEditor.guihelpers.DOMDocumentSaveSetter;
import ModelInterface.ConfigurationEditor.guihelpers.SaveEnabler;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

import org.w3c.dom.Document;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;
import org.w3c.dom.events.EventTarget;
import org.w3c.dom.events.MutationEvent;

import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ConfigurationEditor.utils.Messages;
import ModelInterface.ConfigurationEditor.actions.EditLogSettingsAction;
import ModelInterface.ConfigurationEditor.actions.LoadAction;
import ModelInterface.ConfigurationEditor.actions.NewAction;
import ModelInterface.ConfigurationEditor.actions.QuitAction;
import ModelInterface.ConfigurationEditor.actions.RunAction;
import ModelInterface.ConfigurationEditor.actions.SaveAction;

/**
 * The class which creates the UI to edit a configuration file. TODO: Work out
 * where invokeLater should be used.
 * 
 * @author Josh Lurz
 */
public class ConfigurationEditor extends JFrame implements DOMDocumentEditor {

    /**
     * Automatically generated unique class identifier.
     */
    private static final long serialVersionUID = -4743689341303656460L;

    /**
     * The name of the leaf elements containing configuration information.
     */
    static final String ELEMENT_NAME = "Value"; //$NON-NLS-1$

    /**
     * The name of the root element which contains information in the DOM tree
     * about this object.
     */
    public static final String ROOT_ELEMENT_NAME = "Configuration"; //$NON-NLS-1$

    /**
     * The current parsed XML document.
     */
    private transient Document mCurrentDocument = null;

    /**
     * Constructor which initializes the UI.
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
        setJMenuBar(createMainMenuBar());
        setName(Messages.getString("ConfigurationEditor.0")); //$NON-NLS-1$
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new QuitAction(this));
        setContentPane(createMainWindow());
        setTitle(Messages.getString("ConfigurationEditor.1")); //$NON-NLS-1$
        
        // Fire a blank document replaced with a blank document so
        // the UI can initialize itself. The old value and new value
        // must be different for the property change to fire.
        firePropertyChange("document-replaced", "unknown", null);
    }

    /**
     * Set the current document.
     * 
     * @param aNewDocument
     *            The document to set as the current.
     */
    public void setDocument(final Document aNewDocument) {
        // Save the old document temporarily for the event.
        final Document oldDocument = mCurrentDocument;

        synchronized (aNewDocument) {
            // Store the new document.
            mCurrentDocument = aNewDocument;
        }

        // Add listeners onto the document.
        addDocumentListeners(mCurrentDocument);

        // Alert any listeners that the document changed.
        firePropertyChange("document-replaced", oldDocument, mCurrentDocument);

        // Update the user interface.
        SwingUtilities.invokeLater(new Runnable() {
            synchronized public void run() {
                repaint();
            }
        });
    }

    /**
     * Add listeners onto a document which will mark the document as needing
     * saving and notify any listeners on the editor that the document is now
     * modified.
     * 
     * @param aDocument
     *            Document onto which to add listeners.
     */
    private void addDocumentListeners(final Document aDocument) {
        // Remove the current document modified property change listener
        // as it is hooked to the old document.
        PropertyChangeListener docModifiedListeners[] = getPropertyChangeListeners("document-modified");
        for (int i = 0; i < docModifiedListeners.length; ++i) {
            removePropertyChangeListener("document-modified",
                    docModifiedListeners[i]);
        }

        // Add listeners onto the new document.
        if (aDocument != null) {
            // Add a property change listener which will set the document as
            // modified.
            addPropertyChangeListener("document-modified",
                    new DOMDocumentSaveSetter(aDocument));

            final EventTarget target = (EventTarget) aDocument;
            // Add an event handler which will listen for the document-saved
            // attribute being added to the document which signals that the
            // document has been saved successfully.
            // Need to make sure this doesn't cause the document to be dirty.
            target.addEventListener("DOMAttrModified", new EventListener() {
                public void handleEvent(Event aEvent) {
                    MutationEvent event = (MutationEvent) aEvent;
                    if (event.getAttrName().equals("document-saved")
                            && event.getAttrChange() == MutationEvent.ADDITION) {
                        firePropertyChange("document-saved", false, true);
                        aDocument.getDocumentElement().removeAttribute(
                                "document-saved");
                    }
                }
            }, false);

            // Add an event listener which will dispatch an event on the editor
            // that the document has been modified.
            target.addEventListener("DOMSubtreeModified", new EventListener() {
                public void handleEvent(Event aEvent) {
                    System.out.println("HANDLIN EVENT");
                    // Don't fire events if the source is the root element
                    // because they may be meta-tags being added or removed.
                    MutationEvent event = (MutationEvent)aEvent;
                    if(event.getRelatedNode().equals(aDocument.getDocumentElement())) {
                        System.out.println("Ignoring event on root.");
                    }
                    else {
                        System.out.println("Firing document modified");
                        // Check if the document was already dirty.
                        firePropertyChange("document-modified", FileUtils.isDirty(aDocument), true);
                    }
                }
            }, false);
        }
    }

    /**
     * Get the current document.
     * 
     * @return The current document.
     */
    public Document getDocument() {
        synchronized (this) {
            return mCurrentDocument;
        }
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

        // Create the main options panel.
        final AbstractEditorPanel mainOptionsPanel = new MainOptionsPanel();
        // Set it to listen for property change events.
        addPropertyChangeListener(mainOptionsPanel);

        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.5"), null, mainOptionsPanel, Messages.getString("ConfigurationEditor.6")); //$NON-NLS-1$ //$NON-NLS-2$

        // Create the batch options panel.
        final AbstractEditorPanel batchOptionsPanel = new BatchOptionsPanel();
        // Set it to listen for property change events.
        addPropertyChangeListener(batchOptionsPanel);
        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.110"), null, batchOptionsPanel, Messages.getString("ConfigurationEditor.111")); //$NON-NLS-1$ //$NON-NLS-2$
        // Whats the difference between the two strings?

        // Create the advanced panel.
        final AbstractEditorPanel advancedPanel = new AdvancedOptionsPanel();
        // Set it to listen for property change events.
        addPropertyChangeListener(advancedPanel);
        tabContainer
                .addTab(
                        Messages.getString("ConfigurationEditor.7"), null, advancedPanel, Messages.getString("ConfigurationEditor.8")); //$NON-NLS-1$ //$NON-NLS-2$
        return tabContainer;
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
        mainToolBar.add(createSaveButton());
        mainToolBar.add(createRunButton());
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
        fileMenu.setName("FileMenu");
        fileMenu.setToolTipText(Messages.getString("ConfigurationEditor.25")); //$NON-NLS-1$
        fileMenu.setMnemonic(KeyEvent.VK_F);

        // Add the menu items.
        fileMenu.add(createNewMenuItem());
        fileMenu.add(createLoadMenuItem());
        fileMenu.add(createSaveMenuItem());
        fileMenu.add(createSaveAsMenuItem());
        fileMenu.add(createRunMenuItem());
        fileMenu.add(createQuitMenuItem());
        return fileMenu;
    }

    /**
     * This method creates and initializes the save button.
     * 
     * @return The save button.
     */
    private JButton createSaveButton() {
        final JButton saveButton = new JButton();
        saveButton.setActionCommand("Save"); //$NON-NLS-1$
        saveButton.setAction(new SaveAction(this));
        saveButton.setName("SaveDocumentButton");
        saveButton.setToolTipText(Messages.getString("ConfigurationEditor.29")); //$NON-NLS-1$
        saveButton.setText(Messages.getString("ConfigurationEditor.30")); //$NON-NLS-1$

        // Add a property change listener which will activate the save button
        // when the document is modified.
        addPropertyChangeListener(new SaveEnabler(saveButton));
        return saveButton;
    }

    /**
     * This method initializes the new button.
     * 
     * @return The new button.
     */
    private JButton createNewButton() {
        final JButton newButton = new JButton(Messages
                .getString("ConfigurationEditor.32")); //$NON-NLS-1$
        newButton.setName("NewDocumentButton");
        newButton.setAction(new NewAction());
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
        loadButton.setAction(new LoadAction());
        loadButton.setName("LoadDocumentButton");
        loadButton.setToolTipText(Messages.getString("ConfigurationEditor.37")); //$NON-NLS-1$
        loadButton.setMnemonic(KeyEvent.VK_O);
        return loadButton;
    }

    /**
     * This method initializes the run button.
     * 
     * @return The run button.
     */
    private JButton createRunButton() {
        final JButton runButton = new JButton(Messages
                .getString("ConfigurationEditor.38")); //$NON-NLS-1$
        runButton.setName("RunModelButton");
        runButton.setAction(new RunAction(this));
        runButton.setMnemonic(KeyEvent.VK_R);
        runButton.setToolTipText(Messages.getString("ConfigurationEditor.39")); //$NON-NLS-1$
        // Add a property change listener that will enable this button
        // when the document is non-null.
        addPropertyChangeListener("document-replaced", new ComponentEnabler(
                runButton));
        return runButton;
    }

    /**
     * This method initializes the quit menu item.
     * 
     * @return The quit menu item.
     */
    private JMenuItem createQuitMenuItem() {
        final JMenuItem quitItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.41"));
        quitItem.setName("QuitMenuItem");
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
        newItem.setName("NewMenuItem");
        newItem.setAction(new NewAction());
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
        loadItem.setName("LoadMenuItem");
        loadItem.setAction(new LoadAction());
        loadItem.setToolTipText(Messages.getString("ConfigurationEditor.49")); //$NON-NLS-1$
        loadItem.setMnemonic(KeyEvent.VK_O);
        return loadItem;
    }

    /**
     * This method initializes the save menu item.
     * 
     * @return The save menu item.
     */
    private JMenuItem createSaveMenuItem() {
        final JMenuItem saveItem = new JMenuItem();
        saveItem.setName("SaveMenuItem");
        saveItem.setActionCommand("Save"); //$NON-NLS-1$
        saveItem.setMnemonic(KeyEvent.VK_S);
        saveItem.setAction(new SaveAction(this));
        saveItem.setToolTipText(Messages.getString("ConfigurationEditor.50")); //$NON-NLS-1$

        // Do this after setting the action command to override the action
        // command name as the name in the menu.
        saveItem.setText(Messages.getString("ConfigurationEditor.51")); //$NON-NLS-1$
        // Add a property change listener which will activate the save button
        // when the document is modified.
        addPropertyChangeListener("document-replaced",
                new SaveEnabler(saveItem));
        return saveItem;
    }

    /**
     * This method initializes the save as menu item.
     * 
     * @return The save as menu item.
     */
    private JMenuItem createSaveAsMenuItem() {
        final JMenuItem saveAsItem = new JMenuItem(); //$NON-NLS-1$
        saveAsItem.setName("SaveAsMenuItem");
        saveAsItem.setAction(new SaveAction(this));
        saveAsItem.setToolTipText(Messages.getString("ConfigurationEditor.54")); //$NON-NLS-1$
        saveAsItem.setActionCommand("SaveAs"); //$NON-NLS-1$
        saveAsItem.setMnemonic(KeyEvent.VK_A);

        // Do this after setting the action command to override the action
        // command name as the name in the menu.
        saveAsItem.setText(Messages.getString("ConfigurationEditor.53"));
        // Add a property change listener that will enable the button when it is
        // non-null.
        addPropertyChangeListener("document-replaced", new ComponentEnabler(
                saveAsItem));
        return saveAsItem;
    }

    /**
     * This method initializes the run menu item.
     * 
     * @return The run menu item.
     */
    private JMenuItem createRunMenuItem() {
        final JMenuItem runItem = new JMenuItem(Messages
                .getString("ConfigurationEditor.56")); //$NON-NLS-1$
        runItem.setName("RunMenuItem");
        runItem.setAction(new RunAction(this));
        runItem.setToolTipText(Messages.getString("ConfigurationEditor.57")); //$NON-NLS-1$
        runItem.setMnemonic(KeyEvent.VK_R);

        // Add a property change listener that will enable this button
        // when the document is non-null.
        addPropertyChangeListener("document-replaced", new ComponentEnabler(
                runItem));

        return runItem;
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
}
