/*
 */
package batchcreator;

import interfaceutils.DOMListPanel;
import interfaceutils.DOMListPanelFactory;
import interfaceutils.Util;

import javax.swing.JFrame;
import org.w3c.dom.Document;

import com.zookitec.layout.ComponentEF;
import com.zookitec.layout.ContainerEF;
import com.zookitec.layout.ExplicitConstraints;
import com.zookitec.layout.ExplicitLayout;
import com.zookitec.layout.GroupEF;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Creates a window which has the capability to create or edit a batch file.
 * The selection of the batch file is done by the ConfigurationEditor and passed
 * to this class. The class is composed of three DOMFileListPanels, along with confirmation
 * and cancel buttons.
 * @author Josh Lurz
 */
public class BatchFileEditor extends JFrame {
    /**
	 * Automatically generated unique class identifier.
	 */
	private static final long serialVersionUID = 5932800223359767162L;

    /**
     * The parsed batch file XML document.
     */
    private Document mDocument = null;

    /**
     * The main content pane.
     */
    private JPanel mMainContentPane = null;

    /**
     * The left panel.
     */
    private JPanel mLeftPanel = null;

    /**
     * The middle panel.
     */
    private JPanel mMiddlePanel = null;
    
    /**
     * The right panel.
     */
    private JPanel mRightPanel = null;
    
    /**
     * The cancel button.
     */
    private JButton mCancelButton = null;
    
    /**
     * The OK button.
     */
    private JButton mOKButton = null;
    
    /**
     * The name of the batch file root element.
     */
    private static final String ROOT_ELEMENT_NAME = "ComponentSets"; //$NON-NLS-1$
    //  @jve:decl-index=0:
    
    /**
     * A factory which instantiates the list panels.
     */
    private DOMListPanelFactory mListPanelFactory = null;
    
    /**
     * This is the default constructor
     * @param aInitialFile The file to open immediately in the editor.
     * @param aIsNewFile Whether this is an existing or new file.
     */
    public BatchFileEditor(String aInitialFile, boolean aIsNewFile) {
        super();
        mListPanelFactory = new DOMListPanelFactory();
        initialize(aInitialFile, aIsNewFile);
    }
    
    /**
     * Get the current document.
     * @return The current document.
     */
    public Document getDocument() {
        return mDocument;
    }
    
    /**
     * Create a new document and set it is the current document.
     * @param aFileName The name to save the document as.
     *
     */
    private void createNewDocument(String aFileName) {
        // Create a new document.
        mDocument = Util.getDocumentBuilder(this).newDocument();
       
        // Add the root element onto it.
        mDocument.appendChild(mDocument.createElement(ROOT_ELEMENT_NAME));
        
        // Set the name of the file into the document.
        Util.setDocumentFile(mDocument, new File(aFileName));
        
        // Put up a message telling the user that a new file was created,
        // otherwise there is no feedback for this action.
        String message = Messages.getString("BatchFileEditor.1"); //$NON-NLS-1$
        String messageTitle = Messages.getString("BatchFileEditor.2"); //$NON-NLS-1$
        Logger.global.log(Level.INFO, message);
        JOptionPane.showMessageDialog(this, message, messageTitle,
                JOptionPane.INFORMATION_MESSAGE);
    }
    
    /**
     * Load a document into the current document.
     * @param aFileName The path to a file to load.
     */
    private void loadDocument(String aFileName) {
        try {
        	File newFile = new File(aFileName);
        	// Check if the file exists.
        	if(!newFile.exists()){
        		// Tell the user the file does not exist.
                String message = Messages.getString("BatchFileEditor.3"); //$NON-NLS-1$
                String messageTitle = Messages.getString("BatchFileEditor.4"); //$NON-NLS-1$
                Logger.global.log(Level.SEVERE, message);
                JOptionPane.showMessageDialog(this, message, messageTitle,
                        JOptionPane.ERROR_MESSAGE);
                return;
        	}
            mDocument = Util.getDocumentBuilder(this).parse(aFileName);
            Util.setDocumentFile(mDocument, newFile );
        } catch (Exception e) {
        	// Report the error to the user.
            String message = Messages.getString("BatchFileEditor.5") + e.getMessage(); //$NON-NLS-1$
            String messageTitle = Messages.getString("BatchFileEditor.6"); //$NON-NLS-1$
            Logger.global.log(Level.SEVERE, message);
            JOptionPane.showMessageDialog(this, message, messageTitle,
                    JOptionPane.ERROR_MESSAGE);
            mDocument = null;
        }
    }

    /**
     * This method initializes the main content pane.
     * 
     * @return The main content pane.
     */
    private JPanel getMainContentPane() {
        if (mMainContentPane == null) {
            mMainContentPane = new JPanel(new ExplicitLayout());
            mMainContentPane.setToolTipText(Messages.getString("BatchFileEditor.7")); //$NON-NLS-1$
            mMainContentPane.add(getLeftPanel(), 
                    new ExplicitConstraints(getLeftPanel(),
                            ContainerEF.left(this).add(5), 
                            ContainerEF.top(this).add(5)));
            
            mMainContentPane.add(getMiddlePanel(), 
                    new ExplicitConstraints(getMiddlePanel(),
                            ComponentEF.right(getLeftPanel()).add(5), 
                            ContainerEF.top(this).add(5)));
            
            mMainContentPane.add(getRightPanel(), 
                    new ExplicitConstraints(getRightPanel(),
                            ComponentEF.right(getMiddlePanel()).add(5),
                            ContainerEF.top(this).add(5)));
            // Add okay and cancel buttons to the content pane.
            addOKCancelButtons();
            
            // Calculate the width to be the width of all the panels.
            Component panels[] = { getLeftPanel(), getMiddlePanel(), getRightPanel() };
            double width = GroupEF.preferredWidthSum(panels).getValue((ExplicitLayout)mMainContentPane.getLayout());
            // Add some space for gaps.
            width += 25;
            
            // Calculate the height to be the max height of the panels plus the OK button.
            double height = GroupEF.preferredHeightMax(panels).getValue((ExplicitLayout)mMainContentPane.getLayout());
            height += GroupEF.preferredHeightMax( new Component[]{ mOKButton, mCancelButton }).getValue((ExplicitLayout)mMainContentPane.getLayout());
            height += 55; // TODO: Remove this hack.
            setPreferredSize(new Dimension((int)Math.round(width), (int)Math.round(height)));
        }
        return mMainContentPane;
    }

    /**
     * Add OK and Cancel buttons to the batch editor which will
     * save or discard any changes to the fields. Only call from
     * inside getMainContentPane.
     *
     */
    private void addOKCancelButtons() {
        // Add a cancel button. Cancel button is added first so the 
        // OK button can position itself based on the cancel button.
        mCancelButton = new JButton();
        mCancelButton.setToolTipText(Messages.getString("BatchFileEditor.8") ); //$NON-NLS-1$
        mCancelButton.setText(Messages.getString("BatchFileEditor.9")); //$NON-NLS-1$
        // Add a listener which will close the window.
        mCancelButton.addActionListener(new ActionListener() { 
                    public void actionPerformed(ActionEvent aEvent) {
                        getMainWindow().dispose();
                    }
                });
        mMainContentPane.add(mCancelButton,
                new ExplicitConstraints(mCancelButton,
                        ComponentEF.right(getRightPanel()).subtract(ComponentEF.width(mCancelButton).add(2)),
                        ComponentEF.bottom(getRightPanel()).add(5)));
        
        // Add an OK button.
        mOKButton = new JButton();
        mOKButton.setToolTipText(Messages.getString("BatchFileEditor.10")); //$NON-NLS-1$
        mOKButton.setText(Messages.getString("BatchFileEditor.11")); //$NON-NLS-1$
        
        // Add a listener which will save the batch file and close the
        // window.
        mOKButton.addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent aEvent) {
                        // Save the batch file document.
                        Util.serializeDocument(mDocument, getMainWindow());
                        getMainWindow().dispose();
                    }
                });
        mMainContentPane.add(mOKButton,
                new ExplicitConstraints( mOKButton,
                        ComponentEF.left(mCancelButton).subtract( ComponentEF.width(mOKButton).add(5)),
                        ComponentEF.bottom(getRightPanel()).add(5)));


    }
    
    /**
     * Private helper which returns the main window.
     * TODO: Is there a cleaner way?
     */
    private JFrame getMainWindow() {
        return this;
    }
    
    /**
     * This method initializes the left panel.
     * 
     * @return The left panel.
     */
    private JPanel getLeftPanel() {
        if (mLeftPanel == null) {
            mLeftPanel = mListPanelFactory.createDOMFileListPanel("/ComponentSets", "ComponentSet", "Component Sets", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            ((DOMListPanel)mLeftPanel).getList().addListSelectionListener(((DOMListPanel)getMiddlePanel()).getListModel());
            ((DOMListPanel)mLeftPanel).getListModel().addListDataListener(((DOMListPanel)getMiddlePanel()).getListModel());
        }
        return mLeftPanel;
    }

    /**
     * This method initializes the middle panel.
     * 
     * @return The middle panel.
     */
    private JPanel getMiddlePanel() {
        if (mMiddlePanel == null) {
            mMiddlePanel = mListPanelFactory.createDOMFileListPanel("ComponentSet", "FileSet", "File Sets", false); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            ((DOMListPanel)mMiddlePanel).getList().addListSelectionListener(((DOMListPanel)getRightPanel()).getListModel());
            ((DOMListPanel)mMiddlePanel).getListModel().addListDataListener(((DOMListPanel)getRightPanel()).getListModel());
        }
        return mMiddlePanel;
    }

    /**
     * This method initializes the right panel.
     * 
     * @return The right panel.
     */
    private JPanel getRightPanel() {
        if (mRightPanel == null) {
            mRightPanel = mListPanelFactory.createDOMFileListPanel("FileSet", Messages.getString("BatchFileEditor.19"), Messages.getString("BatchFileEditor.20"), true); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        return mRightPanel;
    }

    /**
     * This method initializes the main batch editor window.
     * @param aFileName The name of the file to load or create.
     * @param aIsNewFile Whether the file should be created.
     */
    private void initialize(String aFileName, boolean aIsNewFile) {
        if(aIsNewFile){
            createNewDocument(aFileName);
        }
        else {
        	// Try and load the document
        	loadDocument(aFileName);
        }

        // If the document could not be loaded, close this window.
        // TODO: This doesn't quite work.
        if(mDocument == null){
        	dispose();
        }
        
        // Set the document into the lists.
        mListPanelFactory.setDocument(mDocument);
        // Don't use the default closer.
        this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new WindowCloseListener());
        this.setContentPane(getMainContentPane());
        
        // Initialize list parents.
        // TODO: Find a better way to do this.
        ((DOMListPanel)mMiddlePanel).getListModel().setParentList(((DOMListPanel)mLeftPanel).getList());
        ((DOMListPanel)mRightPanel).getListModel().setParentList(((DOMListPanel)mMiddlePanel).getList());
  
        this.setTitle(Messages.getString("BatchFileEditor.21")); //$NON-NLS-1$
    }
}  //  @jve:decl-index=0:visual-constraint="13,20"

