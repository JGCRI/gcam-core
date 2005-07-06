/**
 * 
 */
package interfaceutils;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import org.w3c.dom.Document;

import interfaceutils.ButtonSetEnabler;

/**
 * A panel which contains a list based on an underlying DOM tree, and buttons
 * to add, remove, move up, or move down entries in the list.
 * @author Josh Lurz
 */
public class DOMListPanel extends JPanel {
	/**
	 * Unique Identifier required for serialization.
	 */
	private static final long serialVersionUID = 216029089953286071L;
	
	/**
	 * The label to title this panel with.
	 */
	private String mPanelLabel = null;
	
    /**
     * The name of elements of this list.
     */
    private String mElementName = null;
    
    /**
     * The name of the element which contains this list.
     */
    private String mContainerName = null;
    
	/**
	 * The add button.
	 */
	private JButton mAddButton = null;
	
	/**
	 * The delete button.
	 */
	private JButton mDeleteButton = null;
	
	/**
	 * The up button.
	 */
	private JButton mUpButton = null;
	
	/**
	 * The down button.
	 */
	private JButton mDownButton = null;
	
	/**
	 * The list of files.
	 */
	private JList mList = null;
	
	/**
	 * The file list model.
	 */
	private DOMListModel mListModel = null;
	
	/**
	 * The scroll pane containing the file list.
	 */
	private JScrollPane mListScrollPane = null;
	
	/**
	 * The controller for enabling and disabling the set of buttons.
	 */
    private ButtonSetEnabler mFileListEnabler = null;
    
	/**
	 * Whether the children of this list are leaves of the DOM tree, if 
	 * not they have children with child nodes. This determines whether 
	 * the items in the list are files or nodes.
	 */
	private boolean mChildrenAreLeaves = false;
	
	/**
	 * Constructor which calls the default JPanel constructor. This
	 * will implicitly use a FlowLayoutManager and turn on double-bufferring.
	 * @param aElementName The name of elements in the list.
     * @param aContainerName The name of the container element for the list.
	 * @param aPanelLabel The label to put at the top of the panel.
	 * @param aChildrenAreLeaves Whether the elements of this list are leaves. Leaf lists
	 * will add new files as new items, node lists will add elements as new items.
	 */
	public DOMListPanel(String aElementName, String aContainerName,
            String aPanelLabel, boolean aChildrenAreLeaves) {
		super(new GridBagLayout());
		assert(aElementName != null);
		mElementName = aElementName;
        mContainerName = aContainerName;
		mPanelLabel = aPanelLabel;
		mChildrenAreLeaves = aChildrenAreLeaves;
		initialize();
	}
	
    /**
     * Set the underlying document.
     * @param aDocument The new document.
     */
    public void setDocument(Document aDocument){
        getListModel().setDocument(aDocument);
        getButtonEnabler().valueChanged(null);
    }
	/**
	 * Initialize the panel before viewing.
	 *
	 */
	private void initialize(){
        setBorder(BorderFactory.createEtchedBorder());
		// Setup the frame.
        
        // Create a grid bag constraint.
        GridBagConstraints cons = new GridBagConstraints();
        
        // Create a border around the entire panel.
        cons.insets = new Insets(5, 5, 5, 5);
        
        // Use internal spacing between components.
        cons.ipadx = 5;
        cons.ipady = 5;
        
        // Start at (0,0).
        cons.gridx = 0;
        cons.gridy = 0;
        cons.gridwidth = 2;
        // Don't fill empty space.
        cons.fill = GridBagConstraints.NONE;
        
        // Put the label in the center.
        cons.anchor = GridBagConstraints.CENTER;
        
        // Add a label at the top.
        JLabel panelLabel = new JLabel(mPanelLabel);
        add(panelLabel, cons);
        
        // Put the scroll pane at (0,1)
        cons.gridx = 0;
        cons.gridy = 1;
        
        // Make the scroll pane absorb 4 cells.
        cons.gridheight = 4;
        
        // The scroll pane should absorb extra space.
        cons.weightx = 1;
        cons.weighty = 1;
        cons.fill = GridBagConstraints.BOTH;
        
        // Center the scroll pane.
        cons.anchor = GridBagConstraints.CENTER;
        // Add the list scroll pane.
        add(getListScrollPane(), cons);
        

        // Don't allow buttons to absorb space.
        cons.fill = GridBagConstraints.NONE;
        cons.weightx = 0;
        cons.weighty = 0;
        
        // Only take up one cell.
        cons.gridheight = 1;
        cons.gridwidth = 1;
        
        // Add the up button.
        // Put the up button at (3,3)
        cons.gridx = 3;
        cons.gridy = 3;
        
        // Put the down button in the lower left side of
        // the cell.
        cons.anchor = GridBagConstraints.CENTER;
        add(getUpButton(), cons);
        
        // Add the down button.
        // Put the down button at (4,4) in the upper left side
        // of the cell.
        cons.anchor = GridBagConstraints.CENTER;
        cons.gridy = 4;
        add(getDownButton(), cons);
        
        // Add the add button.
        // Put the add button at (0,4)
        // Center the add and delete button.
        cons.anchor = GridBagConstraints.CENTER;
        cons.gridx = 0;
        cons.gridy = 5;
        
        add(getAddButton(), cons);
        
        // Add the delete button.
        // Put the delete button at (1,4)
        cons.gridx = 1;
        add(getDeleteButton(), cons);
	}
	
	/**
	 * Get the file list.
	 * @return The file list.
	 */
    public JList getList() {
        if (mList == null) {
        	mList = new JList();
        	mList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        	mList.setToolTipText(Messages.getString("DOMListPanel.0")); //$NON-NLS-1$
        	mList.setModel(getListModel());
            mListModel.addListDataListener(getButtonEnabler());
        	mList.addListSelectionListener(getButtonEnabler());
        }
        return mList;
    }
    
	/**
	 * Get the list model.
	 * @return The list model.
	 */
	public DOMListModel getListModel() {
		if (mListModel == null) {
			assert(mList != null);
			mListModel = new DOMListModel(mList, mContainerName, mElementName, mChildrenAreLeaves );
		}
		return mListModel;
	}
	
    /**
     * Initialize and get the list scroll pane.
     * 
     * @return The list scroll pane.
     */
    private JScrollPane getListScrollPane() {
        if (mListScrollPane == null) {
        	mListScrollPane = new JScrollPane();
            mListScrollPane.setPreferredSize(new Dimension(100,200));
        	mListScrollPane.setViewportView(getList());
        }
        return mListScrollPane;
    }
    
	/**
	 * Get the file list enabler.
	 * @return The list enabler.
	 */
	private ButtonSetEnabler getButtonEnabler(){
		if(mFileListEnabler == null){
			// Initialize the controller which will enable and disable the buttons.
			mFileListEnabler = new ButtonSetEnabler(getList(), getAddButton(),
					                                getDeleteButton(), getUpButton(),
					                                getDownButton());
		}
		return mFileListEnabler;
	}
	
	/**
	 * Return the UI frame to center dialogs on.
     * TODO: Can this be avoided?
	 * @return The panel.
	 */
	private JPanel getUIRoot(){
		return this;
	}
	
	/**
     * This method initializes the add button.
     * 
     * @return The add button.
     */
    private JButton getAddButton() {
        if (mAddButton == null) {
            mAddButton = new JButton();
            mAddButton.setEnabled(false);
            mAddButton.setText(Messages.getString("DOMListPanel.1")); //$NON-NLS-1$
            mAddButton.setToolTipText(Messages.getString("DOMListPanel.2")); //$NON-NLS-1$
            mAddButton.setPreferredSize(new Dimension(80,25));
            
            // Add the button enabler as an action listener which will update
            // the button states.
            mAddButton.addActionListener(getButtonEnabler());
            
            // Decide how new items will be added.
            if(mChildrenAreLeaves){
            	// Add an action listener which will prompt the user to select a
            	// file and to add it.
            	mAddButton.addActionListener(new AddFileButtonActionListener());
            }
            else {
            	// Add an action listener which will prompt the user to type a 
            	// new item and add it.
            	mAddButton.addActionListener(new AddItemButtonActionListener());
            }
        }
        return mAddButton;
    }

    /**
     * This method initializes the delete button.
     * 
     * @return The delete button.
     */
    private JButton getDeleteButton() {
        if (mDeleteButton == null) {
            mDeleteButton = new JButton();
            mDeleteButton.setEnabled(false);
            mDeleteButton.setToolTipText(Messages.getString("DOMListPanel.3")); //$NON-NLS-1$
            mDeleteButton.setText(Messages.getString("DOMListPanel.4")); //$NON-NLS-1$
            mDeleteButton.setPreferredSize(new Dimension(80,25));
            
            // Add the button enabler as an action listener which will update
            // the button states.
            mDeleteButton.addActionListener(getButtonEnabler());
            
            // Add an action listener which will perform the deletion
            // on the list.
            mDeleteButton.addActionListener(new ActionListener() {
            	public void actionPerformed(ActionEvent aEvent) {
                            getListModel().removeElement(getList().getSelectedValue());
                            getList().setSelectedIndex(getListModel().size() - 1);
                        }
                    });
        }
        return mDeleteButton;
    }
    /**
     * This method initializes the up button.
     * 
     * @return The up button.
     */
    private JButton getUpButton() {
        if (mUpButton == null) {
            mUpButton = new JButton();
            mUpButton.setEnabled(false);
            mUpButton.setText(Messages.getString("DOMListPanel.5")); //$NON-NLS-1$
            mUpButton.setToolTipText(Messages.getString("DOMListPanel.6")); //$NON-NLS-1$
            mUpButton.setPreferredSize(new Dimension(70,25));
            
            // Add the button enabler as an action listener which will update
            // the button states.
            mUpButton.addActionListener(getButtonEnabler());
            
            // Add an action listener which will move the item up in the list.
            mUpButton.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent e) {
                    int newPos = getListModel().moveElementBack(getList().getSelectedValue());
                    getList().setSelectedIndex(newPos);
                }
            });
        }
        return mUpButton;
    }
    
    /**
     * This method initializes the down button.
     * 
     * @return The down button.
     */
    private JButton getDownButton() {
        if (mDownButton == null) {
            mDownButton = new JButton();
            mDownButton.setText(Messages.getString("DOMListPanel.7")); //$NON-NLS-1$
            mDownButton.setToolTipText(Messages.getString("DOMListPanel.8")); //$NON-NLS-1$
            mDownButton.setEnabled(false);
            mDownButton.setPreferredSize(new Dimension(70,25));
            mDownButton.addActionListener(getButtonEnabler());
            mDownButton
                    .addActionListener( new ActionListener() {
                        public void actionPerformed(ActionEvent e) {
                            int newPos = getListModel().moveElementForward(getList().getSelectedValue());
                            getList().setSelectedIndex(newPos);
                        }
                    });
        }
        return mDownButton;
    }
    
	/**
	 * An internal class which implements an action listener which adds
	 * items to the list.
	 * @author Josh Lurz
	 *
	 */
	private final class AddItemButtonActionListener implements ActionListener {
		/**
		 * Method called when an action is performed.
		 * @param aEvent The event causing this action.
		 */
        public void actionPerformed(ActionEvent aEvent) {
            final String message = Messages.getString("DOMListPanel.9"); //$NON-NLS-1$
            String newItem = JOptionPane.showInputDialog(
                    getUIRoot(), message, Messages.getString("DOMListPanel.10"), //$NON-NLS-1$
                    JOptionPane.PLAIN_MESSAGE);
            // Check if the value cannot be added because it is
            // not unique and report an error to the user.
            // TODO: Consolidate duplicate code between this and the other
            // action listener
            if(getListModel().contains(newItem)) {
                String errorTitle = Messages.getString("DOMListPanel.11"); //$NON-NLS-1$
                String errorMessage = Messages.getString("DOMListPanel.12"); //$NON-NLS-1$
                // TODO: Find a better container for the error message.
                JOptionPane.showMessageDialog(null, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
            }
            else {
                getListModel().addElement(newItem);
            }
            
            getList().setSelectedIndex(getListModel().getSize() - 1);
        }
    }
    
	/**
	 * An internal class which implements an action listener which adds
	 * files to the list.
	 * @author Josh Lurz
	 *
	 */
	private final class AddFileButtonActionListener implements ActionListener {
		/**
		 * Method called when an action is performed.
		 * @param aEvent The event causing this action.
		 */
		public void actionPerformed(ActionEvent aEvent) {
			// Create a file chooser and add an XML filter.
		    JFileChooser chooser = new JFileChooser();
		    chooser.setFileFilter(new XMLFileFilter());
		    
		    // Show the file chooser.
		    int rv = chooser.showOpenDialog(getUIRoot());
		    if (rv == JFileChooser.APPROVE_OPTION) {
		    	// Get the list of selected files.
		    	File[] newFiles = Util.getSelectedFiles(chooser);
		    	for (int i = 0; i < newFiles.length; i++) {
		    		try {
		    			String newFile = newFiles[i].getCanonicalPath();
                        // Check if the value cannot be added because it is
                        // not unique and report an error to the user.
                        if(getListModel().contains(newFile)) {
                            String errorTitle = Messages.getString("DOMListPanel.13"); //$NON-NLS-1$
                            String errorMessage = Messages.getString("DOMListPanel.14"); //$NON-NLS-1$
                            // TODO: Find a better container for the error message.
                            JOptionPane.showMessageDialog(null, errorMessage, errorTitle, JOptionPane.ERROR_MESSAGE);
                        }
                        else {
                            getListModel().addElement(newFile);
                        }
		    		} catch(IOException e){
		    			e.printStackTrace();
		    		}
		        }
		        getList().setSelectedIndex(getListModel().size() - 1);
		    }
		}
	}
}
