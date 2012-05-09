/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ConfigurationEditor.configurationeditor;

import ModelInterface.ConfigurationEditor.guicomponents.DOMTreeModel;
import ModelInterface.ConfigurationEditor.guihelpers.PopupMenuCreatorMouseAdapter;

import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.border.BevelBorder;
import javax.swing.tree.TreeSelectionModel;

import org.w3c.dom.Document;

/**
 * The advanced options panel.
 * 
 * @author Josh Lurz
 */
public class AdvancedOptionsPanel extends AbstractEditorPanel {

    /**
     * Unique identifier for serializing.
     */
    private static final long serialVersionUID = -4745218032870665176L;

    /**
     * Constructor
     */
    public AdvancedOptionsPanel() {
        super();
        initialize();
    }

    /**
     * This method initializes the advanced preferences panel.
     */
    private void initialize() {
        setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));

        // Add a property change listener which will swap in a new
        // tree when the document is changed.
        addPropertyChangeListener("document-replaced",
                new PropertyChangeListener() {
                    public void propertyChange(PropertyChangeEvent aEvent) {
                        // Create a new tree using a DOM based model.
                        createAdvancedTree((Document) aEvent.getNewValue());
                    }
                });
    }

    /**
     * Create and display the advanced editing tree.
     * 
     * @param aNewDocument
     *            The new document.
     */
    private void createAdvancedTree(final Document aNewDocument) {
        // Remove the existing content from the panel if there is one.
        final Component[] panelComponents = getComponents();
        for (int i = 0; i < panelComponents.length; ++i) {
            if (panelComponents[i].getName().equals("content")) {
                remove(i);
            }
        }

        if (aNewDocument != null) {
            final JTree advancedTree = new JTree(new DOMTreeModel(aNewDocument
                    .getDocumentElement(), ConfigurationEditor.ELEMENT_NAME));
            // TODO: Are all of these neccessary?
            advancedTree.setVisible(true);
            advancedTree.setShowsRootHandles(true);
            advancedTree.setRootVisible(true);
            advancedTree.getSelectionModel().setSelectionMode(
                    TreeSelectionModel.SINGLE_TREE_SELECTION);

            // Add a mouse listener so that a menu can be
            // displayed on right click.
            advancedTree.addMouseListener(new PopupMenuCreatorMouseAdapter());
            // Create a scroll pane to display the tree.
            final JScrollPane treeScroller = new JScrollPane(advancedTree);
            treeScroller.setPreferredSize(new Dimension(600, 250));
            treeScroller.setName("content");
            add(treeScroller);
        }
        else {
            // Add an item in place of the tree scroller so the user
            // can tell that there would be content here if the document
            // were loaded.
            final String errorString = "A configuration must be opened before the advanced editing options can be used.";
            JLabel label = new JLabel(errorString);
            
            // The label will be removed when a document is added.
            label.setName("content");
            add(label);
        }
    }

}
