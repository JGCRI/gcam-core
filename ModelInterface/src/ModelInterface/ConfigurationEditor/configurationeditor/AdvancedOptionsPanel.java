/**
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
        // Remove the existing tree from the panel if there is one.
        final Component[] panelComponents = getComponents();
        for (int i = 0; i < panelComponents.length; ++i) {
            if (panelComponents[i].getName().equals("tree-scroller")) {
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
            treeScroller.setName("tree-scroller");
            add(treeScroller);
        }
    }

}
