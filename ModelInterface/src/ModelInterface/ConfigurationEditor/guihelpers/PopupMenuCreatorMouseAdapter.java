package ModelInterface.ConfigurationEditor.guihelpers;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.tree.TreePath;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import ModelInterface.ConfigurationEditor.utils.DOMUtils;

/**
 * A class which displays a menu of items to modify a DOM tree. TODO: Rename and
 * document better.
 * 
 * @author Josh Lurz
 * 
 */
public final class PopupMenuCreatorMouseAdapter extends MouseAdapter {
	/**
	 * Method called when the mouse is pressed which selects the current item in
	 * the tree.
	 * 
	 * @param aEvent
	 *            The mouse event.
	 * @see java.awt.event.MouseListener#mousePressed(java.awt.event.MouseEvent)
	 */
	@Override
	public void mousePressed(final MouseEvent aEvent) {
		// Select the pressed item in the tree.
		final JTree sourceTree = ((JTree) aEvent.getSource());
		final TreePath selectedPath = sourceTree.getClosestPathForLocation(
				aEvent.getX(), aEvent.getY());
		sourceTree.setSelectionPath(selectedPath);
	}

	/**
	 * Method called when the mouse is released which displays a pop-up menu on
	 * right click.
	 * 
	 * @param aEvent
	 *            The moust event.
	 * @see java.awt.event.MouseListener#mouseReleased(java.awt.event.MouseEvent)
	 */
	@Override
	public void mouseReleased(final MouseEvent aEvent) {
		if (aEvent.isPopupTrigger()) {
			createPopupMenu(aEvent);
		}
	}

	/**
	 * Create a popup menu to edit the tree.
	 * 
	 * @param aEvent
	 *            The mouse event.
	 */
	private void createPopupMenu(final MouseEvent aEvent) {
		// Get the source JTree of the event.
		final JTree sourceTree = ((JTree) aEvent.getSource());

		// Get the selected node. This should have been selected
		// by the mouse press before the release.
		final Node selectedNode = (Node) sourceTree.getSelectionPath()
				.getLastPathComponent();

		// Create a new menu with options for editing the node.
		// TODO: Accessability for this should be fixed by
		// adding a
		// secondary access to the menu and access keys.
		final JPopupMenu treeEditMenu = new JPopupMenu();

		// Ensure that the selected node is not the root node
		// which cannot currently have items added to it.
		// TODO: What if the configuration document is missing
		// containers?
		if (selectedNode == null
				|| selectedNode.equals(selectedNode.getOwnerDocument()
						.getDocumentElement())) {
			// There are no options currently available.
			final JMenuItem noOptionsItem = new JMenuItem(
					"No Options Currently Available");
			noOptionsItem.setEnabled(false);
			treeEditMenu.add(noOptionsItem);
		}
		// Check if the selected node is an
		// editible child node.
		// Don't display the edit value and name menu items if
		// there isn't.
		else if (sourceTree.getModel().isLeaf(selectedNode)) {
			// Add a menu item for editing the value.
			{ // Seperate the scopes for the two menu items.
				final JMenuItem editValueItem = new JMenuItem("Edit Value...");
				editValueItem.setMnemonic(KeyEvent.VK_V);
				editValueItem.addActionListener(new EditValueClicked(
						selectedNode, sourceTree));
				treeEditMenu.add(editValueItem);
			}
			{
				// Add a menu item for editing the name.
				final JMenuItem editNameItem = new JMenuItem("Edit Name...");
				editNameItem.setMnemonic(KeyEvent.VK_N);
				editNameItem.addActionListener(new EditNameClicked(
						selectedNode, sourceTree));
				treeEditMenu.add(editNameItem);
			}
			{
				// Add a menu item for deleting an item.
				final JMenuItem deleteItem = new JMenuItem("Delete Item");
				deleteItem.setMnemonic(KeyEvent.VK_D);
				deleteItem.addActionListener(new DeleteItemClicked(
						selectedNode, sourceTree));
				treeEditMenu.add(deleteItem);
			}
		}

		else {
			// Add a menu item to add a new value to the tree.
			final JMenuItem addMenuItem = new JMenuItem("Add New Value...");
			addMenuItem.setMnemonic(KeyEvent.VK_V); // BAD

			addMenuItem.addActionListener(new AddItemClicked(selectedNode,
					sourceTree));
			// Add the menu item to the popup menu.
			treeEditMenu.add(addMenuItem);
		}

		// Show the menu at the location of the click.
		treeEditMenu.show((Component) aEvent.getSource(), aEvent.getX(), aEvent
				.getY());
	}

	/**
	 * Action listeners which is activated when the user selects the Add item
	 * menu button. Activates the interface elements to add an item and adds the
	 * item to the tree.
	 * 
	 * @author Josh Lurz
	 */
	private final class AddItemClicked implements ActionListener {
		/**
		 * The tree which the action listener will modify.
		 */
		private transient final JTree mTree;

		/**
		 * The node selected in the tree.
		 */
		private transient final Node mNode;

		/**
		 * Constructor
		 * 
		 * @param aNode
		 *            The node selected in the tree.
		 * @param aTree
		 *            The tree which the action listener will modify.
		 * 
		 */
		public AddItemClicked(Node aNode, JTree aTree) {
			super();
			mNode = aNode;
			mTree = aTree;
		}

		/**
		 * Method called when an the menu item is clicked which displays a
		 * dialog where the user can create a new value.
		 * 
		 * @param aLocalEvent
		 *            The action event received.
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(final ActionEvent aLocalEvent) {
			// Create a dialog which allows the user to input a
			// name
			// and a value for the new element.
			final NewNodeDialog newValueDialog = new NewNodeDialog(
			// TODO: How do option panes manage this?
					// (Frame) aLocalEvent.getSource(),
					null, mNode.getOwnerDocument());
			newValueDialog.setVisible(true);
			final Node newNode = (Node) newValueDialog.getSelectedValue();
			// Only add the new node if it is not null, which
			// would mean
			// the user clicked cancel.
			if (newNode != null) {
				// Check if the node already exists.
				boolean foundDuplicate = false;
				final NodeList children = mNode.getChildNodes();
				for (int i = 0; i < children.getLength(); ++i) {
					if (DOMUtils.getNameAttrValue(newNode).equals(
							DOMUtils.getNameAttrValue(children.item(i)))) {
						foundDuplicate = true;
						// Display a message that this is a
						// duplicate value and offer to replace.
						final String message = "A value with this name already exists, would you like to replace it?";
						final String title = "Replace existing value?";
						final int returnValue = JOptionPane.showConfirmDialog(
								(Component) aLocalEvent.getSource(), message,
								title, JOptionPane.YES_NO_OPTION);
						if (returnValue == JOptionPane.YES_OPTION) {
							// Replace the current node.
							mNode.replaceChild(newNode, children.item(i));
						} else {
							// Don't modify anything.
							return;
						}
					}
				}
				if (!foundDuplicate) {
					mNode.appendChild(newNode);
				}
				mTree.getModel().valueForPathChanged(mTree.getSelectionPath(),
						newNode);
			}
		}
	}

	/**
	 * Action listeners which is activated when the user selects the Edit name
	 * menu button. Activates the interface elements to edit a name and update
	 * the name in the tree.
	 * 
	 * @author Josh Lurz
	 */
	private final class EditNameClicked implements ActionListener {
		/**
		 * The tree which the action listener will modify.
		 */
		private transient final Node mNode;

		/**
		 * The node selected in the tree.
		 */
		private transient final JTree mTree;

		/**
		 * Constructor
		 * 
		 * @param aTree
		 *            The tree which the action listener will modify.
		 * @param aNode
		 *            The node selected in the tree.
		 */
		public EditNameClicked(Node aNode, JTree aTree) {
			super();
			mNode = aNode;
			mTree = aTree;
		}

		/**
		 * Method called when an the menu item is clicked which displays a
		 * dialog where the user can edit the name.
		 * 
		 * @param aLocalEvent
		 *            The action event received.
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(final ActionEvent aLocalEvent) {
			// Show an option pane which allows
			// the value
			// to be modified.
			final String message = "Please enter a new name: ";
			final String title = "Change a Name";
			final String currentValue = DOMUtils.getNameAttrValue(mNode);
			final String newName = (String) JOptionPane.showInputDialog(
					(Component) aLocalEvent.getSource(), message, title,
					JOptionPane.QUESTION_MESSAGE, null, null, currentValue);

			// If the new name is null the user pressed
			// cancel so
			// don't update the tree.
			if (newName != null) {
				// Set the new value as the text content of
				// the node.
				DOMUtils.setNameAttrValue(mNode, newName);
				mTree.getModel().valueForPathChanged(mTree.getSelectionPath(),
						mNode);
			}
		}
	}

	/**
	 * Action listeners which is activated when the user selects the Edit value
	 * menu button. Activates the interface elements to edit a value and update
	 * the tree.
	 * 
	 * @author Josh Lurz
	 */
	private final class EditValueClicked implements ActionListener {
		/**
		 * The tree which the action listener will modify.
		 */
		private transient final JTree mTree;

		/**
		 * The node selected in the tree.
		 */
		private transient final Node mNode;

		/**
		 * Constructor
		 * 
		 * @param aNode
		 *            The node selected in the tree.
		 * @param aTree
		 *            The tree which the action listener will modify.
		 */
		public EditValueClicked(Node aNode, JTree aTree) {
			super();
			mNode = aNode;
			mTree = aTree;
		}

		/**
		 * Method called when an the menu item is clicked which displays a
		 * dialog where the user can edit the value.
		 * 
		 * @param aLocalEvent
		 *            The action event received.
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(final ActionEvent aLocalEvent) {
			// Show an option pane which allows
			// the value
			// to be modified.
			final String message = "Please enter a new value: ";
			final String title = "Change a Value";
			final String currentValue = mNode.getTextContent();
			final String newValue = (String) JOptionPane.showInputDialog(
					(Component) aLocalEvent.getSource(), message, title,
					JOptionPane.QUESTION_MESSAGE, null, null, currentValue);

			// If the new value is null the user pressed
			// cancel so
			// don't update the tree.
			if (newValue != null) {
				// Set the new value as the text content of
				// the node.
				mNode.setTextContent(newValue);
				mTree.getModel().valueForPathChanged(mTree.getSelectionPath(),
						mNode);
			}
		}

	}

	/**
	 * Action listeners which is activated when the user selects the delete item
	 * menu button. Activates the interface elements to delete an item and
	 * deletes the item from the tree.
	 * 
	 * @author Josh Lurz
	 */
	private final class DeleteItemClicked implements ActionListener {
		/**
		 * The tree which the action listener will modify.
		 */
		private transient final JTree mTree;

		/**
		 * The node selected in the tree.
		 */
		private transient final Node mNode;

		/**
		 * Constructor
		 * 
		 * @param aNode
		 *            The node selected in the tree.
		 * @param aTree
		 *            The tree which the action listener will modify.
		 */
		public DeleteItemClicked(Node aNode, JTree aTree) {
			super();
			mNode = aNode;
			mTree = aTree;
		}

		/**
		 * Method called when an the menu item is clicked which displays a
		 * dialog where the user can create a new value.
		 * 
		 * @param aLocalEvent
		 *            The action event received.
		 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
		 */
		public void actionPerformed(final ActionEvent aLocalEvent) {
			final Node parentNode = (Node)mTree.getSelectionPath().getParentPath().getLastPathComponent();

			// Delete the item.
			assert (parentNode != null);

			// Due to limitations in the DOM implementation removeChild, equals and
			// isEqualNode do not work on the node wrapper. Search and find the
			// equivalent node and remove it.
			final int childIndex = mTree.getModel().getIndexOfChild(parentNode, mNode);
			parentNode.removeChild(parentNode.getChildNodes().item(childIndex));
			mTree.getModel().valueForPathChanged(
					mTree.getSelectionPath().getParentPath(), parentNode);
		}
	}
}
