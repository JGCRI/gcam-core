package guicomponents;

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

import utils.DOMUtils;

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
	public void mousePressed(MouseEvent aEvent) {
		// Select the pressed item in the tree.
		JTree sourceTree = ((JTree) aEvent.getSource());
		TreePath selectedPath = sourceTree.getClosestPathForLocation(aEvent
				.getX(), aEvent.getY());
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
	public void mouseReleased(MouseEvent aEvent) {
		if (aEvent.isPopupTrigger()) {
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
			JPopupMenu treeEditMenu = new JPopupMenu();

			// Check if there is a selected node and it is an
			// editible child node.
			// Don't display the edit value and name menu items if
			// there isn't.
			if (selectedNode != null
					&& sourceTree.getModel().isLeaf(selectedNode)) {
				// Add a menu item for editing the value.
				{ // Seperate the scopes for the two menu items.
					JMenuItem editValueItem = new JMenuItem("Edit Value...");
					editValueItem.setMnemonic(KeyEvent.VK_V);
					editValueItem.addActionListener(new ActionListener() {
						/**
						 * Method called when an the menu item is clicked which
						 * displays a dialog where the user can edit the value.
						 * 
						 * @param aLocalEvent
						 *            The action event received.
						 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
						 */
						public void actionPerformed(ActionEvent aLocalEvent) {
							// Show an option pane which allows
							// the value
							// to be modified.
							final String message = "Please enter a new value: ";
							final String title = "Change a Value";
							final String currentValue = selectedNode
									.getTextContent();
							String newValue = (String) JOptionPane
									.showInputDialog((Component) aLocalEvent
											.getSource(), message, title,
											JOptionPane.QUESTION_MESSAGE, null,
											null, currentValue);

							// If the new value is null the user pressed
							// cancel so
							// don't update the tree.
							if (newValue != null) {
								// Set the new value as the text content of
								// the node.
								selectedNode.setTextContent(newValue);
								sourceTree.getModel().valueForPathChanged(
										sourceTree.getSelectionPath(),
										selectedNode);
							}
						}

					});
					treeEditMenu.add(editValueItem);
				}
				{
					// Add a menu item for editing the name.
					JMenuItem editNameItem = new JMenuItem("Edit Name...");
					editNameItem.setMnemonic(KeyEvent.VK_N);
					editNameItem.addActionListener(new ActionListener() {
						/**
						 * Method called when an the menu item is clicked which
						 * displays a dialog where the user can edit the name.
						 * 
						 * @param aLocalEvent
						 *            The action event received.
						 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
						 */
						public void actionPerformed(ActionEvent aLocalEvent) {
							// Show an option pane which allows
							// the value
							// to be modified.
							final String message = "Please enter a new name: ";
							final String title = "Change a Name";
							final String currentValue = DOMUtils
									.getNameAttrValue(selectedNode);
							String newName = (String) JOptionPane
									.showInputDialog((Component) aLocalEvent
											.getSource(), message, title,
											JOptionPane.QUESTION_MESSAGE, null,
											null, currentValue);

							// If the new name is null the user pressed
							// cancel so
							// don't update the tree.
							if (newName != null) {
								// Set the new value as the text content of
								// the node.
								DOMUtils
										.setNameAttrValue(selectedNode, newName);
								sourceTree.getModel().valueForPathChanged(
										sourceTree.getSelectionPath(),
										selectedNode);
							}
						}

					});
					treeEditMenu.add(editNameItem);
				}
			}
			// Ensure that the selected node is not the root node
			// which cannot currently have items added to it.
			// TODO: What if the configuration document is missing
			// containers?
			else if (selectedNode != null
					&& !selectedNode.equals(selectedNode.getOwnerDocument()
							.getDocumentElement())) {
				// Add a menu item to add a new value to the tree.
				JMenuItem addMenuItem = new JMenuItem("Add New Value...");
				addMenuItem.setMnemonic(KeyEvent.VK_V); // BAD

				addMenuItem.addActionListener(new ActionListener() {
					/**
					 * Method called when an the menu item is clicked which
					 * displays a dialog where the user can create a new value.
					 * 
					 * @param aLocalEvent
					 *            The action event received.
					 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
					 */
					public void actionPerformed(ActionEvent aLocalEvent) {
						// Create a dialog which allows the user to input a
						// name
						// and a value for the new element.
						NewNodeDialog newValueDialog = new NewNodeDialog(
						// TODO: How do option panes manage this?
								// (Frame) aLocalEvent.getSource(),
								null, selectedNode.getOwnerDocument());
						newValueDialog.setVisible(true);
						Node newNode = (Node) newValueDialog.getSelectedValue();
						// Only add the new node if it is not null, which
						// would mean
						// the user clicked cancel.
						if (newNode != null) {
							// Check if the node already exists.
							boolean foundDuplicate = false;
							NodeList children = selectedNode.getChildNodes();
							for (int i = 0; i < children.getLength(); ++i) {
								if (DOMUtils.getNameAttrValue(newNode).equals(
										DOMUtils.getNameAttrValue(children
												.item(i)))) {
									foundDuplicate = true;
									// Display a message that this is a
									// duplicate value and offer to replace.
									final String message = "A value with this name already exists, would you like to replace it?";
									final String title = "Replace existing value?";
									int rv = JOptionPane
											.showConfirmDialog(
													(Component) aLocalEvent
															.getSource(),
													message,
													title,
													JOptionPane.YES_NO_OPTION);
									if (rv == JOptionPane.OK_OPTION) {
										// Replace the current node.
										selectedNode.replaceChild(newNode,
												children.item(i));
									} else {
										// Don't modify anything.
										return;
									}
								}
							}
							if (!foundDuplicate) {
								selectedNode.appendChild(newNode);
							}
							sourceTree.getModel()
									.valueForPathChanged(
											sourceTree.getSelectionPath(),
											newNode);
						}
					}
				});
				// Add the menu item to the popup menu.
				treeEditMenu.add(addMenuItem);
			}
			// There are no options currently available.
			else {
				JMenuItem noOptionsItem = new JMenuItem(
						"No Options Currently Available");
				noOptionsItem.setEnabled(false);
				treeEditMenu.add(noOptionsItem);
			}

			// Show the menu at the location of the click.
			treeEditMenu.show((Component) aEvent.getSource(), aEvent.getX(),
					aEvent.getY());
		}
	}
}