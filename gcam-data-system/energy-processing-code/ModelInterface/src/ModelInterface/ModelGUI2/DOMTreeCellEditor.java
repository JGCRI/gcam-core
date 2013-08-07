package ModelInterface.ModelGUI2;

import javax.swing.tree.TreeCellEditor;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.JTree;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.ActionEvent;

import java.util.ArrayList;
import java.util.EventObject;
import java.util.Iterator;

import org.w3c.dom.Node;

import ModelInterface.ModelGUI2.DOMmodel.DOMNodeAdapter;

/**
 * Provides an editor for DOM elements in a JTree, and provides validiation of
 * user input.
 * @author Pralit Patel
 * @see ModelInterface.ModelGUI2.DOMmodel.DOMNodeAdapter;
 */
public class DOMTreeCellEditor implements TreeCellEditor {
	/**
	 * Stores registered CellEditorListeners
	 */
	ArrayList<CellEditorListener> listeners;
	/**
	 * Used to do the editing of the nodes.
	 */
	JTextField tf;
	/**
	 * The current node being editing
	 */
	Node initEditVal;
	/**
	 * Default constructor.  Initilaizes the text field editor.
	 */
	public DOMTreeCellEditor() {
		tf = new JTextField(40);
		// setup the text field so that when enter is pressed
		// it will try to "stopEditing()"
		tf.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "check");
		tf.getActionMap().put("check", new AbstractAction() {
			public void actionPerformed(ActionEvent e) {
				tf.postActionEvent();
				stopCellEditing();
			}
		});
		listeners = new ArrayList<CellEditorListener>();
	}
	/**
	 * Determines if a cell is editable.  Uses the current selection from the source
	 * of the event. If it is a Text Node or Element it is editable.
	 * @param event The event which needs to know it the cell is editable.
	 * @return True if it is editable, false otherwise.
	 */
	public boolean isCellEditable(EventObject event) {
		Object temp = ((JTree)event.getSource()).getLastSelectedPathComponent();
		if(temp == null) {
			return false;
		}
		Node tempNode = ((DOMNodeAdapter)temp).getNode();
		return tempNode.getNodeType() == Node.TEXT_NODE || tempNode.getNodeType() == Node.ELEMENT_NODE;
	}
	/**
	 * Gets the current value from the editor.  The current value may or may not be valid.
	 * @return Current string in the text field editor.
	 */
	public Object getCellEditorValue() {
		return tf.getText();
	}
	/**
	 * Determine if the cell should be selected to edit.
	 * @param event The event that needs to know.
	 * @return true.
	 */
	public boolean shouldSelectCell(EventObject event) {
		return true;
	}
	/**
	 * The user has canceled editing.  Will notify listeners, and reset the editor.
	 */
	public void cancelCellEditing() {
		fireEditingCanceled();
		resetEditor();
	}
	/**
	 * Register a CellEditorListener to be notified of ChangeEvents.
	 * @param l new listener to register.
	 */
	public void addCellEditorListener(CellEditorListener l) {
		listeners.add(l);
	}
	/**
	 * Deregister a CellEditorListener from being notified of ChangeEvents.
	 * @param l the listener to deregister.
	 */
	public void removeCellEditorListener(CellEditorListener l) {
		listeners.remove(l);
	}
	/**
	 * Initializes the editor and returns it.
	 * @param tree JTree from which value comes from.
	 * @param value The value to be editied.
	 * @param isSelected whether the value is selected in the tree.
	 * @param expanded whether the value is expanded in the tree.
	 * @param leaf whether value is a leaf.
	 * @param row the row of the value in the tree.
	 * @return The initialized text field editor.
	 */
	public Component getTreeCellEditorComponent(JTree tree, Object value, boolean isSelected, boolean expanded,
			boolean leaf, int row) {
		// should I check to make sure value it valid?
		initEditVal = ((DOMNodeAdapter)value).getNode();
		tf.setText(((DOMNodeAdapter)value).toEditString());
		return tf;
	}
	/**
	 * Called when the user is done editing and checks the validity of the user's
	 * input.  If the input is invalid the user will be presented with an error
	 * message and asked if they want to continue editing or cancel.  If the continue
	 * false is returned, otherwise it will cancel their edit and return true.  If 
	 * true will be returned the listeners will be notified of the proper event, then the 
	 * editor will be reset.  For an edit to be valid for a text node it must not contain
	 * illegal XML characters.  For an edit to be valid for an element it also cannot have
	 * illegal XML characters, and must be in the format: NodeName[, AttrName="AttrValue"]*.
	 * @return True if the editor is done editing, false otherwise.
	 */
	public boolean stopCellEditing() {
		String currVal = (String)getCellEditorValue();
		// TODO: check for invalid xml chars
		if(initEditVal.getNodeType() == Node.TEXT_NODE) {
			// don't need to do anything
		} else if (initEditVal.getNodeType() == Node.ELEMENT_NODE) {
			if(!currVal.matches("^\\s*[\\w\\-_]+\\s*(?:,\\s*[\\w\\-_]+\\s*=\\s*\"[^\"]+\"\\s*)*\\s*$")) {
				Object[] options = {"Edit", "Cancel"};
				int ans = JOptionPane.showOptionDialog(SwingUtilities.getWindowAncestor(tf), 
						"Node names and attributes must be specified in the format:\nNodeName[, AttrName=\"AttrValue\"]..", 
						"Invalid Format", JOptionPane.YES_NO_OPTION, JOptionPane.ERROR_MESSAGE, null, options, options[1]);
				if(ans == 1) {
					cancelCellEditing();
					return true;
				}
				return false;
			}
		} else {
			// don't know what to do here
		}
		fireEditingStopped();
		resetEditor();
		return true;
	}
	/**
	 * Resets the editor value and sets initEditVal back to null.
	 */
	private void resetEditor() {
		tf.setText("");
		initEditVal = null;
	}
	/**
	 * Notify listeners that the edit has been canceled.
	 */
	private void fireEditingCanceled() {
		ChangeEvent evt = new ChangeEvent(this);
		for(Iterator<CellEditorListener> it = listeners.iterator(); it.hasNext(); ) {
			it.next().editingCanceled(evt);
		}
	}
	/**
	 * Notify listeners that the user is done editing.
	 */
	private void fireEditingStopped() {
		ChangeEvent evt = new ChangeEvent(this);
		for(Iterator<CellEditorListener> it = listeners.iterator(); it.hasNext(); ) {
			it.next().editingStopped(evt);
		}
	}
}
