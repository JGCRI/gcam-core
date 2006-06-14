package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import org.w3c.dom.Node;
import org.w3c.dom.DOMException;

public class NodeInsertUndoableEdit extends AbstractUndoableEdit {
	Node parent;
	Node child;

	public NodeInsertUndoableEdit(Node parent, Node child) {
		this.parent = parent;
		this.child = child;
	}

	public boolean canUndo() {
		// is there a situation where we couldn't?
		return true;
	}

	public boolean canRedo() {
		// is there a situation where we couldn't?
		return true;
	}

	public String getPresentationName() {
		return "Insert "+child.getNodeName();
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			try {
				child.setUserData("isFromUndoable", true, null);
				parent.removeChild(child);
			} catch(DOMException de) {
				child.setUserData("isFromUndoable", null, null);
				CannotUndoException e = new CannotUndoException();
				e.initCause(de);
				throw e;
			}
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			try {
				// index position ??
				child.setUserData("isFromUndoable", true, null);
				parent.appendChild(child);
			} catch(DOMException de) {
				child.setUserData("isFromUndoable", null, null);
				CannotRedoException e = new CannotRedoException();
				e.initCause(de);
				throw e;
			}
		} else {
			throw new CannotRedoException();
		}
	}
}
