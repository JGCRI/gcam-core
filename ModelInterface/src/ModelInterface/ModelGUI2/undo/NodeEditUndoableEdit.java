package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import javax.swing.tree.TreePath;

import org.w3c.dom.Node;

import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.DOMmodel.DOMNodeAdapter;

public class NodeEditUndoableEdit extends AbstractUndoableEdit {

	private DOMmodel model;
	private TreePath path;
	private String oldNodeStr;
	private String newNodeStr;

	public NodeEditUndoableEdit(DOMmodel model, TreePath path, String oldNodeStr, String newNodeStr) {
		this.model = model;
		this.path = path;
		this.oldNodeStr = oldNodeStr;
		this.newNodeStr = newNodeStr;
	}

	public boolean canUndo() {
		return true;
	}

	public boolean canRedo() {
		return true;
	}

	public String getPresentationName() {
		String name;
		DOMNodeAdapter node = (DOMNodeAdapter)path.getLastPathComponent();
		if(node.getNode().getNodeType() == Node.TEXT_NODE) {
			name = "Text";
		} else {
			name= node.getNode().getNodeName();
		}
		return "Edit "+name;
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			model.valueForPathChanged(path, oldNodeStr, false);
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			model.valueForPathChanged(path, newNodeStr, false);
		} else {
			throw new CannotRedoException();
		}
	}
}
