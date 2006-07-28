package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import javax.swing.tree.TreePath;

import javax.swing.event.TreeModelEvent;

import java.util.List;

import ModelInterface.ModelGUI2.QueryTreeModel;

public class QueryAddRemoveUndoableEdit extends AbstractUndoableEdit {

	private QueryTreeModel qt;
	private TreePath path;
	private QueryTreeModel.QueryGroup parent;
	private int[] indices;
	private Object[] children;
	private boolean isInsert;

	public QueryAddRemoveUndoableEdit(TreeModelEvent e, boolean isInsert) {
		this.isInsert = isInsert;
		qt = (QueryTreeModel)e.getSource();
		path = e.getTreePath();
		parent = (QueryTreeModel.QueryGroup)path.getLastPathComponent();
		indices = e.getChildIndices();
		children = e.getChildren();
	}

	public boolean canUndo() {
		return true;
	}

	public boolean canRedo() {
		return true;
	}

	public String getPresentationName() {
		if(isInsert) {
			return "Create Query "+children[0].toString();
		} else {
			if(children.length == 1) {
				return "Remove Query "+children[0].toString();
			} else {
				return "Remove All Queries";
			}
		}
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			if(isInsert) {
				doRemove();
			} else {
				doAdd();
			}
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			if(isInsert) {
				doAdd();
			} else {
				doRemove();
			}
		} else {
			throw new CannotRedoException();
		}
	}

	private void doAdd() {
		List toAdd = parent.getQueryList();
		for(int i = 0; i < indices.length; ++i) {
			toAdd.add(indices[i], children[i]);
		}
		qt.fireTreeNodesInserted(this, path, indices, children);
	}

	private void doRemove() {
		List toRem = parent.getQueryList();
		for(int i = 0; i < indices.length; ++i) {
			toRem.remove(indices[i]);
		}
		qt.fireTreeNodesRemoved(this, path, indices, children);
	}
}
