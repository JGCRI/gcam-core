package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import ModelInterface.ModelGUI2.tables.BaseTableModel;

public class FlipUndoableEdit extends AbstractUndoableEdit {

	private BaseTableModel bt;

	public FlipUndoableEdit(BaseTableModel bt) {
		this.bt = bt;
	}

	public boolean canUndo() {
		return true;
	}

	public boolean canRedo() {
		return true;
	}

	public String getPresentationName() {
		return "Flip Table";
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			bt.flip(-1,-1);
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			bt.flip(-1,-1);
		} else {
			throw new CannotRedoException();
		}
	}
}
