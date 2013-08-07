package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import java.util.Vector;

import ModelInterface.ModelGUI2.tables.BaseTableModel;

public class FilterUndoableEdit extends AbstractUndoableEdit {

	private BaseTableModel bt;
	private Vector oldActiveRows;
	private Vector newActiveRows;

	public FilterUndoableEdit(BaseTableModel bt, Vector oldActiveRows, Vector newActiveRows) {
		this.bt = bt;
		this.oldActiveRows = oldActiveRows;
		this.newActiveRows = newActiveRows;
	}

	public boolean canUndo() {
		// need to make sure this model still has control
		return true;
	}

	public boolean canRedo() {
		// need to make sure this model still has control
		return true;
	}

	public String getPresentationName() {
		return "Filter Table";
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			bt.setActiveRows(oldActiveRows);
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			bt.setActiveRows(newActiveRows);
		} else {
			throw new CannotRedoException();
		}
	}
}
