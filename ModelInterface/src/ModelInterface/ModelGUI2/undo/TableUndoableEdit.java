package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import java.util.Map;

import org.w3c.dom.Node;

import ModelInterface.ModelGUI2.tables.BaseTableModel;

public class TableUndoableEdit extends AbstractUndoableEdit {

	private BaseTableModel bt;
	private int row;
	private int col;
	private Node n;
	private String oldVal;
	private Map data;
	private String key;

	public TableUndoableEdit(BaseTableModel bt, int row, int col, Node n, String oldVal, Map data, String key) {
		this.bt = bt;
		this.row = row;
		this.col = col;
		this.n = n;
		this.oldVal = oldVal;
		this.data = data;
		// will be problem if the table way flipped
		this.key = key;
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
		return "Edit Cell Value";
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			if(oldVal != null) {
				String tmp = n.getNodeValue();
				n.setNodeValue(oldVal);
				oldVal = tmp;
			} else {
				data.put(key, null);
			}
			bt.fireTableCellUpdated(row, col);
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			if(oldVal != null) {
				String tmp = n.getNodeValue();
				n.setNodeValue(oldVal);
				oldVal = tmp;
			} else {
				data.put(key, n);
			}
			bt.fireTableCellUpdated(row, col);
		} else {
			throw new CannotRedoException();
		}
	}
}
