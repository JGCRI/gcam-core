/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
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
	private Map<String, Node> data;
	private String key;

	public TableUndoableEdit(BaseTableModel bt, int row, int col, Node n, String oldVal, Map<String, Node> data, String key) {
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
