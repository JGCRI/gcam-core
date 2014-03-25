/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
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

import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import javax.swing.tree.TreePath;

import javax.swing.event.TreeModelEvent;

import java.util.List;

import ModelInterface.ModelGUI2.QueryTreeModel;

public class QueryAddRemoveUndoableEdit extends MiAbstractUndoableEdit {

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
		addListener(qt);
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
			fireUndoPerformed(this, this);
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
			fireRedoPerformed(this, this);
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
