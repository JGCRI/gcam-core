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

import ModelInterface.ModelGUI2.ScenarioListItem;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.xmldb.XMLDB;

import org.basex.query.QueryProcessor;
import org.basex.query.QueryException;
import org.basex.query.iter.Iter;
import org.basex.query.value.node.ANode;

public class RenameScenarioUndoableEdit extends MiAbstractUndoableEdit {

	private DbViewer viewer;
	private ScenarioListItem oldName;
	private String newName;

	public RenameScenarioUndoableEdit (DbViewer viewer, ScenarioListItem oldName, String newName) {
		this.viewer = viewer;
		this.oldName = oldName;
		this.newName = newName;
		doRename(oldName.getScnName(), newName);
	}

	public boolean canUndo() {
		return true;
	}

	public boolean canRedo() {
		return true;
	}

	public String getUndoPresentationName() {
		return super.getUndoPresentationName()+" Rename Scenario "+newName;
	}
	
	public String getRedoPresentationName() {
		return super.getRedoPresentationName()+" Rename Scenario "+oldName.getScnName();
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			doRename(newName, oldName.getScnName());
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			doRename(oldName.getScnName(), newName);
		} else {
			throw new CannotRedoException();
		}
	}

	/**
	 * Renames a scenario with the same date as in oldName and the
	 * name of fromName then tells the DbViewer to refersh it's
	 * scenario list.  First queries the db to get the correct
	 * scenario context node then uses the xmldb's setValue to do 
	 * the work.
	 * @param fromName The scenario name that is going to be replaced.
	 * @param toName The scenario will be named after this method.
	 */
	private void doRename(String fromName, String toName) {
        QueryProcessor queryProc = XMLDB.getInstance().createQuery(
                "/scenario[@date='"+oldName.getScnDate()+"' and @name='"+fromName+"']/@name", null, null, null);
		try { 
            Iter res = queryProc.iter();
			// if it has no results that is bad.. I'll just go with the null pointer exception
			ANode context = (ANode)res.next();
			XMLDB.getInstance().setValue(context, toName);
			viewer.resetScenarioList();
		} catch(QueryException e) {
			// TODO: put an error up on the screen
			e.printStackTrace();
		} finally {
            queryProc.close();
		}
	}
}
