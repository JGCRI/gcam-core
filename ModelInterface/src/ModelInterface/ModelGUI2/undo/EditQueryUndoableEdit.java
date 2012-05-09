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

import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.common.DataPair;

public class EditQueryUndoableEdit extends MiAbstractUndoableEdit {

	private QueryGenerator qg;
	private boolean hasSetOldValues;
	private boolean hasSetNewValues;
	private boolean hasRealChanges;

	private String oldTitle;
	private String oldAxis1Name;
	private DataPair<String, String> oldNodeLevel;
	private String oldAxis2Name;
	private DataPair<String, String> oldYearLevel;
	private String oldVar;
	private String oldLabelColumnName;
	private String oldXPath;
	private boolean oldSumAll;
	private boolean oldGroup;
	private boolean oldBuildSingleList;
	private String oldComments;

	private String newTitle;
	private String newAxis1Name;
	private DataPair<String, String> newNodeLevel;
	private String newAxis2Name;
	private DataPair<String, String> newYearLevel;
	private String newVar;
	private String newLabelColumnName;
	private String newXPath;
	private boolean newSumAll;
	private boolean newGroup;
	private boolean newBuildSingleList;
	private String newComments;

	public EditQueryUndoableEdit(QueryGenerator qgIn, MiUndoableEditListener listener) {
		qg = qgIn;
		hasRealChanges = hasSetOldValues = hasSetNewValues = false;
		addListener(listener);
		if(qg.hasSingleQueryExtension() && qg.getSingleQueryExtension() != null) {
			addListener(qg.getSingleQueryExtension());
		}
	}

	public void setOldValues(QueryGenerator qgIn) {
		oldTitle = qgIn.toString();
		oldAxis1Name = qgIn.getAxis1Name();
		oldAxis2Name = qgIn.getAxis2Name();
		oldNodeLevel = qgIn.getNodeLevelPair();
		oldYearLevel = qgIn.getYearLevelPair();
		oldVar = qgIn.getVariable();
		oldLabelColumnName = qgIn.getChartLabelColumnName();
		oldXPath = qgIn.getXPath();
		oldSumAll = qgIn.isSumAll();
		oldGroup = qgIn.isGroup();
		oldBuildSingleList = qgIn.isBuildList();
		oldComments = qgIn.getRealComments();
		hasSetOldValues = true;
	}

	public void setNewValues(QueryGenerator qgIn) {
		boolean temp;
		temp = !doDiffCheck(oldTitle, newTitle = qgIn.toString());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldAxis1Name, newAxis1Name = qgIn.getAxis1Name());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldAxis2Name, newAxis2Name = qgIn.getAxis2Name());
		hasRealChanges = hasRealChanges || temp;

		// the equals for DataPair isn't implemented so this is probably better..
		temp = !doDiffCheck(oldNodeLevel.getKey(), (newNodeLevel = qgIn.getNodeLevelPair()).getKey());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldNodeLevel.getValue(), newNodeLevel.getValue());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldYearLevel.getKey(), (newYearLevel = qgIn.getYearLevelPair()).getKey());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldYearLevel.getValue(), newYearLevel.getValue());
		hasRealChanges = hasRealChanges || temp;

		temp = !doDiffCheck(oldVar, newVar = qgIn.getVariable());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldLabelColumnName,
			newLabelColumnName = qgIn.getChartLabelColumnName());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldXPath, newXPath = qgIn.getXPath());
		hasRealChanges = hasRealChanges || temp;
		temp = oldSumAll != (newSumAll = qgIn.isSumAll());
		hasRealChanges = hasRealChanges || temp;
		temp = oldGroup != (newGroup = qgIn.isGroup());
		hasRealChanges = hasRealChanges || temp;
		temp = oldBuildSingleList != (newBuildSingleList = qgIn.isBuildList());
		hasRealChanges = hasRealChanges || temp;
		temp = !doDiffCheck(oldComments, newComments = qgIn.getRealComments());
		hasRealChanges = hasRealChanges || temp;
		hasSetNewValues = true;
	}

	/**
	 * Check an an old value and a new value to see
	 * if they are different.  Uses oldVal.equals(newVal) 
	 * and also checks nulls. It also counts a newVal of
	 * "" the same as null.
	 * @param oldVal The old value to check with.
	 * @param newVal The new value to check with.
	 * @return True if they are the same, false otherwise.
	 */ 
	private boolean doDiffCheck(Object oldVal, Object newVal) {
		return ((oldVal == null) && (newVal == null || newVal.equals(""))) ||
				newVal.equals(oldVal);
	}

	public boolean hasRealChanges() {
		if(!hasSetOldValues || !hasSetNewValues) {
			// error?
			return false;
		}
		return hasRealChanges;
	}

	/**
	 * Return true if the xpath changed, false otherwise.
	 * @return True if the xpath chagned, else false.
	 */
	public boolean didXPathChange() {
		return !doDiffCheck(oldXPath, newXPath);
	}

	/**
	 * Return true if the node level chagned, false otherwise.
	 * @return True if the node level chagned, else false.
	 */
	public boolean didNodeLevelChange() {
		return !doDiffCheck(oldNodeLevel.getKey(), newNodeLevel.getKey()) || 
				!doDiffCheck(oldNodeLevel.getValue(), newNodeLevel.getValue());
	}

	public boolean canUndo() {
		return hasSetOldValues;
	}

	public boolean canRedo() {
		return hasSetNewValues;
	}

	public String getPresentationName() {
		return "Edit Query "+qg.toString();
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
			// if the xpath or node level changed we will have
			// to trash the old collapseOnList
			if(didNodeLevelChange() || didXPathChange()) {
				qg.resetCollapseOnList();
			}
			qg.setTitle(oldTitle);
			qg.setAxis1Name(oldAxis1Name);
			qg.setAxis2Name(oldAxis2Name);
			qg.setNodeLevel(oldNodeLevel);
			qg.setYearLevel(oldYearLevel);
			qg.setVariable(oldVar);
			qg.setCharLabelColumnName(oldLabelColumnName);
			qg.setXPath(oldXPath);
			qg.setSumAll(oldSumAll);
			qg.setGroup(oldGroup);
			qg.setBuildList(oldBuildSingleList);
			qg.setComments(oldComments);
			fireUndoPerformed(this, this);
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
			// if the xpath or node level changed we will have
			// to trash the old collapseOnList
			if(didNodeLevelChange() || didXPathChange()) {
				qg.resetCollapseOnList();
			}
			qg.setTitle(newTitle);
			qg.setAxis1Name(newAxis1Name);
			qg.setAxis2Name(newAxis2Name);
			qg.setNodeLevel(newNodeLevel);
			qg.setYearLevel(newYearLevel);
			qg.setVariable(newVar);
			qg.setCharLabelColumnName(newLabelColumnName);
			qg.setXPath(newXPath);
			qg.setSumAll(newSumAll);
			qg.setGroup(newGroup);
			qg.setBuildList(newBuildSingleList);
			qg.setComments(newComments);
			fireRedoPerformed(this, this);
		} else {
			throw new CannotRedoException();
		}
	}
}
