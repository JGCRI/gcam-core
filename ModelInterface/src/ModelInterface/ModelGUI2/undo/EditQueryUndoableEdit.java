package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

public class EditQueryUndoableEdit extends AbstractUndoableEdit {

	private QueryGenerator qg;
	private boolean hasSetOldValues;
	private boolean hasSetNewValues;

	private String oldTitle;
	private String oldAxis1Name;
	private String oldNodeLevel;
	private String oldAxis2Name;
	private String oldYearLevel;
	private String oldVar;
	private String oldLabelColumnName;
	private String oldXPath;
	private boolean oldSumAll;
	private boolean oldGroup;

	private String newTitle;
	private String newAxis1Name;
	private String newNodeLevel;
	private String newAxis2Name;
	private String newYearLevel;
	private String newVar;
	private String newLabelColumnName;
	private String newXPath;
	private boolean newSumAll;
	private boolean newGroup;

	public EditQueryUndoableEdit(QueryGenerator qgIn) {
		qg = qgIn;
		hasSetOldValues = hasSetNewValues = false;
	}

	public void setOldValues(QueryGenerator qgIn) {
		oldTitle = qgIn.toString();
		oldAxis1Name = qgIn.getAxis1Name();
		oldAxis2Name = qgIn.getAxis2Name();
		oldNodeLevel = qgIn.getNodeLevel();
		oldYearLevel = qgIn.getYearLevel();
		oldVar = qgIn.getVariable();
		oldLabelColumnName = qgIn.getChartLabelColumnName();
		oldXPath = qgIn.getXPath();
		oldSumAll = qgIn.isSumAll();
		oldGroup = qgIn.isGroup();
		hasSetOldValues = true;
	}

	public void setNewValues(QueryGenerator qgIn) {
		newTitle = qgIn.toString();
		newAxis1Name = qgIn.getAxis1Name();
		newAxis2Name = qgIn.getAxis2Name();
		newNodeLevel = qgIn.getNodeLevel();
		newYearLevel = qgIn.getYearLevel();
		newVar = qgIn.getVariable();
		newLabelColumnName = qgIn.getChartLabelColumnName();
		newXPath = qgIn.getXPath();
		newSumAll = qgIn.isSumAll();
		newGroup = qgIn.isGroup();
		hasSetNewValues = true;
	}

	public boolean canUndo() {
		return hasSetOldValues;
	}

	public boolean canRedo() {
		return hasSetNewValues;
	}

	public String getPresentationName() {
		return "Edit Query";
	}

	public void undo() throws CannotUndoException {
		if(canUndo()) {
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
		} else {
			throw new CannotUndoException();
		}
	}

	public void redo() throws CannotRedoException {
		if(canRedo()) {
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
		} else {
			throw new CannotRedoException();
		}
	}
}
