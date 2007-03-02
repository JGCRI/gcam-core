package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

public class EditQueryUndoableEdit extends MiAbstractUndoableEdit {

	private QueryGenerator qg;
	private boolean hasSetOldValues;
	private boolean hasSetNewValues;
	private boolean hasRealChanges;

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
	private String oldComments;

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
	private String newComments;

	public EditQueryUndoableEdit(QueryGenerator qgIn, MiUndoableEditListener listener) {
		qg = qgIn;
		hasRealChanges = hasSetOldValues = hasSetNewValues = false;
		addListener(listener);
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
		oldComments = qgIn.getRealComments();
		hasSetOldValues = true;
	}

	public void setNewValues(QueryGenerator qgIn) {
		System.out.println("hasRealChanges "+hasRealChanges);
		hasRealChanges = hasRealChanges || !doDiffCheck(oldTitle, newTitle = qgIn.toString());
		System.out.println("hasRealChanges "+hasRealChanges);
		hasRealChanges = hasRealChanges || !doDiffCheck(oldAxis1Name, newAxis1Name = qgIn.getAxis1Name());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldAxis2Name, newAxis2Name = qgIn.getAxis2Name());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldNodeLevel, newNodeLevel = qgIn.getNodeLevel());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldYearLevel, newYearLevel = qgIn.getYearLevel());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldVar, newVar = qgIn.getVariable());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldLabelColumnName,
			newLabelColumnName = qgIn.getChartLabelColumnName());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldXPath, newXPath = qgIn.getXPath());
		hasRealChanges = hasRealChanges || oldSumAll != (newSumAll = qgIn.isSumAll());
		hasRealChanges = hasRealChanges || oldGroup != (newGroup = qgIn.isGroup());
		hasRealChanges = hasRealChanges || !doDiffCheck(oldComments, newComments = qgIn.getRealComments());
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
				oldVal.equals(newVal);
	}

	public boolean hasRealChanges() {
		if(!hasSetOldValues || !hasSetNewValues) {
			// error?
			return false;
		}
		return hasRealChanges;
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
			fireUndoPerformed(this, this);
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
			fireRedoPerformed(this, this);
		} else {
			throw new CannotRedoException();
		}
	}
}
