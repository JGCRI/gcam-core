package ModelInterface.ModelGUI2.undo;

import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import com.sleepycat.dbxml.XmlQueryContext;
import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

import ModelInterface.ModelGUI2.ScenarioListItem;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.xmldb.XMLDB;

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
	 * scenario list.  First queries the db to the get the correct
	 * scenario context node then uses the xmldb's setValue to do 
	 * the work.
	 * @param fromName The scenario name that is going to be replaced.
	 * @param toName The scenario will be named after this method.
	 */
	private void doRename(String fromName, String toName) {
		// As of dbxml 2.4 we must get an eager result otherwise we will have a deadlock
		// when we try to update
		XmlQueryContext qc = XMLDB.getInstance().createQueryContext();
		XmlResults res = null;
		try { 
			qc.setEvaluationType(XmlQueryContext.Eager);
			res = XMLDB.getInstance().createQuery(
					"/scenario[@date='"+oldName.getScnDate()+"' and @name='"+fromName+"']/@name", null, null, null, qc);
			// if it has no results that is bad.. I'll just go with the null pointer exception
			XmlValue context = res.next();
			XMLDB.getInstance().setValue(context, toName);
			viewer.resetScenarioList();
			context.delete();
		} catch(XmlException e) {
			// TODO: put an error up on the screen
			e.printStackTrace();
		} finally {
			if(res != null) {
				res.delete();
			}
			qc.delete();
		}
	}
}
