package ModelInterface.ModelGUI2.undo;

import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.UndoableEdit;
import javax.swing.undo.AbstractUndoableEdit;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Provide UndoableEdits with the ability to notify MiUndoableEditListeners
 * of undos and redo.
 * @author Pralit Patel
 * @see ModelInterface.ModelGUI2.undo.MiUndoableEditListener
 */ 
public abstract class MiAbstractUndoableEdit extends AbstractUndoableEdit {
	/**
	 * List of registered listeners
	 */
	protected List<MiUndoableEditListener> editListeners = new ArrayList<MiUndoableEditListener>();

	/**
	 * Registers a listener with this Edit.
	 * @param listener The listener that will be registered.
	 */
	public void addListener(MiUndoableEditListener listener) {
		editListeners.add(listener);
	}

	/**
	 * Unregisters a listener with this Edit.
	 * @param listener The listener that should be removed.
	 */
	public void removeListener(MiUndoableEditListener listener) {
		editListeners.remove(listener);
	}

	/**
	 * Notifies all registerd listeners that an undo has occured.
	 * @param source The source of the undo.
	 * @param edit The edit that did the undo.
	 */ 
	public void fireUndoPerformed(Object source, UndoableEdit edit) {
		UndoableEditEvent e = new UndoableEditEvent(source, edit);
		for(Iterator<MiUndoableEditListener> it = editListeners.iterator(); it.hasNext(); ) {
			it.next().undoPerformed(e);
		}
	}

	/**
	 * Notifies all registerd listeners that an redo has occured.
	 * @param source The source of the redo.
	 * @param edit The edit that did the redo.
	 */ 
	public void fireRedoPerformed(Object source, UndoableEdit edit) {
		UndoableEditEvent e = new UndoableEditEvent(source, edit);
		for(Iterator<MiUndoableEditListener> it = editListeners.iterator(); it.hasNext(); ) {
			it.next().redoPerformed(e);
		}
	}

	/**
	 * If we are going to die might as well remove all listeners.
	 * @see javax.swing.undo.UndoableEdit
	 */
	public void die() {
		editListeners.clear();
	}
}
