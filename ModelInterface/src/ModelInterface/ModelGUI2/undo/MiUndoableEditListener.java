package ModelInterface.ModelGUI2.undo;

import javax.swing.event.UndoableEditEvent;

/**
 * Create and interface to listen for undoable events. This version 
 * makes it clear as to if an undo or redo occured.
 * @author Pralit Patel
 */
public interface MiUndoableEditListener extends java.util.EventListener {
	/**
	 * Will be called when an undo occurs.  The event provides
	 * the source and the UndoableEdit that performed the undo.
	 * @param e The Event that did the undo.
	 */
	public void undoPerformed(UndoableEditEvent e);

	/**
	 * Will be called when an redo occurs.  The event provides
	 * the source and the UndoableEdit that performed the redo.
	 * @param e The Event that did the redo.
	 */
	public void redoPerformed(UndoableEditEvent e);
}
