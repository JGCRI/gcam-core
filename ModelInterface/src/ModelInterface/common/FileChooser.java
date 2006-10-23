package ModelInterface.common;

import javax.swing.filechooser.FileFilter;
import java.io.File;
import java.awt.Component;

/**
 * Interface which defines what a FileChooser should be 
 * able to do.
 * @author Pralit Patel
 */
public interface FileChooser {
	/**
	 * The flag for a load dialog.
	 */
	public static final int LOAD_DIALOG = 0;

	/**
	 * The flag for save dialog.
	 */
	public static final int SAVE_DIALOG = 1;

	/**
	 * Does everything from get the file dialog to setting the initial file,
	 * getting the users choices and returning them.
	 * @param parent The frame that should be the parent of the File Chooser dialog.
	 * @param title The title to give the dialog.
	 * @param loadOrSave A flag to indicate if this dialog is to load a file, or save.
	 * @param setFile The initial directory the chooser should be set to.
	 * @param fileFilter The filter for the file types.
	 * @return An array of selected/saved files or null if the user canceled. 
	 */
	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter);
}
