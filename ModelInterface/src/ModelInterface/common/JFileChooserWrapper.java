package ModelInterface.common;

import javax.swing.filechooser.FileFilter;
import javax.swing.JFileChooser;
import java.io.File;
import java.awt.Component;
import java.awt.event.ActionListener;

/**
 * This class wraps a JFileChooser so that it can easily use
 * a JFileChooser for the FileChooser without changing the code
 * which needs to select files.
 * @author Pralit Patel 
 */
public class JFileChooserWrapper implements FileChooser {
	/**
	 * The instance to the JFileChooser this class
	 * is wrapping.
	 */
	private JFileChooser toWrap;

	/**
	 * Default Constructor.
	 */
	public JFileChooserWrapper() {
		toWrap = new JFileChooser();
		// maybe I should not hard code this,
		// or at least provide some way of changing this
		toWrap.setMultiSelectionEnabled(true);
	}

	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter) {
		return doFilePrompt(parent, title, loadOrSave, setFile, fileFilter, null, null);
	}

	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter,
			final ActionListener l, final String actionCommand) {
		// will this inherit the comment from FileChooser, should I write
		// a more descriptive comment 
		// should I have a finally that cleans out the set files, file filters, etcetera ? 
		toWrap.setDialogTitle(title);
		if(setFile.isDirectory()) {
			toWrap.setCurrentDirectory(setFile);
		} else {
			toWrap.setSelectedFile(setFile);
		}
		// TODO: find out if this will this be ok with a null FileFilter
		toWrap.setFileFilter(fileFilter);

		// TODO: find a better way as this is a hack
		if(fileFilter.getDescription().startsWith("Directory")) {
			toWrap.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		} else {
			toWrap.setFileSelectionMode(JFileChooser.FILES_ONLY);
		}

		int result = JFileChooser.CANCEL_OPTION;
		if(loadOrSave == FileChooser.LOAD_DIALOG) {
			result = toWrap.showOpenDialog(parent);
		} else if(loadOrSave == FileChooser.SAVE_DIALOG) {
			result = toWrap.showSaveDialog(parent);
		} else {
			System.out.println("Invalid flag for load/save dialog");
			assert(false);
		}
		if(result == JFileChooser.APPROVE_OPTION) {
			File[] ret = toWrap.getSelectedFiles();
			if (ret.length == 0) {
				ret = new File[1];
				ret[0] = toWrap.getSelectedFile();
			}
			if(l != null && actionCommand != null) {
				RecentFilesList.getInstance().addFile(ret, l, actionCommand);
			}
			return ret;
		} else {
			return null;
		}
	}
}
