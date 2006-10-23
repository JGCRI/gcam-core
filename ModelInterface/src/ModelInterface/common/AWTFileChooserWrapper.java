package ModelInterface.common;

import javax.swing.filechooser.FileFilter;
import java.awt.FileDialog;
import java.io.File;
import java.io.FilenameFilter;
import java.awt.Component;
import java.awt.Frame;
import java.awt.Dialog;

import ModelInterface.InterfaceMain;

/**
 * This class wraps an AWT FileDialog so that it can easily use
 * it for the FileChooser without changing the code
 * which needs to select files.
 * @author Pralit Patel 
 */
public class AWTFileChooserWrapper implements FileChooser {
	/**
	 * Default Constuctor.
	 */
	public AWTFileChooserWrapper() {
		// do nothing
	}

	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter) {
		// should I try to keep this FileDialog cached some how..
		// most likly the parent is not going to change anyways
		FileDialog toWrap = null;
		if(parent instanceof Frame) {
			toWrap = new FileDialog((Frame)parent, title);
		} else if(parent instanceof Dialog) {
			toWrap = new FileDialog((Dialog)parent, title);
		} else {
			// should I throw an exception, use the Main window as the parent or
			// some thing else?
			/*
			throw new UnsupportedOperationException(
					"This FileChooser does not support a parent Component of such a type.");
					*/
			System.out.println("WARNING: This FileChooser does not support a parent Component of such a type.");
			toWrap = new FileDialog((Frame)InterfaceMain.getInstance(), title);
		}

		if(!setFile.isDirectory()) {
			toWrap.setDirectory(setFile.getParent());
			toWrap.setFile(setFile.getName());
		} else {
			toWrap.setDirectory(setFile.getAbsolutePath());
		}
		if(loadOrSave == FileChooser.LOAD_DIALOG) {
			toWrap.setMode(FileDialog.LOAD);
		} else if(loadOrSave == FileChooser.SAVE_DIALOG) {
			toWrap.setMode(FileDialog.SAVE);
		} else {
			System.out.println("Invalid load/save flag");
			assert(false);
		}
		toWrap.setFilenameFilter(new FilenameFilter() {
			public boolean accept(File dir, String name) {
				// a null fileFilter means accept all files
				return fileFilter != null ? fileFilter.accept(new File(dir, name)) : true;
			}
		});
		toWrap.setVisible(true);
		String result = toWrap.getFile();
		if(result == null) {
			return null;
		} else {
			// FileDialog does not seem to support multiple file
			// selection
			File[] ret = new File[1];
			ret[0] = new File(toWrap.getDirectory(), result);
			return ret;
		}
	}
}
