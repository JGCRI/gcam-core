/**
 * 
 */
package utils;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * File filter which selects executable files.
 * @author Josh Lurz
 */
public class ExeFileFilter extends FileFilter {
	/**
	 * Decides whether a given file is acceptable. A file is acceptable
	 * if it is a directory, or an executable file.
	 * @return Whether the file is acceptable.
	 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
	 */
    @Override public boolean accept(File aFile) {
        if (aFile.isDirectory()) {
            return true;
        }
        String ext = Util.getExtension(aFile);
        return (ext != null) ? ext.equals("exe") : false; //$NON-NLS-1$
    }

	/**
	 * Get the description of the filter. This is used to create
	 * the file chooser UI panel.
	 * @return A description of the files this filter accepts.
	 * @see javax.swing.filechooser.FileFilter#getDescription()
	 */
    @Override public String getDescription() {
        return Messages.getString("ExeFileFilter.1"); //$NON-NLS-1$
    }

}
