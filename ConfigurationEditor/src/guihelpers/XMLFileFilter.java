/*
 */
package guihelpers;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import utils.FileUtils;
import utils.Messages;

/**
 * File filter which selects xml files.
 * @author Josh Lurz
 */
public class XMLFileFilter extends FileFilter {
	/**
	 * Decides whether a given file is acceptable. A file is acceptable
	 * if it is a directory, or an XML file.
	 * @return Whether the file is acceptable.
	 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
	 */
    @Override public boolean accept(final File aFile) {
		if (aFile.isDirectory()) {
	    	return true;
	    }
	    
        final String ext = FileUtils.getExtension(aFile);
		return ( ext == null ) ? false : ext.equals("xml"); //$NON-NLS-1$
	}

	/**
	 * Get the description of the filter. This is used to create
	 * the file chooser UI panel.
	 * @return A description of the files this filter accepts.
	 * @see javax.swing.filechooser.FileFilter#getDescription()
	 */
    @Override public String getDescription() {
		return Messages.getString("XMLFileFilter.1"); //$NON-NLS-1$
	}

}
