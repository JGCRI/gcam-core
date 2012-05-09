/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ConfigurationEditor.utils;

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
    @Override public boolean accept(final File aFile) {
        if (aFile.isDirectory()) {
            return true;
        }
        final String ext = FileUtils.getExtension(aFile);
        return (ext == null) ? false : ext.equals("exe"); //$NON-NLS-1$
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
