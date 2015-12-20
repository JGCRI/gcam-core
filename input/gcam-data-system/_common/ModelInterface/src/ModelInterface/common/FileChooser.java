/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
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
package ModelInterface.common;

import javax.swing.filechooser.FileFilter;
import java.io.File;
import java.awt.Component;
import java.awt.event.ActionListener;

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
	 * getting the users choices and returning them. If the action listener and
	 * action command are not null the file will be added to the recent files list.
	 * @param parent The frame that should be the parent of the File Chooser dialog.
	 * @param title The title to give the dialog.
	 * @param loadOrSave A flag to indicate if this dialog is to load a file, or save.
	 * @param setFile The initial directory the chooser should be set to.
	 * @param fileFilter The filter for the file types.
	 * @param l The ActionListener that could open up the selected file.
	 * @param actionCommand The command that would have to be used to open the selected file.
	 * @return An array of selected/saved files or null if the user canceled. 
	 */
	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter,
			ActionListener l, String actionCommand);

	// should I deprecate this?
	/**
	 * A convenience method for doFilePrompt in case the user is not interested
	 * in having the file added to the recent files list.
	 * @param parent The frame that should be the parent of the File Chooser dialog.
	 * @param title The title to give the dialog.
	 * @param loadOrSave A flag to indicate if this dialog is to load a file, or save.
	 * @param setFile The initial directory the chooser should be set to.
	 * @param fileFilter The filter for the file types.
	 * @return An array of selected/saved files or null if the user canceled. 
	 * @see doFilePrompt(Component, String, int, File, FileFilter, ActionListener, String)
	 */
	public File[] doFilePrompt(final Component parent, final String title, 
			final int loadOrSave, final File setFile, final FileFilter fileFilter);
}
