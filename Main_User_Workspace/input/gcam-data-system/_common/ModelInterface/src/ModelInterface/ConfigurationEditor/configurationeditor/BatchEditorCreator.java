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
package ModelInterface.ConfigurationEditor.configurationeditor;

import ModelInterface.ConfigurationEditor.guihelpers.WindowCloseListener;

import java.io.File;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import ModelInterface.ConfigurationEditor.utils.Messages;

/**
 * Runnable that creates and displays the batch file editor.
 * 
 * @author Josh Lurz
 */
final class BatchEditorCreator implements Runnable {
	/**
	 * The file for the batch file editor to open.
	 */
	private transient final File mFile;

	/**
	 * Whether to launch the batch file editor in new file creation mode.
	 */
	private transient final boolean mCreateNewFile;

	/**
	 * Constructor
	 * 
	 * @param aFile
	 *            File to edit.
	 * @param aCreateNewFile
	 *            Whether to launch the batch file editor in new file
	 *            creation mode.
	 */
	public BatchEditorCreator(File aFile, final boolean aCreateNewFile) {
		super();
		mFile = aFile;
		mCreateNewFile = aCreateNewFile;
	}

	/**
	 * Run method which launches the editor if the file is valid.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public synchronized void run() {
		final JDialog editDialog = new JDialog();
		// Don't use the default closer.
		editDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		editDialog.addWindowListener(new WindowCloseListener());
		editDialog.setTitle(Messages.getString("BatchFileEditor.21")); //$NON-NLS-1$
		
		// Create the editor panel.
		final BatchFileEditor editorPanel = new BatchFileEditor(mFile
				.getAbsolutePath(), mCreateNewFile);
		editDialog.setContentPane(editorPanel);

		// Check if the batch file editor can be shown.
		if (editorPanel.isValidEditor()) {
			editDialog.pack();
			editDialog.setVisible(true);
		}
	}
}
