package configurationeditor;

import guihelpers.WindowCloseListener;

import java.io.File;

import javax.swing.JDialog;
import javax.swing.WindowConstants;

import utils.Messages;

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