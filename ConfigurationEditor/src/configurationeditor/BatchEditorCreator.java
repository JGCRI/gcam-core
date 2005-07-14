package configurationeditor;

import java.io.File;

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
		final BatchFileEditor batchEditor = new BatchFileEditor(mFile
				.getAbsolutePath(), mCreateNewFile);
		// Check if the batch file editor can be shown.
		if (batchEditor.isValidEditor()) {
			batchEditor.pack();
			batchEditor.setVisible(true);
		}
	}
}