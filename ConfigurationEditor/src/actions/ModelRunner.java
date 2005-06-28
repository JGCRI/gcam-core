/**
 * 
 */
package actions;

import java.awt.Dimension;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;

import configurationeditor.ConfigurationEditor;

/**
 * Runs the model executable and displays output from the executable
 * in a window.
 * @author Josh Lurz
 */
public class ModelRunner implements Runnable {

	/**
	 * Path to the executable.
	 */
	private File mExecutableFile = null;
	
	/**
	 * File containing the location of the temporary configuration file.
	 */
	private File mTempConfLocation = null;
	
	/**
	 * A reference to the parent editor.
	 *
	 */
	private ConfigurationEditor mParentEditor = null;
	
	/**
	 * The run action which started this run.
	 */
	private RunAction mInitiatingRunAction = null;
	
	/**
	 * The output stream of the model(STDOUT).
	 */
	private InputStream mModelOutput = null;
	
	/**
	 * The error stream of the model(STDERR).
	 */
	private InputStream mModelError = null;
	
	/**
	 * Private boolean which tells whether the model is running so 
	 * that the output watching thread knows when to stop.
	 */
	private boolean mModelRunning = false;
	
	/**
	 * The exit code of the model. This is checked by the output watching thread.
	 */
	private int mModelExitCode = 0;
	
	/**
	 * Constructor which initializes variables needed to run the model.
	 * @param aExecutableFile The absolute path to the model executable.
	 * @param aTempConfLocation The location of the temporary configuration file.
	 * @param aParentEditor A reference to the top level editor window.
	 * @param aRunAction The run action which initiated this model run.
	 */
	public ModelRunner(File aExecutableFile, File aTempConfLocation, ConfigurationEditor aParentEditor, RunAction aRunAction) {
		super();
		mExecutableFile = aExecutableFile;
		mTempConfLocation = aTempConfLocation;
		mParentEditor = aParentEditor;
		mInitiatingRunAction = aRunAction;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		// Set that the model is now running.
		mInitiatingRunAction.setModelRunning(true);
		// Need to get the location to run the executable from.
		String parentDirectory = mExecutableFile.getParent();
		
		Logger.global
				.log(
						Level.INFO,
						Messages.getString("RunAction.12") + mExecutableFile.getAbsolutePath() //$NON-NLS-1$
								+ " -C" + mTempConfLocation.getAbsolutePath() + Messages.getString("ModelRunner.0") + parentDirectory); //$NON-NLS-1$ //$NON-NLS-2$
		ProcessBuilder procBuilder = new ProcessBuilder(mExecutableFile.getAbsolutePath(), "-C" + mTempConfLocation.getAbsolutePath()); //$NON-NLS-1$
		// Set the working directory.
		procBuilder.directory(new File(parentDirectory));
		try {
			Process modelProcess = procBuilder.start();
			mModelOutput = new BufferedInputStream(modelProcess.getInputStream());
			mModelError = new BufferedInputStream(modelProcess.getErrorStream());
			
			mModelRunning = true;
			// Create another process which will print the output to a dialog box.
			Thread outputWatcher = new Thread( new Runnable(){
				public void run(){
					// Create a new dialog box.
					JDialog outputDialog = new JDialog(mParentEditor, Messages.getString("ModelRunner.2"), false); //$NON-NLS-1$
					outputDialog.setPreferredSize(new Dimension(400,400));
					JTextArea outputArea = new JTextArea();
					
					JScrollPane scrollPane = new JScrollPane(outputArea,
							ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
							ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
					outputDialog.add(scrollPane);
					
					outputDialog.pack();
					outputDialog.setVisible(true);
					// Setup a loop which will continue until the model stops running
					// which adds output to the text area.
					final int BUFFER_SIZE = 100;
					byte buffer[] = new byte[BUFFER_SIZE];
					while (mModelRunning) {
						// Attempt to read first from the output stream
						// and then from the error stream. Is this threadsafe?
						try {
							if (mModelOutput.available() > 0) {
								int bytesRead = mModelOutput.read(buffer, 0,
										BUFFER_SIZE);
								if (bytesRead > 0) {
									String newOutput = new String(buffer, 0,
											bytesRead);
									outputArea.append(newOutput);
								}
							}
							if (mModelError.available() > 0) {
								int bytesRead = mModelError.read(buffer, 0,
										BUFFER_SIZE);
								if (bytesRead > 0) {
									String newOutput = new String(buffer, 0,
											bytesRead);
									outputArea.append(newOutput);
								}
							}
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				}
			});
			outputWatcher.start();
			// Wait on this thread for the model to complete.
			modelProcess.waitFor();
			
			// Get the exit code of the process.
			mModelExitCode = modelProcess.exitValue();
			
			// Log that the model has completed running.
			Logger.global.log(Level.INFO, Messages.getString("ModelRunner.3") + mModelExitCode + "."); //$NON-NLS-1$ //$NON-NLS-2$
			// Stop the output watching thread by setting that the model is no longer running.
			mModelRunning = false;

		} catch (IOException e) {
			// Run failed.
			String errorTitle = Messages.getString("RunAction.15"); //$NON-NLS-1$
			String errorMessage = Messages.getString("RunAction.16") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
			JOptionPane.showMessageDialog(mParentEditor, errorTitle,
					errorMessage, JOptionPane.ERROR_MESSAGE);
		}
		catch(InterruptedException e){
			Logger.global.log(Level.WARNING, Messages.getString("ModelRunner.5") + e.getMessage()); //$NON-NLS-1$
		}
		finally {
			// Always set that the model is no longer running on completion.
			mInitiatingRunAction.setModelRunning(false);
		}
	}

}
