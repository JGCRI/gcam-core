/**
 * 
 */
package actions;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

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
     * The process which is running the model executable.
     */
    private Process mModelProcess = null;
    
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
            // Start the model. This will occur in a seperate thread.
			mModelProcess = procBuilder.start();
            
            // Hookup to the model input and output streams.
			mModelOutput = new BufferedInputStream(mModelProcess.getInputStream());
			mModelError = new BufferedInputStream(mModelProcess.getErrorStream());
			
            // Set a flag indicating the model is currently running 
            // to prevent the user from running the model twice.
			mModelRunning = true;
            
			// Create another process which will print the output to a dialog box.
			Thread outputWatcher = new OutputWatcher();
            // Start the output watching process.
			outputWatcher.start();
            
			// Wait on this thread for the model to complete.
            mModelProcess.waitFor();
			
			// Get the exit code of the process.
			mModelExitCode = mModelProcess.exitValue();
            
            // Forcibly clean up the process.
            mModelProcess.destroy();

			// Log that the model has completed running.
			Logger.global.log(Level.INFO, Messages.getString("ModelRunner.3") + mModelExitCode + "."); //$NON-NLS-1$ //$NON-NLS-2$
			// Stop the output watching thread by setting that the model is no longer running.
			mModelRunning = false;

		} catch (IOException e) {
			// Run failed.
		    String errorTitle = Messages.getString("RunAction.15"); //$NON-NLS-1$
		    String errorMessage = Messages.getString("RunAction.16") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
		    JOptionPane.showMessageDialog(mParentEditor, errorMessage,
		                errorTitle, JOptionPane.ERROR_MESSAGE);
		}
		catch(InterruptedException e){
		    Logger.global.log(Level.WARNING, Messages.getString("ModelRunner.5") + e.getMessage()); //$NON-NLS-1$
		}
        // Always set that the model is no longer running on completion.
        mInitiatingRunAction.setModelRunning(false);
	}
    
    /**
     * A class which watches the output of the executable
     * and dumps it onto a window for the user.
     * @author Josh Lurz
     */
    private final class OutputWatcher extends Thread {
        /**
         * Method which the Thread initiates when it starts. The thread
         * creates a new window and a displays output from the executable
         * into a text area in the window.
         */
        public void run(){
        	// Create a new dialog box to display the output.
        	JDialog outputDialog = new JDialog(mParentEditor, Messages.getString("ModelRunner.2"), false); //$NON-NLS-1$
            outputDialog.setLayout(new BoxLayout(outputDialog.getContentPane(), BoxLayout.Y_AXIS));
            
            // Stop the model if the user closes the window.
            outputDialog.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            outputDialog.addWindowListener(new OutputWindowCloseListener());
        	outputDialog.setPreferredSize(new Dimension(400,400));
            
            // Create a text area to contain the output.
        	final JTextArea outputArea = new JTextArea();
            
            // Don't allow the user to edit model output.
        	outputArea.setEditable(false);
            
            // Put the output area into a scroll pane.
        	final JScrollPane outputScrollPane = new JScrollPane(outputArea,
        			ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
        			ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
            
            // Setup the scroll pane to automatically scroll as content
            // is added.
        	outputScrollPane.setAutoscrolls(true);
            outputScrollPane.addMouseMotionListener(new ScrollPaneScroller());

             
            // Add the scroll pane at the center of the window.
        	outputDialog.add(outputScrollPane);
        	
            // Add a button at the bottom which will stop the model.
            JButton terminateButton = new JButton();
            terminateButton.setText("Terminate");
            terminateButton.setToolTipText("Terminate the model run.");
            terminateButton.setPreferredSize(new Dimension(50,40));
            terminateButton.addActionListener(new ActionListener() {
                /**
                 * Method called when the button is clicked which
                 * will terminate the model
                 * @param aEvent The event received.
                 */
                public void actionPerformed(ActionEvent aEvent) {
                    Logger.global.log(Level.INFO, "Terminate button pressed, attempting to stop the model.");
                    // Set the model and the output watcher 
                    // to stop when there is a chance.
                    mModelRunning = false;
                    mModelProcess.destroy();
                }
                
            });
            outputDialog.add(terminateButton);
            
            // TODO: Save button?
            
            // Display the dialog.
        	outputDialog.pack();
        	outputDialog.setVisible(true);
            
            // Lower the priority on the current thread because this will
            // become the output watcher. 
            setPriority(Thread.MIN_PRIORITY);
            
            // Create a buffer to read data from the model. This is 
            // much faster than reading character by character.
        	final int BUFFER_SIZE = 100;
        	byte buffer[] = new byte[BUFFER_SIZE];
            
            // Setup a loop which will continue until the model stops running
            // which adds output to the text area.
        	while (mModelRunning) {
                // Sleep so the model has a chance to produce
                // some output.
        		try {
        			sleep(100); // Tweak this.
                    // Should we explicitly yield here?
        		} catch (InterruptedException e) {
                    // This could be caused legitimately by the Cancel
                    // button.
        			e.printStackTrace();
                    // If we were interrupted the user tried to cancel
                    // the model run, so stop the loop.
                    mModelRunning = false;
        		}
        		try {
                    // Check if there is any data available from
                    // the model output stream.
                    if (mModelOutput.available() > 0) {
                        int bytesRead = mModelOutput.read(buffer, 0,
                                BUFFER_SIZE);
                        if (bytesRead > 0) {
                            final String newOutput = new String(buffer, 0, bytesRead);
                            // Update the interface on the GUI thread.
                            // Using invoke and wait to avoid a race condition
                            // with the string buffer.
                            SwingUtilities.invokeLater(new Runnable() {
                                /**
                                 * Method called from the GUI thread to update
                                 * the output area.
                                 */
                                public void run() {
                                    // Add the output to the output area.
                                    outputArea.append(newOutput);
                                    // Scroll the output window.
                                    // Get the bounds of the output area so we can
                                    // determine its bottom.
                                    Rectangle bottom = outputArea.getBounds();
                                    
                                    // Get the viewable rectangle currently of
                                    // the scroll pane so that the viewable window
                                    // doesn't adjust horizontally.
                                    Rectangle currView = outputScrollPane.getViewportBorderBounds();
                                    
                                    // Create a rectangle to scroll to which is located at the
                                    // bottom the output area in the same horizontal position as it 
                                    // was before the move. The last two parameters, the width and height
                                    // of the rectangle, do not matter.
                                    Rectangle lowerBound = new Rectangle(bottom.y + bottom.height, currView.x, 1, 1);
                                    outputScrollPane.scrollRectToVisible(lowerBound);
                                }
                            });
                        }
                    }
                    
                    // Check if there is any data available from
                    // the model error stream. Send this to standard 
                    // output.
        			if (mModelError.available() > 0) {
        				int bytesRead = mModelError.read(buffer, 0,
        						BUFFER_SIZE);
        				// if (bytesRead > 0) {
                        //     Logger.global.log(Level.WARNING, new String(buffer, 0,bytesRead) );
        				// }
                        // TODO: Remove this once the model writes only to cout.
                        if (bytesRead > 0) {
                            final String newOutput = new String(buffer, 0, bytesRead);
                            // Update the interface on the GUI thread.
                            // Using invoke and wait to avoid a race condition
                            // with the string buffer.
                            SwingUtilities.invokeLater(new Runnable() {
                                /**
                                 * Method called from the GUI thread to update
                                 * the output area.
                                 */
                                public void run() {
                                    // Add the output to the output area.
                                    outputArea.append(newOutput);
                                    // Scroll the output window.
                                    // Get the bounds of the output area so we can
                                    // determine its bottom.
                                    Rectangle bottom = outputArea.getBounds();
                                    
                                    // Get the viewable rectangle currently of
                                    // the scroll pane so that the viewable window
                                    // doesn't adjust horizontally.
                                    Rectangle currView = outputScrollPane.getViewportBorderBounds();
                                    
                                    // Create a rectangle to scroll to which is located at the
                                    // bottom the output area in the same horizontal position as it 
                                    // was before the move. The last two parameters, the width and height
                                    // of the rectangle, do not matter.
                                    Rectangle lowerBound = new Rectangle(bottom.y + bottom.height, currView.x, 1, 1);
                                    outputScrollPane.scrollRectToVisible(lowerBound);
                                }
                            });
                        }
                    }
        		} catch (IOException e) {
        			e.printStackTrace();
                    // An error occurred, cancel the model run.
                    mModelRunning = false;
                    mModelProcess.destroy();
        		}
        	}
            // Loop exited
        	Logger.global.log(Level.INFO, "Output watching loop exited. Thread finished.");
        }
        
        /**
         * Object which scrolls a JScrollPane when the user moves the mouse. 
         * @author Josh Lurz
         *
         */
        private final class ScrollPaneScroller extends MouseMotionAdapter {
            /**
             * Method called when the mouse is dragged.
             * @param aEvent The mouse event received.
             * TODO: Move to interface utils.
             */
            public void mouseDragged(MouseEvent aEvent) {
                Rectangle r = new Rectangle(aEvent.getX(), aEvent.getY(), 1, 1);
                ((JScrollPane)aEvent.getSource()).scrollRectToVisible(r);
            }
        }
    }

    /**
     * A window listener which will stop the model executable
     * and the output watching thread at the first chance.
     * @author Josh Lurz
     */
    private final class OutputWindowCloseListener implements WindowListener {
        public void windowOpened(WindowEvent aEvent) {
            // Do nothing
        }

        public void windowClosing(WindowEvent aEvent) {
            Logger.global.log(Level.INFO, "Output window closed, attempting to stop the model.");
            // Stop the loop and from that the model
            // and output thread when there is a chance.
            mModelRunning = false;
            mModelProcess.destroy();
            
            // Close the window.
            ((JDialog)aEvent.getSource()).setVisible(false);
        }

        public void windowClosed(WindowEvent aEvent) {
            // Do nothing
        }

        public void windowIconified(WindowEvent aEvent) {
            // Do nothing
        }

        public void windowDeiconified(WindowEvent aEvent) {
            // Do nothing
        }

        public void windowActivated(WindowEvent aEvent) {
            // Do nothing
        }

        public void windowDeactivated(WindowEvent aEvent) {
            // Do nothing
        }
    }


}
