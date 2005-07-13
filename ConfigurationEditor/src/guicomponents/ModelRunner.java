/**
 * 
 */
package guicomponents;

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

import actions.RunAction;

import utils.Messages;

import configurationeditor.ConfigurationEditor;

/**
 * Runs the model executable and displays output from the executable in a
 * window.
 * 
 * @author Josh Lurz
 */
public class ModelRunner implements Runnable {
    /**
     * Path to the executable.
     */
    private final transient File mExecutableFile;

    /**
     * File containing the location of the temporary configuration file.
     */
    private final transient File mTempConfLocation;

    /**
     * A reference to the parent editor.
     * 
     */
    private final transient ConfigurationEditor mParentEditor;

    /**
     * The run action which started this run.
     */
    private final transient RunAction mSourceAction;

    /**
     * Private boolean which tells whether the model is running so that the
     * output watching thread knows when to stop.
     */
    private transient boolean mModelRunning = false;

    /**
     * The exit code of the model. This is checked by the output watching
     * thread.
     */
    private transient int mModelExitCode;

    /**
     * The process which is running the model executable.
     */
    // private transient Process mModelProcess = null;
    /**
     * Constructor which initializes variables needed to run the model.
     * 
     * @param aExecutableFile
     *            The absolute path to the model executable.
     * @param aTempConfLocation
     *            The location of the temporary configuration file.
     * @param aParentEditor
     *            A reference to the top level editor window.
     * @param aRunAction
     *            The run action which initiated this model run.
     */
    public ModelRunner(File aExecutableFile, File aTempConfLocation,
            ConfigurationEditor aParentEditor, RunAction aRunAction) {
        super();
        mExecutableFile = aExecutableFile;
        mTempConfLocation = aTempConfLocation;
        mParentEditor = aParentEditor;
        mSourceAction = aRunAction;
    }

    /**
     * Executes the model and dispatches a second thread to read output from the
     * model.
     * 
     * @see java.lang.Runnable#run()
     */
    public void run() {
        // Set that the model is now running.
        mSourceAction.setModelRunning(true);
        // Need to get the location to run the executable from.
        final String parentDirectory = mExecutableFile.getParent();

        Logger.global
                .log(
                        Level.INFO,
                        Messages.getString("RunAction.12") + mExecutableFile.getAbsolutePath() //$NON-NLS-1$
                                + " -C" + mTempConfLocation.getAbsolutePath() + Messages.getString("ModelRunner.0") + parentDirectory); //$NON-NLS-1$ //$NON-NLS-2$
        final ProcessBuilder procBuilder = new ProcessBuilder(mExecutableFile
                .getAbsolutePath(), "-C" + mTempConfLocation.getAbsolutePath()); //$NON-NLS-1$
        // Set the working directory.
        procBuilder.directory(new File(parentDirectory));
        try {
            // Start the model. This will occur in a seperate thread.
            final Process modelProcess = procBuilder.start();

            // Hookup to the model input and output streams.
            final InputStream modelOutput = new BufferedInputStream(
                    modelProcess.getInputStream());
            final InputStream modelError = new BufferedInputStream(modelProcess
                    .getErrorStream());

            // Set a flag indicating the model is currently running
            // to prevent the user from running the model twice.
            mModelRunning = true;

            // Create another process which will print the output to a dialog
            // box.
            final Thread outputWatcher = new OutputWatcher(modelProcess,
                    modelOutput, modelError);
            // Start the output watching process.
            outputWatcher.start();

            // Wait on this thread for the model to complete.
            modelProcess.waitFor();

            // Get the exit code of the process.
            mModelExitCode = modelProcess.exitValue();

            // Forcibly clean up the process.
            modelProcess.destroy();

            // Log that the model has completed running.
            Logger.global.log(Level.INFO,
                    Messages.getString("ModelRunner.3") + mModelExitCode + "."); //$NON-NLS-1$ //$NON-NLS-2$
            // Stop the output watching thread by setting that the model is no
            // longer running.
            mModelRunning = false;

        } catch (IOException e) {
            // Run failed.
            final String errorTitle = Messages.getString("RunAction.15"); //$NON-NLS-1$
            final String errorMessage = Messages.getString("RunAction.16") + e.getMessage() + "."; //$NON-NLS-1$ //$NON-NLS-2$
            JOptionPane.showMessageDialog(mParentEditor, errorMessage,
                    errorTitle, JOptionPane.ERROR_MESSAGE);
        } catch (InterruptedException e) {
            Logger.global.log(Level.WARNING, Messages
                    .getString("ModelRunner.5") + e.getMessage()); //$NON-NLS-1$
        }
        // Always set that the model is no longer running on completion.
        mSourceAction.setModelRunning(false);
    }

    /**
     * A class which watches the output of the executable and dumps it onto a
     * window for the user.
     * 
     * @author Josh Lurz
     */
    private final class OutputWatcher extends Thread {
        /**
         * The process from which it is watching for output.
         */
        private transient final Process mProcess;

        /**
         * The output stream being watched(STDOUT).
         */
        private transient final InputStream mModelOutput;

        /**
         * The error stream being watched(STDERR).
         */
        private transient final InputStream mModelError;

        /**
         * Constructor
         * 
         * @param aProcess
         *            The process from which it is watching for output.
         * @param aModelOutput
         *            The output stream being watched(STDOUT).
         * @param aModelError
         *            The error stream being watched(STDERR).
         */
        public OutputWatcher(Process aProcess, InputStream aModelOutput,
                InputStream aModelError) {
            super();
            mProcess = aProcess;
            mModelOutput = aModelOutput;
            mModelError = aModelError;
        }

        /**
         * Method which the Thread initiates when it starts. The thread creates
         * a new window and a displays output from the executable into a text
         * area in the window.
         */
        @Override
        public void run() {
            // Create the GUI components.
            final JDialog outputDialog = createOutputDialog();
            final JTextArea outputArea = createOutputArea();
            final JScrollPane outputScrollPane = createOutputScrollPane(outputArea);

            // Add the scroll pane at the center of the window.
            outputDialog.add(outputScrollPane);
            outputDialog.add(createTerminateButton());

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
            final byte buffer[] = new byte[BUFFER_SIZE];

            // Setup a loop which will continue until the model stops running
            // which adds output to the text area.
            while (mModelRunning) {
                // Sleep so the model has a chance to produce
                // some output.
                try {
                    sleep(100); // Tweak this.
                    // Should we explicitly yield here?
                } catch (InterruptedException aException) {
                    // This could be caused legitimately by the Cancel
                    // button.
                    Logger.global.throwing("Interrupted Exception",
                            "ModelRunner", aException);
                    // If we were interrupted the user tried to cancel
                    // the model run, so stop the loop.
                    mModelRunning = false;
                }
                try {
                    updateOutputWindow(outputArea, outputScrollPane,
                            mModelOutput, buffer);
                    updateOutputWindow(outputArea, outputScrollPane,
                            mModelError, buffer);

                } catch (IOException aException) {
                    Logger.global.throwing("Interrupted Exception",
                            "ModelRunner", aException);
                    // An error occurred, cancel the model run.
                    mModelRunning = false;
                    mProcess.destroy();
                }
            }
            // Loop exited
            Logger.global.log(Level.INFO,
                    "Output watching loop exited. Thread finished.");
        }

        /**
         * Read from the given input stream and pass the output to an object
         * which will update the text area and scroll panel.
         * 
         * @param aOutputArea
         *            The output area to update.
         * @param aOutputScrollPane
         *            The output scroll pane to update.
         * @param aStream
         *            The stream from which to read.
         * @param aBuffer
         *            The temporary buffer to use.
         * @throws IOException
         *             Exception thrown when reading fails.
         */
        private void updateOutputWindow(final JTextArea aOutputArea,
                final JScrollPane aOutputScrollPane, final InputStream aStream,
                final byte[] aBuffer) throws IOException {
            // Check if there is any data available from
            // the model output stream.
            if (aStream.available() > 0) {
                final int bytesRead = aStream.read(aBuffer, 0, aBuffer.length);
                if (bytesRead > 0) {
                    final String newOutput = new String(aBuffer, 0, bytesRead);
                    // Update the interface on the GUI thread.
                    // Using invoke and wait to avoid a race condition
                    // with the string buffer.
                    SwingUtilities.invokeLater(new ScreenPrinter(
                            aOutputScrollPane, aOutputArea, newOutput));
                }
            }
        }

        /**
         * Creates a button which can terminate the model process.
         * 
         * @return A button which can terminate the model process.
         */
        private JButton createTerminateButton() {
            // Add a button at the bottom which will stop the model.
            final JButton terminateButton = new JButton("Terminate");
            terminateButton.setToolTipText("Terminate the model run.");
            terminateButton.addActionListener(new TerminateButtonListener());
            return terminateButton;
        }

        /**
         * An output scroll pane which has auto scroll activated and a mouse
         * listener to scroll the pane.
         * 
         * @param aOutputArea
         *            The output area to contain in the scroll pane.
         * @return A scroll pane wrapping the output area.
         */
        private JScrollPane createOutputScrollPane(final JTextArea aOutputArea) {
            // Put the output area into a scroll pane.
            final JScrollPane outputScrollPane = new JScrollPane(aOutputArea,
                    ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

            // Setup the scroll pane to automatically scroll as content
            // is added.
            outputScrollPane.setAutoscrolls(true);
            outputScrollPane.addMouseMotionListener(new ScrollPaneScroller());
            return outputScrollPane;
        }

        /**
         * Creates a text area to display output from the model.
         * @return An output area to display output.
         */
        private JTextArea createOutputArea() {
            // Create a text area to contain the output.
            final JTextArea outputArea = new JTextArea();

            // Don't allow the user to edit model output.
            outputArea.setEditable(false);
            return outputArea;
        }

        /**
         * Creates a dialog which will display output from the model.
         * @return An output dialog to contain output from the model.
         */
        private JDialog createOutputDialog() {
            // Create a new dialog box to display the output.
            final JDialog outputDialog = new JDialog(mParentEditor, Messages
                    .getString("ModelRunner.2"), false); //$NON-NLS-1$
            outputDialog.setLayout(new BoxLayout(outputDialog.getContentPane(),
                    BoxLayout.Y_AXIS));

            // Stop the model if the user closes the window.
            outputDialog
                    .setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            outputDialog.addWindowListener(new OutputWindowCloseListener(
                    mProcess));
            outputDialog.setPreferredSize(new Dimension(400, 400));
            return outputDialog;
        }

        /**
         * Listener which terminates the model run.
         * 
         * @author Josh Lurz
         */
        private final class TerminateButtonListener implements ActionListener {
            /**
             * Method called when the button is clicked which will terminate the
             * model
             * 
             * @param aEvent
             *            The event received.
             */
            public void actionPerformed(final ActionEvent aEvent) {
                Logger.global
                        .log(Level.INFO,
                                "Terminate button pressed, attempting to stop the model.");
                // Set the model and the output watcher
                // to stop when there is a chance.
                mModelRunning = false;
                mProcess.destroy();
            }
        }

        /**
         * Appends a given output string to a text area contained in a scroll
         * pane. Scrolls the pane to keep the last line of output on the screen.
         * 
         * @author Josh Lurz
         */
        private final class ScreenPrinter implements Runnable {
            /**
             * The scroll pane to which the printer is adding output.
             */
            private transient final JScrollPane mPane;

            /**
             * The text area to which the printer is adding output.
             */
            private transient final JTextArea mArea;

            /**
             * The output the screen printer is adding.
             */
            private transient final String mOutput;

            /**
             * Constructor
             * 
             * @param aPane
             *            The scroll pane to which the printer is adding output.
             * @param aArea
             *            The text area to which the printer is adding output.
             * @param aOutput
             *            The output the screen printer is adding.
             */
            public ScreenPrinter(JScrollPane aPane, JTextArea aArea,
                    String aOutput) {
                super();
                mPane = aPane;
                mArea = aArea;
                mOutput = aOutput;
            }

            /**
             * Method called from the GUI thread to update the output area.
             */
            public void run() {
                // Add the output to the output area.
                mArea.append(mOutput);
                // Scroll the output window.
                // Get the bounds of the output area so we
                // can
                // determine its bottom.
                final Rectangle bottom = mArea.getBounds();

                // Get the viewable rectangle currently of
                // the scroll pane so that the viewable
                // window
                // doesn't adjust horizontally.
                final Rectangle currView = mPane.getViewportBorderBounds();

                // Create a rectangle to scroll to which is
                // located at the
                // bottom the output area in the same
                // horizontal position as it
                // was before the move. The last two
                // parameters, the width and height
                // of the rectangle, do not matter.
                final Rectangle lowerBound = new Rectangle(bottom.y
                        + bottom.height, currView.x, 1, 1);
                mPane.scrollRectToVisible(lowerBound);
            }
        }

        /**
         * Object which scrolls a JScrollPane when the user moves the mouse.
         * 
         * @author Josh Lurz
         * 
         */
        private final class ScrollPaneScroller extends MouseMotionAdapter {
            /**
             * Method called when the mouse is dragged.
             * 
             * @param aEvent
             *            The mouse event received. TODO: Move to interface
             *            utils.
             */
            @Override
            public void mouseDragged(final MouseEvent aEvent) {
                final Rectangle rect = new Rectangle(aEvent.getX(), aEvent
                        .getY(), 1, 1);
                ((JScrollPane) aEvent.getSource()).scrollRectToVisible(rect);
            }
        }
    }

    /**
     * A window listener which will stop the model executable and the output
     * watching thread at the first chance.
     * 
     * @author Josh Lurz
     */
    private final class OutputWindowCloseListener implements WindowListener {
        /**
         * The model process to close when the window is closed.
         */
        private final transient Process mProcess;

        /**
         * Constructor
         * 
         * @param aProcess
         *            Process to close when the window is closed.
         */
        public OutputWindowCloseListener(Process aProcess) {
            super();
            mProcess = aProcess;
        }

        /**
         * Method called when the output window is closing which ensures that
         * the model executable is stopped.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowClosing(final WindowEvent aEvent) {
            Logger.global.log(Level.INFO,
                    "Output window closed, attempting to stop the model.");
            // Stop the loop and from that the model
            // and output thread when there is a chance.
            mModelRunning = false;
            mProcess.destroy();

            // Close the window.
            ((JDialog) aEvent.getSource()).setVisible(false);
        }

        /**
         * Method called when a window is opened, implemented to do nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowOpened(final WindowEvent aEvent) {
            // Do nothing
        }

        /**
         * Method called when a window is closed, implemented to do nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowClosed(final WindowEvent aEvent) {
            // Do nothing
        }

        /**
         * Method called when a window is iconified, implemented to do nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowIconified(final WindowEvent aEvent) {
            // Do nothing
        }

        /**
         * Method called when a window is deiconified, implemented to do
         * nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowDeiconified(final WindowEvent aEvent) {
            // Do nothing
        }

        /**
         * Method called when a window is activated, implemented to do nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowActivated(final WindowEvent aEvent) {
            // Do nothing
        }

        /**
         * Method called when a window is deactivated, implemented to do
         * nothing.
         * 
         * @param aEvent
         *            The window event received.
         */
        public void windowDeactivated(final WindowEvent aEvent) {
            // Do nothing
        }
    }

}
