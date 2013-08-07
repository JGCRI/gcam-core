package ModelInterface;

import org.w3c.dom.Node;

/**
 * The interface for being able to run a batch file.
 * TODO: explain the batch file XML structure here.
 *
 * @author Pralit Patel
 */
public interface BatchRunner {
	/**
	 * Runs the batch command specified by the
	 * Node command.
	 * @param command The command to run.
	 */
	public void runBatch(Node command);
}
