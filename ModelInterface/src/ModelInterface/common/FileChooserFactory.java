package ModelInterface.common;

/**
 * A factory class which will be used to retrieve
 * the appropriate file chooser for the platform.
 * @author Pralit Patel
 */ 
public class FileChooserFactory {
	/**
	 * Private instance of this class
	 */
	private static FileChooserFactory thisFactory = new FileChooserFactory();

	/**
	 * Flag to decide which file chooser to use.
	 * TODO: maybe I should use a real flag if there could
	 * be more types of FileChoosers 
	 */
	private boolean useSwing;

	/**
	 * Private constructor.  This is a singleton class, so make
	 * the constructor private that way there won't be multiple 
	 * instances floating around.
	 */
	private FileChooserFactory() {
		useSwing = !System.getProperty("os.name").toLowerCase().startsWith("mac");
	}

	/**
	 * Get the file choose this app should use.
	 * @param useSwingIn Whether to use the JFileChooser Demo, or AWT. 
	 * @return A FileChooser.
	 */
	public static FileChooser getFileChooser(boolean useSwingIn) {
		if(useSwingIn) {
			return new JFileChooserWrapper();
		} else {
			return new AWTFileChooserWrapper();
		}
	}

	/**
	 * Get the file choose this app should use.  The version will use the default
	 * for the OS.
	 * @return A FileChooser.
	 */
	public static FileChooser getFileChooser() {
		return getFileChooser(thisFactory.useSwing);
	}
}
