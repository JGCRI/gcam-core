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
