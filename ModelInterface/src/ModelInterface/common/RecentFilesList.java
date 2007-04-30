package ModelInterface.common;

import java.lang.reflect.InvocationTargetException;

import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.util.Properties;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.io.File;

import ModelInterface.InterfaceMain;
import ModelInterface.InterfaceMain.MenuManager;
import ModelInterface.MenuAdder;

/**
 * A singleton class which will handle all aspects of listing recent files.
 * These aspects include collecting a list of recently Open/Save As files, 
 * updating the Recent Files menu, having them opened by the correct 
 * ActionListener, and use the Interface's properties to load/store the
 * recent files list.  An action listener should check the source of the
 * action since if it comes from a RecentFile it should not prompt for a
 * file to open.  Only finite number of recent files will be kept and
 * that number is determined by the RecentFilesLength property.
 *
 * @author Pralit Patel 
 */
public class RecentFilesList implements MenuAdder {
	/**
	 * Private instance of this class.
	 */
	private static final RecentFilesList instance = new RecentFilesList();

	/**
	 * The menu this class will add.  This menu will contain a
	 * menu item for each recent file available.
	 */
	private final JMenu recentFilesMenu = new JMenu("Open Recent Files");

	/**
	 * The number of recent files this list will keep track of.
	 */
	private int recentFilesLength;

	/**
	 * The max length a file name in the list can be.
	 */
	private static final int MAX_TITLE_LENGTH = 40;

	/**
	 * Private constructor.
	 */
	private RecentFilesList() {
		InterfaceMain.getInstance().addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control") 
					&& evt.getNewValue().equals("ModelInterface")) {
					doSetProperties();
				}
			}
		});
	}

	/**
	 * Get the instance of this class
	 * @return The instance of this class.
	 */
	public static RecentFilesList getInstance() {
		return instance;
	}

	public void addMenuItems(MenuManager menuMan) {
		// now is the time create the MenuItems for each recent file.
		Properties prop = InterfaceMain.getInstance().getProperties();
		try {
			String lengthStr =  prop.getProperty("RecentFilesLength", "5");
			prop.setProperty("RecentFilesLength", lengthStr);
			recentFilesLength = Integer.parseInt(lengthStr);
		} catch(NumberFormatException nfe) {
			nfe.printStackTrace();
			recentFilesLength = 5;
		}

		// should I add these through the menu manager?
		for(int i = 1; i <= recentFilesLength; ++i) {
			String filesStr = prop.getProperty("RecentFile"+i);
			String targetStr = prop.getProperty("RecentFileTarget"+i);

			// if we get back null that means that we have < RecentFilesLength
			// in the properties so just go ahead and stop now
			if(filesStr == null || targetStr == null) {
				break;
			}

			// files are seperated by semicolons
			String[] filesSplit = filesStr.split(";");
			// target will be stored targetclass;actionCommand
			String[] targetSplit = targetStr.split(";");
			if(filesSplit.length < 1 || targetSplit.length != 2) {
				continue;
			}
			File[] files = new File[filesSplit.length];
			for(int j = 0; j < files.length; ++j) {
				files[j] = new File(filesSplit[j]);
			}

			recentFilesMenu.add(new RecentFile(files, targetSplit[0], targetSplit[1]));
		}

		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).
			addMenuItem(recentFilesMenu, InterfaceMain.FILE_OPEN_SUBMENU_POS);
	}

	/**
	 * Sets the properties with the latest recent files.
	 */
	private void doSetProperties() {
		Properties prop = InterfaceMain.getInstance().getProperties();
		Component[] theFiles = recentFilesMenu.getMenuComponents();
		for(int i = 0; i < theFiles.length; ++i) {
			RecentFile f = (RecentFile)theFiles[i];
			prop.setProperty("RecentFile"+(i+1), f.getFilePaths());
			prop.setProperty("RecentFileTarget"+(i+1), f.getTargetName()+";"+f.getActionCommand());
		}
	}

	/**
	 * Called to notify that a file was opened or saved that could be re-opened and 
	 * thus will be added to the recent files list.  If the file is already in the
	 * list it will be moved up the list.  If we already have the max allowed
	 * recent files then the least recently opened will be removed.
	 * @param files The files opened.
	 * @param source The ActionListener will be responsible for opening the files.
	 * @param actionCommand The command for the event so it gets opened. 
	 */ 
	public void addFile(File[] files, ActionListener source, String actionCommand) {
		JMenuItem temp = new RecentFile(files, source, actionCommand);
		int pos;
		if((pos = doesMenuContain(temp)) != -1) {
			recentFilesMenu.remove(pos);
		} else if(recentFilesMenu.getItemCount() == recentFilesLength) {
			recentFilesMenu.remove(recentFilesLength-1);
		}
		recentFilesMenu.insert(temp, 0);
	}

	/**
	 * Looks at the subelements of the recent files list menu and 
	 * determines if it contains the passed in menu item.
	 * @param item The item which is being search for
	 * @return The position in which it was found, or -1 if not found
	 */
	private int doesMenuContain(JMenuItem item) {
		for(int i = 0; i < recentFilesMenu.getItemCount(); ++i) {
			if(recentFilesMenu.getItem(i).equals(item)) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Displays an error message dialog with a reason for
	 * why it was not able to open the recent file.
	 * @param reason A brief explanation of what went wrong.
	 */
	private static void showNoOpenError(String reason) {
		JOptionPane.showMessageDialog(InterfaceMain.getInstance(), reason, "Could Not Open File Error",
				JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * A menu item which represents a recent file.  It listen for it's own
	 * actions and pass it on the the appropriate ActionListener so that it
	 * will open the file. A RecentFile is immutable.
	 * @author Pralit Patel
	 */
	public class RecentFile extends JMenuItem implements ActionListener {
		/**
		 * The files that should be opened.
		 */
		private final File[] files;

		/**
		 * The class name of the target which will do
		 * the opening.
		 */
		private final String targetName;

		/**
		 * The action command to give when calling the
		 * actionPerformed
		 */
		private final String actionCommand;

		/**
		 * Constructs a RecentFile.  Used when reading creating from the properties
		 * since the targetName is already determined.
		 * @param files The files to open
		 * @param targetname The class name for that will do the opening.
		 * @param actionCommand The command to give the ActionEvent.
		 */
		public RecentFile(File[] files, String targetName, String actionCommand) {
			super(createTitle(actionCommand, files[0].getAbsolutePath()));
			this.files = files;
			this.targetName = targetName;
			this.actionCommand = actionCommand;
			addActionListener(this);
			setToolTipText(files[0].getAbsolutePath());
		}

		/**
		 * Constructs a RecentFile.  Used when a file was opened/saved.  It will
		 * have to determine the class name to use so that it would be able to 
		 * reopen the file again.
		 * @param files The files to open
		 * @param list The listener that could open this file again.
		 * @param actionCommand The command to give the ActionEvent.
		 */
		public RecentFile(File[] files, ActionListener list, String actionCommand) {
			super(actionCommand+": "+files[0].getName());
			this.files = files;
			this.targetName = list.getClass().getName();
			this.actionCommand = actionCommand;
			addActionListener(this);
			setToolTipText(files[0].getAbsolutePath());
		}

		public void actionPerformed(ActionEvent e) {
			// this would be a good time to make sure the files exist
			for(File file : files) {
				if(!file.exists()) {
					System.out.println("Does not exist anymore");
					showNoOpenError("The file no longer exists.");
					return;
				}
			}
			ActionEvent newE = new ActionEvent(this, e.getID(), actionCommand);
			try {
				MenuAdder target = InterfaceMain.getInstance().getMenuAdder(targetName);
				target.getClass().getMethod("actionPerformed", 
						ActionEvent.class).invoke(target, newE);
			} catch(NoSuchMethodException methodE) {
				showNoOpenError("Could not find target to open.");
				methodE.printStackTrace();
			} catch(IllegalAccessException accessE) {
				showNoOpenError("Could not find target to open.");
				accessE.printStackTrace();
			} catch(InvocationTargetException invokeE) {
				showNoOpenError("Could not find target to open.");
				invokeE.printStackTrace();
			} catch(NullPointerException nullE) {
				showNoOpenError("Could not find target to open.");
				nullE.printStackTrace();
			}
		}
		
		/**
		 * Get the files of this recent file.
		 * @return The files.
		 */
		public File[] getFiles() {
			return files;
		}

		/**
		 * Get the files as absolute file paths concatonated together
		 * with semicolons.
		 * @return The file paths ready to added to the properties.
		 */
		public String getFilePaths() {
			StringBuilder ret = new StringBuilder();
			for(File file : files) {
				ret.append(file.getAbsolutePath()).append(";");
			}
			return ret.toString();
		}

		/**
		 * Get the target name.
		 * @return Target class name.
		 */
		public String getTargetName() {
			return targetName;
		}

		/**
		 * Get the action command that will be used.
		 * @return The action command.
		 */
		public String getActionCommand() {
			return actionCommand;
		}

		public boolean equals(Object other) {
			if(other == null || !(other instanceof RecentFile)) {
				return false;
			} else {
				RecentFile o = (RecentFile)other;
				if(files.length != o.files.length) {
					return false;
				}
				for(int i = 0; i < files.length; ++i) {
					if(!files[i].equals(o.files[i])) {
						return false;
					}
				}
				return targetName.equals(o.targetName) && 
					actionCommand.equals(o.actionCommand);
			}
		}

		public int hashCode() {
			return files.hashCode() ^ targetName.hashCode() ^ actionCommand.hashCode();
		}
	}

	/**
	 * Create a title that is no longer that MAX_TITLE_LENGTH.  The rest will
	 * be abbreviated with a .. also the full file path may be viewed through
	 * the tool tip.
	 * @param actionCommand The action command used to open the file.
	 * @param fileName The file name to abbreviate.
	 * @return The title that is no longer than MAX_TITLE_LENGTH
	 */
	private static String createTitle(String actionCommand, String fileName) {
		// subtract off the actionCommand length and 4 for ": .."
		int maxFileLen = MAX_TITLE_LENGTH - actionCommand.length() - 4;
		if(fileName.length() <= maxFileLen) {
			return actionCommand+": "+fileName;
		} else {
			return actionCommand+": .."+fileName.substring(
					fileName.length() - maxFileLen -1, fileName.length());
		}
	}
}
