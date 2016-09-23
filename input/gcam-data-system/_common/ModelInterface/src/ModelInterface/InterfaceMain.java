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
package ModelInterface;

import java.util.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import javax.swing.JFrame;
import javax.swing.UIManager;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;

import javax.swing.undo.UndoManager;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CannotRedoException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionException;

import ModelInterface.ConfigurationEditor.configurationeditor.ConfigurationEditor;
import ModelInterface.DMsource.DMViewer;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.InputViewer;
import ModelInterface.PPsource.PPViewer;
import ModelInterface.common.RecentFilesList;

import ModelInterface.ModelGUI2.XMLFilter;
import ModelInterface.ConfigurationEditor.utils.FileUtils;
import ModelInterface.ConfigurationEditor.utils.DOMUtils;
import ModelInterface.common.FileChooser;
import ModelInterface.common.FileChooserFactory;
import ModelInterface.BatchRunner;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.KeyEvent;
import java.awt.Container;
import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.GraphicsEnvironment;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

public class InterfaceMain implements ActionListener {
	/**
	 * Unique identifier used for serializing.
	 */
	private static final long serialVersionUID = -9137748180688015902L;

	public static final int FILE_MENU_POS = 0;
	public static final int EDIT_MENU_POS = 1;
	public static final int HELP_MENU_POS = 100;
	public static final int FILE_NEW_MENUITEM_POS = 0;
	public static final int FILE_OPEN_SUBMENU_POS = 5;
	public static final int FILE_SAVE_MENUITEM_POS = 10;
	public static final int FILE_SAVEAS_MENUITEM_POS = 11;
	public static final int FILE_QUIT_MENUITEM_POS = 50;
	public static final int EDIT_COPY_MENUITEM_POS = 10;
	public static final int EDIT_PASTE_MENUITEM_POS = 11;
	public static final int EDIT_UNDO_MENUITEM_POS = 1;
	public static final int EDIT_REDO_MENUITEM_POS = 2;

	private static File propertiesFile = new File("model_interface.properties");
	private static String oldControl;
	private static InterfaceMain main;
	private JMenuItem newMenu;
	private JMenuItem saveMenu;
	private JMenuItem saveAsMenu;
	private JMenuItem quitMenu;
	private JMenuItem copyMenu;
	private JMenuItem pasteMenu;
	private JMenuItem undoMenu;
	private JMenuItem redoMenu;
	private JMenuItem batchMenu;
	private Properties savedProperties;

	private UndoManager undoManager;

	private List<MenuAdder> menuAdders;

    /**
     * The main GUI from the rest of the GUI components of the ModelInterface will rely on.
     */
    private JFrame mainFrame;

	/**
	 * Main function, creates a new thread for the gui and runs it.
	 */
	public static void main(String[] args) {

		Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
			public void uncaughtException(Thread t, Throwable e) {
                if(InterfaceMain.getInstance() != null ) {
                    InterfaceMain.getInstance().showMessageDialog(e, "Unexpected Error", 
                        JOptionPane.ERROR_MESSAGE);
                }
		// still print the stack trace to the console for debugging
		e.printStackTrace();
			}
		    });

		// -b <batch file> -l <log file>
		OptionParser parser = new OptionParser();
		parser.accepts("help", "print usage information").forHelp();
		parser.accepts("b", "XML batch file to process").withRequiredArg();
		parser.accepts("l", "log file into which to redirect ModelInterface output").withRequiredArg();
		
		OptionSet opts = null;
		try {
		    opts = parser.parse(args);
		} catch (OptionException e) {
		    System.err.println(e);
		    System.exit(1);
		}

		if (opts.has("help")) {
		    try {
			System.out.println("Usage: java -jar ModelInterface.jar -b <batch file> -l <log file>");
			parser.printHelpOn(System.out);
		    } catch (Exception e) {
			System.err.println("Failed to write usage message");
			System.exit(1);
		    }
		    System.exit(1);
		}

        // if the -l option is set then we will redirect standard output to the specified log file
        PrintStream stdout = System.out;
        if (opts.has("l")) {
            String logFile = (String) opts.valueOf("l");
            stdout.println("InterfaceMain: Directing stdout to " + logFile);
            try {
                FileOutputStream log = new FileOutputStream(logFile);
                System.setOut(new PrintStream(log));
            } catch (Exception e) {
                // If there was an error opening the log file we will post a message indicating as
                // much but continue on with out the redirect.
                System.err.println("Failed to open log file '" + logFile + "' for writing: " + e);
            }
        }

		if (opts.has("b")) {
		    String filename = (String) opts.valueOf("b");
		    System.out.println("InterfaceMain: batchFile: " + filename);

		    System.setProperty("java.awt.headless", "true");
		    System.out.println("Running headless? "+GraphicsEnvironment.isHeadless());
            Document batchDoc = filename.equals("-") ? DOMUtils.parseInputStream(System.in) : FileUtils.loadDocument(new File(filename), null);
		    main  = new InterfaceMain();

		    // Construct the subset of menu adders that are also BatchRunner while
		    // avoiding creating any GUI components
		    // TODO: avoid code duplication
		    final MenuAdder dbView = new DbViewer();
		    final MenuAdder inputView = new InputViewer();
		    main.menuAdders = new ArrayList<MenuAdder>(2);
		    main.menuAdders.add(dbView);
		    main.menuAdders.add(inputView);

		    // Run the batch file
            if(batchDoc != null) {
                main.runBatch(batchDoc.getDocumentElement());
            } else {
                System.out.println("Skipping batch "+filename+" due to parsing errors.");
            }
		    System.setOut(stdout);
		    return;
		}

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			// warn the user.. should be ok to keep going
			System.out.println("Error setting look and feel: " + e);
		}

		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
		});

	}

	/**
	 * Create a new instance of this class and makes it visible
	 */
	private static void createAndShowGUI() {
		main = null;
		main  = new InterfaceMain();
        main.mainFrame = new JFrame("Model Interface");
		if(Boolean.parseBoolean(main.savedProperties.getProperty("isMaximized", "false"))) {
			main.mainFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
		}
		String lastHeight = main.savedProperties.getProperty("lastHeight", "600");
		String lastWidth = main.savedProperties.getProperty("lastWidth", "800");
		main.mainFrame.setSize(Integer.parseInt(lastWidth), Integer.parseInt(lastHeight));

		main.mainFrame.setLayout(new BorderLayout());

		main.initialize();
		//main.pack();
		main.mainFrame.setVisible(true);
	}

	private InterfaceMain() {
        mainFrame = null;
		savedProperties = new Properties();
		if(propertiesFile.exists()) {
			try {
				savedProperties.loadFromXML(new FileInputStream(propertiesFile));
			} catch (FileNotFoundException notFound) {
				// well I checked if it existed before so..
			} catch (IOException ioe) {
				ioe.printStackTrace();
			}
		}
		oldControl = "ModelInterface";
	}

	private void initialize() {
		MenuManager menuMan = new MenuManager(null);
		addWindowAdapters();
		addMenuItems(menuMan);
		addMenuAdderMenuItems(menuMan);
		finalizeMenu(menuMan);
	}
    public JFrame getFrame() {
        return mainFrame;
    }    
	private void addMenuItems(MenuManager menuMan) {
		JMenu m = new JMenu("File");
		menuMan.addMenuItem(m, FILE_MENU_POS);
		JMenu submenu;
		JMenuItem menuItem;

		submenu = new JMenu("Open");
		submenu.setMnemonic(KeyEvent.VK_S);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(submenu, FILE_OPEN_SUBMENU_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_OPEN_SUBMENU_POS+2);
		//m.add(submenu);
		//m.addSeparator();

		//m.add(makeMenuItem("Quit"));
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(newMenu = new JMenuItem("New"), FILE_NEW_MENUITEM_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_NEW_MENUITEM_POS);
		newMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(saveMenu = new JMenuItem("Save")/*makeMenuItem("Save")*/, FILE_SAVE_MENUITEM_POS);
		saveMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(saveAsMenu = new JMenuItem("Save As"), FILE_SAVEAS_MENUITEM_POS);
		menuMan.getSubMenuManager(FILE_MENU_POS).addSeparator(FILE_SAVEAS_MENUITEM_POS);
		saveAsMenu.setEnabled(false);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(quitMenu = makeMenuItem("Quit"), FILE_QUIT_MENUITEM_POS);

		menuMan.addMenuItem(new JMenu("Edit"), EDIT_MENU_POS);

		copyMenu = new JMenuItem("Copy");
		// key stroke is system dependent
		//copyMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(copyMenu, EDIT_COPY_MENUITEM_POS);
		pasteMenu = new JMenuItem("Paste");
		// key stroke is system dependent
		//pasteMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(pasteMenu, EDIT_PASTE_MENUITEM_POS);
		menuMan.getSubMenuManager(EDIT_MENU_POS).addSeparator(EDIT_PASTE_MENUITEM_POS);

		copyMenu.setEnabled(false);
		pasteMenu.setEnabled(false);

		batchMenu = new JMenuItem("Batch File");
		batchMenu.setEnabled(true);
		batchMenu.addActionListener(this);
		menuMan.getSubMenuManager(FILE_MENU_POS).addMenuItem(batchMenu, FILE_OPEN_SUBMENU_POS);

		menuMan.addMenuItem(new JMenu("Help"), HELP_MENU_POS);

		setupUndo(menuMan);
	}

	private void addMenuAdderMenuItems(MenuManager menuMan) {
		/* FileChooserDemo is being removed, but I will leave this here,
		 * This is how I envision the menuitems to be added and hopefully all
		 * the listeners would be set up correctly and we won't need to keep
		 * the pointer to the classes around
		 FileChooserDemo fcd = new FileChooserDemo(this);
		 fcd.addMenuItems(menuMan);
		 */
		final MenuAdder dbView = new DbViewer();
		dbView.addMenuItems(menuMan);
		final MenuAdder inputView = new InputViewer();
		inputView.addMenuItems(menuMan);
		final MenuAdder PPView = new PPViewer();
		PPView.addMenuItems(menuMan);
		final MenuAdder DMView = new DMViewer();
		DMView.addMenuItems(menuMan);
		final MenuAdder recentFilesList = RecentFilesList.getInstance();
		recentFilesList.addMenuItems(menuMan);
		final MenuAdder aboutDialog = new AboutDialog();
		aboutDialog.addMenuItems(menuMan);

		// Create the Configuration editor and allow it to add its menu items to the
		// menu system.
		final MenuAdder confEditor = new ConfigurationEditor();
		confEditor.addMenuItems(menuMan);
		
		menuAdders = new ArrayList<MenuAdder>(6);
		menuAdders.add(dbView);
		menuAdders.add(inputView);
		menuAdders.add(PPView);
		menuAdders.add(DMView);
		menuAdders.add(recentFilesList);
		menuAdders.add(aboutDialog);
		menuAdders.add(confEditor);
	}

	private void finalizeMenu(MenuManager menuMan) {
		JMenuBar mb = menuMan.createMenu(); //new JMenuBar();
		mainFrame.setJMenuBar(mb);
	}

	private void addWindowAdapters() {
		// Add adapter to catch window events.
		WindowAdapter myWindowAdapter = new WindowAdapter() {
			public void windowStateChanged(WindowEvent e) {
				savedProperties.setProperty("isMaximized", String.valueOf((e.getNewState() & JFrame.MAXIMIZED_BOTH) != 0));
			}
			public void windowClosing(WindowEvent e) {
				System.out.println("Caught the window closing");
				fireProperty("Control", oldControl, "ModelInterface");
				if(!Boolean.parseBoolean(savedProperties.getProperty("isMaximized"))) {
					savedProperties.setProperty("lastWidth", String.valueOf(mainFrame.getWidth()));
					savedProperties.setProperty("lastHeight", String.valueOf(mainFrame.getHeight()));
				}
				try {
					savedProperties.storeToXML(new FileOutputStream(propertiesFile), "TODO: add comments");
				} catch(FileNotFoundException notFound) {
					notFound.printStackTrace();
				} catch (IOException ioe) {
					ioe.printStackTrace();
				}
				System.exit(0);
			}
			public void windowClosed(WindowEvent e) {
				System.out.println("Caught the window closed");
				fireProperty("Control", oldControl, "ModelInterface");
				if(!Boolean.parseBoolean(savedProperties.getProperty("isMaximized"))) {
					savedProperties.setProperty("lastWidth", String.valueOf(mainFrame.getWidth()));
					savedProperties.setProperty("lastHeight", String.valueOf(mainFrame.getHeight()));
				}
				try {
					savedProperties.storeToXML(new FileOutputStream(propertiesFile), "TODO: add comments");
				} catch(FileNotFoundException notFound) {
					notFound.printStackTrace();
				} catch (IOException ioe) {
					ioe.printStackTrace();
				}
				System.exit(0);
			}
		};
		mainFrame.addWindowListener(myWindowAdapter);
		mainFrame.addWindowStateListener(myWindowAdapter);

		mainFrame.getGlassPane().addMouseListener( new MouseAdapter() {});
		mainFrame.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	}

	private JMenuItem makeMenuItem(String title) {
		JMenuItem m = new JMenuItem(title);
		m.addActionListener(this);
		return m;
	}

	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("Quit")) {
			//fireProperty("Control", oldControl, "ModelInterface");
			mainFrame.dispose();
		} else if(e.getActionCommand().equals("Batch File")) {
			// TODO: make it so recent files could work with this
			FileChooser fc = FileChooserFactory.getFileChooser();
			final File[] result = fc.doFilePrompt(mainFrame, "Open Batch File", FileChooser.LOAD_DIALOG, 
					new File(getProperties().getProperty("lastDirectory", ".")),
					new XMLFilter());
            // these should be run off the GUI thread
            new Thread(new Runnable() {
                public void run() {
                    if(result != null) {
                        for(File file : result) {
                            Document doc = FileUtils.loadDocument(file, null);
                            // Only run if the batch file was parsed correctly
                            // note and error would have already been given if it wasn't
                            // parsed correctly
                            if(doc != null) {
                                runBatch(doc.getDocumentElement());
                            }
                        }
                    }
                    // TODO: message that all were run
                }
            }).start();
		}
	}
	public static InterfaceMain getInstance() {
		return main;
	}
	public JMenuItem getNewMenu() {
		return newMenu;
	}
	public JMenuItem getSaveMenu() {
		return saveMenu;
	}
	public JMenuItem getSaveAsMenu() {
		return saveAsMenu;
	}
	public JMenuItem getQuitMenu() {
		return quitMenu;
	}
	public JMenuItem getCopyMenu() {
		return copyMenu;
	}
	public JMenuItem getPasteMenu() {
		return pasteMenu;
	}
	public JMenuItem getUndoMenu() {
		// will this be needed since they will be setup in here?
		return undoMenu;
	}
	public JMenuItem getRedoMenu() {
		// will this be needed since they will be setup in here?
		return redoMenu;
	}
	public JMenuItem getBatchMenu() {
		return batchMenu;
	}
	public void fireControlChange(String newValue) {
		System.out.println("Going to change controls");
		if(newValue.equals(oldControl)) {
			oldControl += "Same";
		}
		fireProperty("Control", oldControl, newValue);
		oldControl = newValue;
	}
	public void fireProperty(String propertyName, Object oldValue, Object newValue) {
        final PropertyChangeEvent event = new PropertyChangeEvent(this, propertyName, oldValue, newValue);
        for(PropertyChangeListener listener : mainFrame.getPropertyChangeListeners()) {
            listener.propertyChange(event);
        }
	}
	public class MenuManager {
		private JMenuItem menuValue;
		private Map<Integer, MenuManager> subItems;
		private SortedSet<Integer> sepList;
		MenuManager(JMenuItem menuValue) {
			this.menuValue = menuValue;
			sepList = null;
			if(menuValue == null || menuValue instanceof JMenu) {
				subItems = new TreeMap<Integer, MenuManager>();
			} else {
				subItems = null;
			}
		}
		/*
		public JMenuItem getMenuValue() {
			return menuValue;
		}
		public Map getSubItems() {
			return subItems;
		}
		*/
		public void addSeparator(int where) {
			if(sepList == null) {
				sepList = new TreeSet<Integer>();
			}
			sepList.add(where);
		}
		public int addMenuItem(JMenuItem menu, int where) {
			if(subItems.containsKey(where)) {
				return addMenuItem(menu, where+1);
			} else {
				subItems.put(where, new MenuManager(menu));
				return where;
			}
		}
		public MenuManager getSubMenuManager(int where) {
			if(!subItems.containsKey(where)) {
				// throw exception or just return null?
				return null;
			}
			return ((MenuManager)subItems.get(where));
		}
		JMenuBar createMenu() {
			JMenuBar ret = new JMenuBar();
			Object[] keys = subItems.keySet().toArray();
			for(int i = 0; i < keys.length; ++i) {
				ret.add(((MenuManager)subItems.get(keys[i])).createSubMenu());
			}
			return ret;
		}
		private JMenuItem createSubMenu() {
			if(subItems == null) {
				return menuValue;
			} else {
				Object[] keys = subItems.keySet().toArray();
				for(int i = 0; i < keys.length; ++i) {
					if(sepList != null && !sepList.isEmpty() &&
							((Integer)keys[i]).intValue() > ((Integer)sepList.first()).intValue()) {
						((JMenu)menuValue).addSeparator();
						sepList.remove(sepList.first());
					}
					menuValue.add(((MenuManager)subItems.get(keys[i])).createSubMenu());
				}
				return menuValue;
			}
		}
	}
	public Properties getProperties() {
		return savedProperties;
	}
	private void setupUndo(MenuManager menuMan) {
		undoManager = new UndoManager();
		undoManager.setLimit(10);

		undoMenu = new JMenuItem("Undo");
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(undoMenu, EDIT_UNDO_MENUITEM_POS);
		redoMenu = new JMenuItem("Redo");
		menuMan.getSubMenuManager(EDIT_MENU_POS).addMenuItem(redoMenu, EDIT_REDO_MENUITEM_POS);
		menuMan.getSubMenuManager(EDIT_MENU_POS).addSeparator(EDIT_REDO_MENUITEM_POS);

		undoMenu.setEnabled(false);
		redoMenu.setEnabled(false);

		ActionListener undoListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String cmd = e.getActionCommand();
				if(cmd.startsWith("Undo")) {
					try {
						undoManager.undo();
						refreshUndoRedo();
					} catch(CannotUndoException cue) {
						cue.printStackTrace();
					}
				} else if(cmd.startsWith("Redo")) {
					try {
						undoManager.redo();
						refreshUndoRedo();
					} catch(CannotRedoException cre) {
						cre.printStackTrace();
					}
				} else {
					System.out.println("Didn't recognize: "+cmd);
				}
			}
		};

		undoMenu.addActionListener(undoListener);
		redoMenu.addActionListener(undoListener);
	}
	public UndoManager getUndoManager() {
		return undoManager;
	}
	public void refreshUndoRedo() {
		undoMenu.setText(undoManager.getUndoPresentationName());
		undoMenu.setEnabled(undoManager.canUndo());
		redoMenu.setText(undoManager.getRedoPresentationName());
		redoMenu.setEnabled(undoManager.canRedo());
	}
	/**
	 * Get the menu adder with the specified class name.  Used
	 * to get the instance of the menu adder that could open
	 * a recent file.
	 * @param classname The class that is requested.
	 * @return The instance of the class or null if not found.
	 */ 
	public MenuAdder getMenuAdder(String classname) {
		for(Iterator<MenuAdder> it = menuAdders.iterator(); it.hasNext(); ) {
			MenuAdder curr = it.next();
			if(curr.getClass().getName().equals(classname)) {
				return curr;
			}
		}
		return null;
	}
	/**
	 * Runs the given batch file.  Relys on the menuAdders list
	 * and if any of the class implements BatchRunner it will pass
	 * it off the command to that class. 
	 * @param doc The batch file parsed into a DOM document which contains
     *            the commands to run.
	 * @see BatchRunner 
	 */
	private void runBatch(Node doc) {
		// TODO: remove this check once batch queries get merged
		if(doc.getNodeName().equals("queries")) {
			System.out.println("Batch queries are not yet merged with this functionality.");
			System.out.println("Please open a database then run the batch file.");
			// TODO: print this on the screen
			return;
		}

		NodeList commands = doc.getChildNodes();
		for(int i = 0; i < commands.getLength(); ++i) {
			if(commands.item(i).getNodeName().equals("class")) {
				Element currClass = (Element)commands.item(i);
				String className = currClass.getAttribute("name");
				MenuAdder runner = getMenuAdder(className);
				if(runner != null && runner instanceof BatchRunner) {
					((BatchRunner)runner).runBatch(currClass);
				} else {
					showMessageDialog(
							"Could not find batch runner for class "+className,
							"Batch File Error", JOptionPane.ERROR_MESSAGE);
				}
			}
		}
		showMessageDialog(
				"Finished running batch file",
				"Batch File Complete", JOptionPane.INFORMATION_MESSAGE);
	}
    /**
     * Convert JOptionPane message types to string so that they can be
     * logged to the console.
     * @param messageType The JOptionPane message type.
     * @return A string representing the meaning of messageType.
     */
    private static String convertMessageTypeToString(int messageType) {
        switch(messageType) {
            case JOptionPane.ERROR_MESSAGE:
                return "ERROR";
            case JOptionPane.INFORMATION_MESSAGE:
                return "INFO";
            case JOptionPane.PLAIN_MESSAGE:
                return "PLAIN";
            case JOptionPane.QUESTION_MESSAGE:
                return "QUESTION";
            case JOptionPane.WARNING_MESSAGE:
                return "WARNING";
            default:
                return "UNKNOWN";
        }
    }

    /**
     * Wrapper for JOptionPane.showMessageDialog which checks if we are running
     * headless.  If we are running headless a message is just written to stdout
     * instead of popping up on screen.
     * @param message The message to show.
     * @param title The title of the dialog.
     * @param messageType The message type.
     */
    public void showMessageDialog(Object message, String title, int messageType) {
        if(GraphicsEnvironment.isHeadless()) {
            // Convert the message dialog to a console log
            System.out.print(convertMessageTypeToString(messageType));
            System.out.print("; ");
            System.out.println(message);
        } else {
            // Just forward to JOptionPane
            JOptionPane.showMessageDialog(mainFrame, message, title, messageType);
        }
    }

    /**
     * Convert JOptionPane option types to string so that they can be
     * logged to the console.
     * @param optionType The JOptionPane option type.
     * @return A string representing the meaning of optionType.
     */
    private static String convertOptionTypeToString(int optionType) {
        switch(optionType) {
            case JOptionPane.CANCEL_OPTION:
                return "CANCEL";
            case JOptionPane.CLOSED_OPTION:
                return "CLOSED";
            case JOptionPane.NO_OPTION:
                return "NO";
            case JOptionPane.YES_OPTION:
                return "YES";
            default:
                return "UNKNOWN";
        }
    }

    /**
     * Wrapper for JOptionPane.showConfirmDialog which checks if we are running
     * headless.  If we are running headless a message is just written to stdout
     * instead of popping up on screen and the defaultOption will be selected.
     * @param message The message to show.
     * @param title The title of the dialog.
     * @param optionType The option types to choose from.
     * @param messageType The message type.
     * @param defaultOption The default option to choose when running headless.
     * @return The option chosen.
     */
    public int showConfirmDialog(Object message, String title, int optionType, int messageType, int defaultOption) {
        if(GraphicsEnvironment.isHeadless()) {
            // Convert the message dialog to a console log
            System.out.print("YES/NO/CANCEL");
            System.out.print("; ");
            System.out.print(message);
            System.out.print("; ");
            System.out.println(convertOptionTypeToString(defaultOption));
            return defaultOption;
        } else {
            // Just forward to JOptionPane
            return JOptionPane.showConfirmDialog(mainFrame, message, title, optionType, messageType);
        }
    }
}
