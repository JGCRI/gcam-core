package ModelGUI2;

import org.w3c.dom.*;
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.apache.xpath.domapi.*;
import org.jfree.chart.JFreeChart;
import org.w3c.dom.xpath.*;
import javax.swing.event.*;
import javax.swing.tree.TreeSelectionModel;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.*;
import javax.swing.tree.TreePath;

public class FileChooserDemo extends JFrame implements ActionListener,
		TableModelListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private JFrame thisFrame;

	private Document doc;

	private LSInput lsInput;

	private LSParser lsParser;

	private DOMImplementationLS implls;

	int lastFlipX = 0;

	int lastFlipY = 0;

	JMenuItem menuOpenX = null;

	JMenuItem menuOpenC = null;

	JMenuItem menuSave = null;

	JMenuItem menuClose = null;

	JMenuItem menuTableFilter = null;

	JMenuItem menuTableAdd = null;

	protected JMenuItem copyMenu = null;

	protected JMenuItem pasteMenu = null;

	JSplitPane splitPane;

	JLabel infoLabel;

	JTextField nameField;

	JTextField attribField;

	JTextField valueField;

	Document lastDoc;

	JTextArea textArea;

	JTree jtree;

	JTable jTable;

	private JPopupMenu treeMenu;

	private TreePath selectedPath;

	private JDialog addChildDialog;

	private JPopupMenu tableMenu; // for new 'flip' right click option
	
	private JFrame chartWindow = null;
	
	//private JLabel labelChart = null;
	
	XMLFilter xmlFilter = new XMLFilter();

	CSVFilter csvFilter = new CSVFilter();

	File file;

	File recentFile = new File("recent.xml");

	JFileChooser globalFC; // for saving last current directory

	// removed static final because they will change now
	int windowHeight = 460;

	int leftWidth = 300;

	//static final int rightWidth = 340;

	int windowWidth = 640;

	static String[] names = { "Single Table", "Multi Tables", "Combo Tables" };

	static JFrame frame; // goes with radio buttons

	/**
	 * Main function, creates a new thread for the gui and runs it.
	 */
	public static void main(String[] args) {

		//Schedule a job for the event-dispatching thread:
		//creating and showing this application's GUI.

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
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
	public static void createAndShowGUI() {
		FileChooserDemo f = new FileChooserDemo("ModelGUI");
		//f.pack();
		f.setVisible(true);
	}

	/**
	 * Create a frame, add menu items, and initialize the DOM stuff. Looks for
	 * the recent.xml file to find previous settings, currently only the
	 * previous directory, and sets that as the current directory to look for
	 * files. Also adds window listeners so we can be notified of clicks on menu
	 * items.
	 * 
	 * @param title
	 *            The title of this frame
	 */
	FileChooserDemo(String title) {
		super(title);
		thisFrame = this;

		globalFC = new JFileChooser();
		globalFC.setCurrentDirectory( new File(".") );

		try {
			System.setProperty(DOMImplementationRegistry.PROPERTY,
					"org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry reg = DOMImplementationRegistry
					.newInstance();
			DOMImplementation impl = reg.getDOMImplementation("XML 3.0");
			if (impl == null) {
				System.out
						.println("Could not find a DOM3 Load-Save compliant parser.");
				JOptionPane.showMessageDialog(this,
						"Could not find a DOM3 Load-Save compliant parser.",
						"Initialization Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			implls = (DOMImplementationLS) impl;
			lsInput = implls.createLSInput();
			DocumentType DOCTYPE = impl.createDocumentType("recent", "", "");
			lastDoc = impl.createDocument("", "recent", DOCTYPE);
		} catch (Exception e) {
			System.err.println("Couldn't initialize DOMImplementation: " + e);
			JOptionPane.showMessageDialog(this,
					"Couldn't initialize DOMImplementation\n" + e,
					"Initialization Error", JOptionPane.ERROR_MESSAGE);
		}

		try {
			lsInput.setByteStream(new FileInputStream(recentFile));
			lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
			lastDoc = lsParser.parse(lsInput);

			XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(lastDoc);
			XPathResult res = (XPathResult) xpeImpl.createExpression(
					"//recent/lastDirectory/node()",
					xpeImpl.createNSResolver(lastDoc.getDocumentElement()))
					.evaluate(lastDoc.getDocumentElement(),
							XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

			Node tempNode;
			while ((tempNode = res.iterateNext()) != null) {
				String pathDirectory = tempNode.getNodeValue();
				globalFC.setCurrentDirectory(new File(pathDirectory));
			}

			res = (XPathResult)xpeImpl.createExpression("//recent/lastHeight/node()", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

			if( (tempNode = res.iterateNext()) ==  null ){
				throw new Exception();
			} else {
				windowHeight = Integer.parseInt( tempNode.getNodeValue() );
			}
			/*
			   while( (tempNode = res.iterateNext()) != null){
			   windowHeight = Integer.parseInt( tempNode.getNodeValue() );
			   }
			   */

			res = (XPathResult)xpeImpl.createExpression("//recent/lastWidth", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

			while( (tempNode = res.iterateNext()) != null) {
				if( ((Element)tempNode).getAttribute( "pane" ).equals( "leftPane" )) {
					leftWidth = Integer.parseInt( tempNode.getFirstChild().getNodeValue() );
				} else if( ((Element)tempNode).getAttribute( "pane" ).equals( "totalPane" )) {
					windowWidth = Integer.parseInt( tempNode.getFirstChild().getNodeValue() );
				}
			}

		} catch (java.io.FileNotFoundException fe) {

			System.out.println("File exception " + fe);
			System.out.println("so that hopefully means no file, so i'll just use default");

			Element aNode = lastDoc.createElement("lastDirectory");
			aNode.appendChild( lastDoc.createTextNode(globalFC.getCurrentDirectory().toString()) );
			lastDoc.getDocumentElement().appendChild(aNode);

			aNode = lastDoc.createElement( "lastHeight" );
			aNode.appendChild( lastDoc.createTextNode( "460" ));
			lastDoc.getDocumentElement().appendChild(aNode);

			aNode = lastDoc.createElement( "lastWidth" );
			aNode.appendChild( lastDoc.createTextNode( "300" ));
			aNode.setAttribute( "pane", "leftPane" );
			lastDoc.getDocumentElement().appendChild(aNode);

			aNode = lastDoc.createElement( "lastWidth" );
			aNode.appendChild( lastDoc.createTextNode( "640" ));
			aNode.setAttribute( "pane", "totalPane" );
			lastDoc.getDocumentElement().appendChild(aNode);
		} catch (Exception e) {

			// because heights and widths were added after need to check for old version of
			// recent.xml then update it to include heights and widths

			System.out.println("exception " + e);
			System.out.println("so that hopefully means no h and w, so i'll just use default");

			Element aNode = lastDoc.createElement( "lastHeight" );
			aNode.appendChild( lastDoc.createTextNode( "460" ));
			lastDoc.getDocumentElement().appendChild(aNode);

			aNode = lastDoc.createElement( "lastWidth" );
			aNode.appendChild( lastDoc.createTextNode( "300" ));
			aNode.setAttribute( "pane", "leftPane" );
			lastDoc.getDocumentElement().appendChild(aNode);

			aNode = lastDoc.createElement( "lastWidth" );
			aNode.appendChild( lastDoc.createTextNode( "640" ));
			aNode.setAttribute( "pane", "totalPane" );
			lastDoc.getDocumentElement().appendChild(aNode);
		}

		Container contentPane = getContentPane();

		// Create a user interface.
		contentPane.setLayout(new BorderLayout());

		// Use the helper method makeMenuItem
		// for making the menu items and registering
		// their listener.
		JMenu m = new JMenu("File");
		JMenu submenu;
		JMenuItem menuItem;

		//m.add(menuOpenX = makeMenuItem("Open"));
		//m.add(menuOpenC = makeMenuItem("Open"));

		//a submenu
		m.addSeparator();
		submenu = new JMenu("Open ...");
		submenu.setMnemonic(KeyEvent.VK_S);

		menuItem = new JMenuItem("XML file");
		menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2,
				ActionEvent.ALT_MASK));
		menuItem.addActionListener(this);
		submenu.add(menuItem);

		menuItem = new JMenuItem("CSV file");
		menuItem.addActionListener(this);
		submenu.add(menuItem);
		m.add(submenu);

		m.add(menuSave = makeMenuItem("Save"));
		menuSave.setEnabled(false); // save will first be gray since no file
									// open
		m.add(menuClose = makeMenuItem("Quit"));

		JMenu editMenu = new JMenu("Edit");
		copyMenu = new JMenuItem("Copy");
		copyMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
				ActionEvent.CTRL_MASK));
		editMenu.add(copyMenu);
		pasteMenu = new JMenuItem("Paste");
		pasteMenu.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
				ActionEvent.CTRL_MASK));
		editMenu.add(pasteMenu);

		copyMenu.setEnabled(false);
		pasteMenu.setEnabled(false);

		JMenu tableMenu = new JMenu("Table");
		tableMenu.add(menuTableFilter = makeMenuItem("Filter"));

		// nothing for this is implemented, just something i figured should
		// happen
		tableMenu.add(menuTableAdd = makeMenuItem("Add Data"));

		menuTableFilter.setEnabled(false);
		menuTableAdd.setEnabled(false);

		JMenuBar mb = new JMenuBar();
		mb.add(m);
		mb.add(editMenu);
		mb.add(tableMenu);

		setJMenuBar(mb);
		setSize(windowWidth, windowHeight);
		
		// Create a window to display the chart in.
		chartWindow = new JFrame();
		chartWindow.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				//chartWindow.getContentPane().removeAll();
			}
		});
		
		// Create a label for the chart.
		//labelChart = new JLabel();
		
		// Add the label containing the chart to the window.
		//chartWindow.getContentPane().add( labelChart );
		//chartWindow.add( labelChart );
		// Add adapter to catch window closing event.
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				dispose();
				updateRecentDoc();
				writeFile(recentFile, lastDoc); // NEWLY ADDED !!!!!!!!!!!
				System.exit(0);
			}
		});

	}

	/**
	 * Creates a new JTree with the current doc, then sets up a splitPane to
	 * hold the JTree on the left, and and empty pane for future tables of the
	 * right. Also creates the listener for the JTree so right click options can
	 * be handled.
	 */
	public void displayJtree() {
		Container contentPane = getContentPane();
		if (splitPane != null) {
			contentPane.remove(splitPane);
		}
		// Set up the tree
		jtree = new JTree(new DOMmodel(doc.getDocumentElement()));
		jtree.setEditable(true);
		jtree.getSelectionModel().setSelectionMode(
				TreeSelectionModel.SINGLE_TREE_SELECTION);
		jtree.setShowsRootHandles(true);
		jtree.getModel().addTreeModelListener(new MyTreeModelListener());

		//listen for right click on the tree
		jtree.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				maybeShowPopup(e);
			}

			public void mouseReleased(MouseEvent e) {
				maybeShowPopup(e);
			}

			private void maybeShowPopup(MouseEvent e) {
				if (e.isPopupTrigger()) {
					selectedPath = jtree.getClosestPathForLocation(e.getX(), e
							.getY());
					jtree.setSelectionPath(selectedPath);
					MenuElement[] me = treeMenu.getSubElements();
					// Display Table is only availabe on elements that contain
					// text data
					// Add Child is available on non text nodes
					for (int i = 0; i < me.length; i++) {
						if (((JMenuItem) me[i]).getText().equals(
								"Display Table")) {
							Node currentNode = ((DOMmodel.DOMNodeAdapter) jtree
									.getLastSelectedPathComponent()).getNode();
							if (currentNode.getFirstChild() != null
									&& currentNode.getFirstChild()
											.getNodeType() == Element.TEXT_NODE) {
								((JMenuItem) me[i]).setEnabled(true);
							} else {
								((JMenuItem) me[i]).setEnabled(false);
							}
							/*
							 * if
							 * (jtree.getModel().isLeaf(jtree.getLastSelectedPathComponent())) {
							 * ((JMenuItem)me[i]).setEnabled(false); } else {
							 * ((JMenuItem)me[i]).setEnabled(true); }
							 */
						}
						if (((JMenuItem) me[i]).getText().equals("Add Child")) {
							Node nodeClicked = ((DOMmodel.DOMNodeAdapter) jtree
									.getLastSelectedPathComponent()).getNode();

							if (nodeClicked.getNodeType() == Element.TEXT_NODE) {
								((JMenuItem) me[i]).setEnabled(false);
							} else {
								((JMenuItem) me[i]).setEnabled(true);
							}

						}
					}
					treeMenu.show(e.getComponent(), e.getX(), e.getY());
				}
			}
		});
		//******

		// Build left-side view
		JScrollPane treeView = new JScrollPane(jtree);
		treeView.setPreferredSize(new Dimension(leftWidth, windowHeight));

		//jTable = new JTable();
		JScrollPane tableView = new JScrollPane(/* jTable */);
		//tableView.setPreferredScrollableViewportSize( new Dimension (
		// rightWidth, windowHeight ));
		tableView.setPreferredSize(new Dimension(windowWidth - leftWidth, windowHeight));

		// Build split-pane view
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, treeView,
				tableView);
		splitPane.setContinuousLayout(true);
		splitPane.setDividerLocation(leftWidth);
		splitPane.setPreferredSize(new Dimension(windowWidth + 10, windowHeight + 10));
		// Add GUI components
		contentPane.add("Center", splitPane);

		treeMenu = makePopupTreeMenu();

		//create the dialog for adding new node children, but leave invisible
		makeAddChildDialog();

		//this.show();
		//this.pack();
		this.setVisible(true);
	}

	/**
	 * Process events from the menu items.
	 * 
	 * @param e
	 *            the event, only care about a click on a menu item
	 */
	public void actionPerformed(ActionEvent e) {
		boolean status = false;
		String command = e.getActionCommand();
		if (command.equals("XML file")) {
			// Open a file
			status = openXMLFile();
			if (!status) {
				JOptionPane.showMessageDialog(null, "Error opening file!",
						"File Open Error", JOptionPane.ERROR_MESSAGE);
			}
			if (doc == null) {
				//probably the cancell, just return here to avoid exceptions
				return;
			}
			displayJtree();
			menuSave.setEnabled(true); // now save can show up

		} else if (command.equals("CSV file")) {
			// Open a file
			status = openCSVFile();
			if (!status) {
				JOptionPane.showMessageDialog(null, "Error opening file!",
						"File Open Error", JOptionPane.ERROR_MESSAGE);
			}
			if (doc == null) {
				//probably the cancell, just return here to avoid exceptions
				return;
			}
			displayJtree();
			menuSave.setEnabled(true); // now save can show up
		} else if (command.equals("Save")) {
			// Save a file
			status = saveFile();
			if (!status) {
				JOptionPane.showMessageDialog(null,
						"IO error in saving file!!", "File Save Error",
						JOptionPane.ERROR_MESSAGE);
			}
			//remember the cancel case if stuff is added here in the future
		} else if (command.equals("Filter")) {
			try {
				if (((JTable) ((JScrollPane) splitPane.getRightComponent())
						.getViewport().getView()).getModel() instanceof TableSorter) {

					((BaseTableModel) ((TableSorter) ((JTable) ((JScrollPane) splitPane
							.getRightComponent()).getViewport().getView())
							.getModel()).getTableModel()).filterData(this);
				} else {
					((BaseTableModel) ((JTable) ((JScrollPane) splitPane
							.getRightComponent()).getViewport().getView())
							.getModel()).filterData(this);
					if (((JTable) ((JScrollPane) splitPane.getRightComponent())
							.getViewport().getView()).getModel() instanceof MultiTableModel) {
						// NOT THE BEST WAY TO SET ROW HEIGHT
						int j = 1;
						JTable jTable = (JTable) ((JScrollPane) splitPane
								.getRightComponent()).getViewport().getView();
						while (j < jTable.getRowCount()) {
							jTable.setRowHeight(j - 1, 16);
							jTable.setRowHeight(j, 200);
							j += 2;
						}
					}
				}
			} catch (UnsupportedOperationException uoe) {
				JOptionPane.showMessageDialog(null,
						"This table does not support filtering",
						"Table Filter Error", JOptionPane.ERROR_MESSAGE);
			}
		} else if (command.equals("Quit")) {
			dispose();
			updateRecentDoc();
			writeFile(recentFile, lastDoc); // NEWLY ADDED !!!!!!!!!!!
		}
	}

	/**
	 * Updates recentDoc with most current globalFC and last Height and Widths
	 */ 
	public void updateRecentDoc(){ 
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(lastDoc);
		XPathResult res = (XPathResult)xpeImpl.createExpression("//recent/lastDirectory/node()", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

		Node tempNode;
		while( (tempNode = res.iterateNext()) != null ){
			tempNode.setNodeValue( globalFC.getCurrentDirectory().toString() );
		}

		res = (XPathResult)xpeImpl.createExpression("//recent/lastHeight/node()", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

		tempNode = res.iterateNext();
		tempNode.setNodeValue( (new Integer( this.getHeight() )).toString() );

		res = (XPathResult)xpeImpl.createExpression("//recent/lastWidth", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);

		while( (tempNode = res.iterateNext()) != null) {
			if( ((Element)tempNode).getAttribute( "pane" ).equals( "leftPane" )) {
				if( splitPane != null ) {
					leftWidth = splitPane.getDividerLocation();
				}
				tempNode.getFirstChild().setNodeValue( (new Integer( leftWidth )).toString() );
			} else if( ((Element)tempNode).getAttribute( "pane" ).equals( "totalPane" )) {
				tempNode.getFirstChild().setNodeValue( (new Integer( this.getWidth() )).toString() );
			}
		}
	}

	/**
	 * This "helper method" makes a menu item and then registers this object as
	 * a listener to it.
	 * 
	 * @param name
	 *            Name of menu item
	 * @return a new menu item with specified name
	 */
	private JMenuItem makeMenuItem(String name) {
		JMenuItem m = new JMenuItem(name);
		m.addActionListener(this);
		return m;
	}

	/**
	 * Create right click menu for the JTree and returns it, also defines the
	 * listeners for each button
	 * 
	 * @return the right click menu for the JTree
	 */
	private JPopupMenu makePopupTreeMenu() {
		treeMenu = new JPopupMenu();
		JMenuItem menuItem = new JMenuItem("Display Table");
		menuItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
				if (!jtree.getModel().isLeaf(
						jtree.getLastSelectedPathComponent())) {

					// find out the type of table and create it
					RadioButton.showDialog(/* frame */thisFrame, null, "",
							"Choose Table Viewing Type", names, "");
					JScrollPane tableView = RadioButton.createSelection(
							selectedPath, doc, thisFrame);

					if (tableView == null) {
						return;
					}

					// maybe this will solve the resizing of the left component
					//tableView.setPreferredSize(new Dimension(windowWidth - leftWidth, windowHeight));

					// don't know why but it is moving the divider all of a sudden, so will
					// force it to be where it was
					int divLoc = splitPane.getDividerLocation();
					splitPane.setRightComponent(tableView);
					splitPane.setDividerLocation(divLoc);
					tableMenu = makePopupTableMenu();
					// add the listener for right click which currently only
					// handles flip
					((JTable) tableView.getViewport().getView())
							.addMouseListener(new MouseAdapter() {
								public void mousePressed(MouseEvent e) {
									maybeShowPopup(e);
								}

								public void mouseReleased(MouseEvent e) {
									maybeShowPopup(e);
								}

								private void maybeShowPopup(MouseEvent e) {
									if (e.isPopupTrigger()) {
										//selectedPath =
										// jtree.getClosestPathForLocation(e.getX(),
										// e.getY());
										MenuElement[] me = tableMenu
												.getSubElements();
										for (int i = 0; i < me.length; i++) {
											if (((JMenuItem) me[i]).getText()
													.equals("Flip")) {
												lastFlipX = e.getX();
												lastFlipY = e.getY();
											}
										}
										tableMenu.show(e.getComponent(), e
												.getX(), e.getY());
									}
								}
							});
				}

			}

			public void mouseClicked(MouseEvent e) {
				//shouldn't the action go here
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}
		});
		treeMenu.add(menuItem);
		treeMenu.add(new JSeparator());
		menuItem = new JMenuItem("Add Child");
		menuItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
				jtree.setSelectionPath(selectedPath);

				Node nodeClicked = ((DOMmodel.DOMNodeAdapter) jtree
						.getLastSelectedPathComponent()).getNode();
				if (nodeClicked.getNodeType() != Element.TEXT_NODE) { // can't
																	  // add
																	  // child
																	  // to text
																	  // node
					showAddChildDialog();
				}

			}

			public void mouseClicked(MouseEvent e) {
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}
		});
		treeMenu.add(menuItem);

		treeMenu.add(new JSeparator());
		menuItem = new JMenuItem("Delete Node");
		menuItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
				jtree.setSelectionPath(selectedPath);
				deleteNode();
			}

			public void mouseClicked(MouseEvent e) {
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}
		});
		treeMenu.add(menuItem);

		return treeMenu;
	}

	/**
	 * Creates the right click menu for tables which currently consists of flip,
	 * also creates the listener
	 * 
	 * @return the right click menu created
	 */
	private JPopupMenu makePopupTableMenu() {
		tableMenu = new JPopupMenu();
		final JMenuItem flipItem = new JMenuItem("Flip");
		flipItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
					// get the correct row and col which only matters for
					// mulitablemodel
					// then be sure to pass on the call to the correct table
					// model
					e.translatePoint(lastFlipX, lastFlipY);
					Point p = e.getPoint();
					JTable jTable = (JTable) ((JScrollPane) splitPane
							.getRightComponent()).getViewport().getView();
					int row = jTable.rowAtPoint(p);
					int col = jTable.columnAtPoint(p);

					if (jTable.getModel() instanceof TableSorter) {
						((BaseTableModel) ((TableSorter) jTable.getModel())
								.getTableModel()).flip(row, col);
					} else {
						((BaseTableModel) jTable.getModel()).flip(row, col);
					}
			}
			public void mouseClicked(MouseEvent e) {
				//shouldn't the action go here
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}
		});
		tableMenu.add(flipItem);
		
		final JMenuItem chartItem = new JMenuItem("Chart");
		chartItem.addMouseListener( new MouseListener(){
			public void mouseReleased(MouseEvent e) {
				JTable jTable = (JTable) ((JScrollPane) splitPane
						.getRightComponent()).getViewport().getView();
				JFreeChart chart;
				if( jTable.getModel() instanceof TableSorter) {
					chart = ((BaseTableModel) ((TableSorter)jTable.getModel()).getTableModel()).createChart();
				} else {
				chart = ((BaseTableModel) jTable.getModel())
						.createChart();
				}
				// Turn the chart into an image.
				BufferedImage chartImage = chart.createBufferedImage(500, 500);
				
				JLabel labelChart = new JLabel();
				labelChart.setIcon(new ImageIcon(chartImage));
				chartWindow.getContentPane().add(labelChart);
				chartWindow.pack();
				chartWindow.setVisible(true);
			}
			public void mouseClicked(MouseEvent e) {
			}

			public void mousePressed(MouseEvent e) {
			}

			public void mouseEntered(MouseEvent e) {
			}

			public void mouseExited(MouseEvent e) {
			}
		});
		tableMenu.add(chartItem);
		return tableMenu;
	}

	/**
	 * Creates dialogs to confirm deleting a node from the tree, if yes then
	 * tell the tree model to delete the node from the slected path
	 */
	private void deleteNode() {
		//Node currNode =
		// ((DOMmodel.DOMNodeAdapter)selectedPath.getLastPathComponent()).getNode();
		String message = "Are you sure you want to delete this node";

		if (jtree.getModel().isLeaf(selectedPath.getLastPathComponent())) {
			message += "?";
		} else {
			message += " and all of its children?";
		}

		int ans = JOptionPane.showConfirmDialog(this, message, "Delete Node",
				JOptionPane.YES_NO_OPTION);

		if (ans == JOptionPane.NO_OPTION)
			return;

		//delete the node
		((DOMmodel) jtree.getModel()).removeNodeFrom(selectedPath);
	}

	/**
	 * Intializes and shows the add child dialog
	 */
	private void showAddChildDialog() {
		infoLabel.setText("Adding child to "
				+ selectedPath.getLastPathComponent());

		nameField.setText("");
		attribField.setText("");
		valueField.setText("");

		//display possible locations where to add node

		addChildDialog.pack();
		//center above the main window
		addChildDialog.setLocationRelativeTo(addChildDialog.getParent());
		addChildDialog.show();
	}

	/**
	 * Creates the dialog box that will be made visible when the user choses to
	 * add a child to an esiting node in the tree.
	 */
	public void makeAddChildDialog() {
		addChildDialog = new JDialog(this, "Add Child Node", true);
		Container content = addChildDialog.getContentPane();
		content.setLayout(new BoxLayout(addChildDialog.getContentPane(),
				BoxLayout.X_AXIS));

		content.add(makeAddNodePanel());
		content.add(new JSeparator(SwingConstants.VERTICAL));
		content.add(makeAddChildButtonPanel());
	}

	/**
	 * Creates the layout for the add node input part of the dialog
	 * 
	 * @return the panel which was created for the dialog layout
	 */
	private JPanel makeAddNodePanel() {
		infoLabel = new JLabel(".");
		nameField = new JTextField();
		attribField = new JTextField();
		valueField = new JTextField();

		JPanel childPanel = new JPanel();
		childPanel.setLayout(new BoxLayout(childPanel, BoxLayout.Y_AXIS));
		childPanel.setBorder(BorderFactory.createEmptyBorder(10, 5, 5, 5));

		childPanel.add(infoLabel);
		childPanel.add(Box.createRigidArea(new Dimension(0, 10)));

		JLabel nameLabel = new JLabel("Node Name (required): ");
		childPanel.add(nameLabel);
		childPanel.add(Box.createRigidArea(new Dimension(0, 2)));
		childPanel.add(nameField);
		childPanel.add(Box.createRigidArea(new Dimension(0, 10)));

		JLabel attribLabel = new JLabel(
				"Node Attribute(s) (optional list in the form name=node name, year=1975)");
		childPanel.add(attribLabel);
		childPanel.add(Box.createRigidArea(new Dimension(0, 2)));
		childPanel.add(attribField);
		childPanel.add(Box.createRigidArea(new Dimension(0, 10)));

		JLabel valueLabel = new JLabel("Node Value ");
		childPanel.add(valueLabel);
		childPanel.add(Box.createRigidArea(new Dimension(0, 2)));
		childPanel.add(valueField);
		childPanel.add(Box.createRigidArea(new Dimension(0, 10)));

		return childPanel;
	}

	/**
	 * Creates the layout for the add node button part of the dialog
	 * 
	 * @return the panel which was created for the dialog layout
	 */
	private JPanel makeAddChildButtonPanel() {
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(0, 1, 5, 5));
		//buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));

		JButton addNodeButton = new JButton("Add Node");
		addNodeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addChildNode();
				addChildDialog.hide();
			}
		});

		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addChildDialog.hide();
			}
		});

		buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		buttonPanel.add(addNodeButton);
		buttonPanel.add(Box.createRigidArea(new Dimension(0, 5)));
		buttonPanel.add(cancelButton);
		buttonPanel.add(Box.createVerticalGlue());

		JPanel tempPanel = new JPanel();
		tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
		tempPanel.add(new JSeparator(SwingConstants.VERTICAL));
		tempPanel.add(buttonPanel);

		return buttonPanel;
	}

	/**
	 * Takes the newly created node and tells the tree model to add it to the
	 * tree
	 */
	private void addChildNode() {
		//build the new child from info entered into Add Child dialog
		Node newChild = extractNewChild();

		if (newChild != null) {
			DOMmodel model = (DOMmodel) jtree.getModel();
			model.addTreeModelListener(new MyTreeModelListener());
			model.insertNodeInto(newChild, selectedPath);
			jtree.scrollPathToVisible(new TreePath(newChild));

		}

	}

	/**
	 * Listener for table model events, we only care about when a user has
	 * updated the data, in which case we have to tell the tree to refresh to
	 * show the changed value int the tree.
	 * 
	 * @param e
	 *            the event that has occured
	 */
	public void tableChanged(TableModelEvent e) {
		if (e.getType() == TableModelEvent.UPDATE) {
			((DOMmodel) jtree.getModel())
					.fireTreeNodesChanged(new TreeModelEvent(e.getSource(),
							selectedPath));
		}
	}

	/**
	 * Listener of tree model events only care about when a user has updated the
	 * data, in which case we have to tell the table to refresh to show the
	 * changed value. Make sure that the event came from the tree model so we
	 * don't keep sending messages back and forth between the table and tree
	 */
	class MyTreeModelListener implements TreeModelListener {
		public void treeNodesChanged(TreeModelEvent e) {
			try {
				if (e.getSource() instanceof DOMmodel) {
					BaseTableModel bt;
					if (((JTable) ((JScrollPane) splitPane.getRightComponent())
							.getViewport().getView()).getModel() instanceof TableSorter) {
						bt = (BaseTableModel) ((TableSorter) ((JTable) ((JScrollPane) splitPane
								.getRightComponent()).getViewport().getView())
								.getModel()).getTableModel();
					} else {
						bt = (BaseTableModel) ((JTable) ((JScrollPane) splitPane
								.getRightComponent()).getViewport().getView())
								.getModel();
					}
					bt.fireTableRowsUpdated(0, bt.getRowCount());
				}
			} catch (Exception ex) {
			}
		}

		public void treeNodesInserted(TreeModelEvent e) {
		}

		public void treeNodesRemoved(TreeModelEvent e) {
		}

		public void treeStructureChanged(TreeModelEvent e) {
		}
	}

	/**
	 * Gets the input values from the add child dialog and parses them makes
	 * sure they are valid then returns a new node as specified by the input in
	 * the dialog.
	 * 
	 * @return new node as specified in the dialog
	 */
	private Node extractNewChild() {
		String nodeName = nameField.getText().trim();
		String attribs = attribField.getText().trim();
		String value = valueField.getText().trim();
		int sIndex, eIndex, index;
		boolean repeat = true;
		//create new node with given name
		Element newNode = null;

		while (repeat) {
			try {
				newNode = doc.createElement(nodeName);
				repeat = false;
			} catch (Exception nameExc) {
				JOptionPane.showMessageDialog(null, "Invalid node name!");
				Object[] possibilities = null;
				String s2 = (String) JOptionPane
						.showInputDialog(
								null,
								"Please try again, press cancel or leave blank to cancel 'Add Child'",
								"Re-enter a valid node name)",
								JOptionPane.PLAIN_MESSAGE, null, possibilities,
								null);
				nodeName = s2;
				if (s2 == null || s2.length() == 0) {
					return null;
				}
				System.out.println("BAD NAME .. " + nameExc);
			}
		}

		repeat = true; //for detecting bad attributes
		while (repeat) {

			//add all attributes to the new node, if the exist
			//  hopefully a comma-seporated list of attributes in the form:
			// name=nodeName, year=1975, ...
			if (attribs.length() > 0) {
				StringTokenizer st = new StringTokenizer(attribs, ",", false);
				String attrib, val, strboth;

				try {
					int numTokens = st.countTokens();
					for (int i = 0; i < numTokens; i++) {
						strboth = st.nextToken().trim();
						StringTokenizer stInner = new StringTokenizer(strboth,
								"=", false);
						attrib = stInner.nextToken().trim();
						val = stInner.nextToken().trim();
						newNode.setAttribute(attrib, val);
					}
					repeat = false;

				} catch (Exception se) {
					JOptionPane.showMessageDialog(null,
							"Syntax for attribute(s) incorrect!");
					Object[] possibilities = null;
					String s = (String) JOptionPane
							.showInputDialog(
									null,
									"Please try again, leave blank if you don't want any attributes, press cancel to cancel entire 'Add Child', othwerwise attributes must of form: attrname1=attrval1,attrname2=attrval2, ..",
									"Re-enter attribute(s)",
									JOptionPane.PLAIN_MESSAGE, null,
									possibilities, null);
					if ((s != null) && (s.length() > 0)) {
						attribs = s;

					}
					if (s == null) {
						return null;
					}
					if (s.length() == 0) { // no attribute
						repeat = false;
					}

					//System.out.println("BAD ATTRIBUTE ADDED!!!!");
				}
			} else {
				repeat = false;
			}

		} // while !okaytogoon

		if (value.length() > 0) {
			Text tempText = doc.createTextNode(value);
			newNode.appendChild(tempText);
		}

		return newNode;
	}

	/**
	 * Creates a JFileChooser to figure out which file to parse, then parses the
	 * file and sets doc to it
	 * 
	 * @return true if we parsed a file, false otherwise
	 */
	boolean openXMLFile() {

		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Open XML File");

		// Choose only files, not directories
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

		// Start in current directory
		fc.setCurrentDirectory(globalFC.getCurrentDirectory());

		// Set filter for Java source files.
		fc.setFileFilter(xmlFilter);

		// Now open chooser
		int result = fc.showOpenDialog(this);

		if (result == JFileChooser.CANCEL_OPTION) {
			return true;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			readXMLFile(file);

			//textArea.setText("Opened");
		} else {
			return false;
		}
		return true;
	}

	/**
	 * Creates filchoosers to get a CSV file and a header file, then processes
	 * them with the CSV to XML converter
	 * 
	 * @return true if processed and created a doc, false otherwise
	 */
	boolean openCSVFile() {

		File file2;
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Open CSV File");

		// Choose only files, not directories
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

		// Start in current directory
		fc.setCurrentDirectory(globalFC.getCurrentDirectory());

		// Set filter for Java source files.
		fc.setFileFilter(csvFilter);

		// Now open chooser
		int result = fc.showOpenDialog(this);

		if (result == JFileChooser.CANCEL_OPTION) {
			return true;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			globalFC.setCurrentDirectory(fc.getCurrentDirectory());

			// inserted for opening file 2
			JFileChooser fc2 = new JFileChooser();
			fc2.setDialogTitle("Open Headers File");
			fc2.setFileSelectionMode(JFileChooser.FILES_ONLY);
			fc2.setCurrentDirectory(fc.getCurrentDirectory());
			int result2 = fc2.showOpenDialog(this);
			if (result2 == JFileChooser.CANCEL_OPTION) {
				return true;
			} else if (result2 == JFileChooser.APPROVE_OPTION) {
				file2 = fc2.getSelectedFile();
				globalFC.setCurrentDirectory(fc2.getCurrentDirectory());
				readCSVFile(file, file2);
				// DO STUFF WITH FILE1 AND FILE2
			}

		} else {
			return false;
		}
		return true;
	}

	/**
	 * Use a JFileChooser in Save mode to select files to open. Use a filter for
	 * FileFilter subclass to select for *.java files. If a file is selected,
	 * then write the the string from the textarea into it.
	 */
	boolean saveFile() {

		File file = null;
		JFileChooser fc = new JFileChooser();

		// Start in current directory
		fc.setCurrentDirectory(globalFC.getCurrentDirectory());

		// Set filter for Java source files.
		fc.setFileFilter(xmlFilter); // *********************************

		// Set to a default name for save.
		fc.setSelectedFile(file);

		// Open chooser dialog
		int result = fc.showSaveDialog(this);

		if (result == JFileChooser.CANCEL_OPTION) {
			return true;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			if (!file.getName().matches("[.]")) {
				if (!(file.getAbsolutePath().endsWith(".xml"))) {
					file = new File(file.getAbsolutePath() + ".xml");
				}
			}
			if (file.exists()) {
				int response = JOptionPane.showConfirmDialog(null,
						"Overwrite existing file?", "Confirm Overwrite",
						JOptionPane.OK_CANCEL_OPTION,
						JOptionPane.QUESTION_MESSAGE);
				//if they hit cancell it gives and error message, so i
				//made it return true, that could be a problem in the future
				if (response == JOptionPane.CANCEL_OPTION)
					return true;
			}
			globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			return writeFile(file, doc);
		} else {
			return false;
		}
	}

	/**
	 * Does the parsing of an XML file, and sets doc to it
	 * 
	 * @param file
	 *            the file that will be parsed
	 */
	public void readXMLFile(File file) {
		try {
			lsInput.setByteStream(new FileInputStream(file));
			lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
			doc = lsParser.parse(lsInput);
			//removeEmptyTextNodes(doc.getDocumentElement());
		} catch (Exception e) {
			System.out.println("Got Exception while creating XML document: "
					+ e);
			JOptionPane.showMessageDialog(this,
					"Exception while creating XML document\n" + e, "Exception",
					JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * Takes a CSV file, and Headers file, then processes the files by building
	 * a new tree with the DOMTreeBuilder class. After the tree is build doc is
	 * set to that tree
	 * 
	 * @param file
	 *            the CSV file
	 * @param file2
	 *            the Headers file
	 */
	public void readCSVFile(File file, File file2) {
		StringTokenizer st;
		String intValueStr;
		String strToReplace;
		int counter;
		int intValue;
		int dollarindex = 0;
		String inputLine;

		ArrayList dataArr;
		HashMap nickNameMap = new HashMap(); // shortname -> long string to
											 // append to end
		HashMap tableIDMap = new HashMap(); // tableID -> long string of headers
		DOMTreeBuilder tree = new DOMTreeBuilder();

		try {

			FileInputStream hashfis = new FileInputStream(file2);
			DataInputStream hashfin = new DataInputStream(hashfis);
			BufferedReader hashInput = new BufferedReader(
					new InputStreamReader(hashfin));
			hashInput.readLine(); // ignores first line of file
			inputLine = hashInput.readLine().trim();
			while (inputLine != null && inputLine.charAt(0) == '$') { // read in
																	  // header
																	  // nick
																	  // names
				st = new StringTokenizer(inputLine, ",", false);
				intValueStr = st.nextToken(); // $nickname
				inputLine = inputLine.substring(intValueStr.length() + 1)
						.trim();
				nickNameMap.put(intValueStr, inputLine);
				if ((inputLine = hashInput.readLine()) != null) {
					inputLine.trim();
				}
			}
			while (inputLine != null) {
				if (!inputLine.equals("")) {
					st = new StringTokenizer(inputLine, ",", false);
					intValueStr = st.nextToken(); // numID
					inputLine = inputLine.substring(intValueStr.length() + 1); // everything
																			   // but
																			   // numID
					try {
						intValue = Integer.parseInt(intValueStr);

						inputLine = inputLine.replaceAll("[,][\\s]*[,]", ""); // gets
																			  // rid
																			  // of
																			  // end
																			  // commas
						if (inputLine.endsWith(",")) { // gets ride of last
													   // comma if there is one
							inputLine = inputLine.substring(0, inputLine
									.length() - 1);
						} // extra commas are now all gone

						dollarindex = 0;
						while ((dollarindex = inputLine.indexOf('$')) != -1) {
							counter = dollarindex;
							while (counter < inputLine.length()
									&& inputLine.charAt(counter) != ',') {
								counter++;
							}
							strToReplace = inputLine.substring(dollarindex,
									counter);
							if (nickNameMap.containsKey(strToReplace)) {
								//strToReplace = strToReplace.substring(1);
								//strToReplace = "^[.]*"+strToReplace+"[.]*$";
								inputLine = inputLine.replaceAll("\\"
										+ strToReplace, ((String) nickNameMap
										.get(strToReplace)));
							} else {
								System.out
										.println("***Couldn't find replacement for "
												+ strToReplace + "!***");
								JOptionPane.showMessageDialog(this,
										"Couldn't find replacement for "
												+ strToReplace, "Warning",
										JOptionPane.WARNING_MESSAGE);
							}
						}
						tableIDMap.put(new Integer(intValue), inputLine);
					} catch (NumberFormatException e) {
						System.out
								.println("*** Hashtable file formatted incorrectly ***"
										+ e);
						JOptionPane.showMessageDialog(this,
								"Hashtable file formatted incorrectly\n" + e,
								"Exception", JOptionPane.ERROR_MESSAGE);
					}
				}
				if ((inputLine = hashInput.readLine()) != null) {
					inputLine.trim();
				}
			}

			// tableIDMap should now be all set up ...

			FileInputStream fis = new FileInputStream(file);
			DataInputStream fin = new DataInputStream(fis);
			BufferedReader stdInput = new BufferedReader(new InputStreamReader(
					fin));

			inputLine = stdInput.readLine().trim(); // read one line of input

			while (inputLine != null) {
				while (inputLine != null
						&& !inputLine.startsWith("INPUT_TABLE")) {
					inputLine = stdInput.readLine();
				}
				if (inputLine == null) {
					break;
				}
				stdInput.readLine(); // reads/ignores "Variable ID" line
				inputLine = stdInput.readLine().trim(); // should have just the
														// id number
				st = new StringTokenizer(inputLine, ",", false);
				intValue = Integer.parseInt(st.nextToken());

				if (tableIDMap.containsKey(new Integer(intValue))) {
					tree.setHeader(((String) tableIDMap.get(new Integer(
							intValue))));
					stdInput.readLine(); // ignores this line
					stdInput.readLine(); // ignores header line

					inputLine = stdInput.readLine().trim(); // start reading in
															// data
					while (inputLine != null && !inputLine.equals("")
							&& inputLine.charAt(0) != ',') {
						st = new StringTokenizer(inputLine, ",", false);
						int NUM_COLS = st.countTokens();
						dataArr = new ArrayList(NUM_COLS);
						for (int i = 0; i < NUM_COLS; i++) {
							dataArr.add(i, (st.nextToken()).trim());
						} // one line of data stores in arraylist
						tree.addToTree(dataArr);
						//makeTree( rootElement, docName );
						dataArr.clear();
						if ((inputLine = stdInput.readLine()) != null) {
							inputLine.trim();
						}
					}
				} else {
					System.out.println("***Warning: skipping table: "
							+ intValue + "!***");
				}

				if ((inputLine = stdInput.readLine()) != null) {
					inputLine.trim();
				}
			}

			doc = tree.getDoc();
			//tree.outputTree(args[2]);

			fin.close();
			hashfin.close();

		} catch (Exception e) {
			System.out
					.println("Excpetion thrown while trying to read csv and header files "
							+ e);
			StackTraceElement[] s = e.getStackTrace();
			for (int i = 0; i < s.length; i++) {
				System.out.println(s[i]);
			}
			JOptionPane.showMessageDialog(this,
					"Excpetion thrown while trying to read csv and header files\n"
							+ e, "Exception", JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * Writes the DOM document to the specified file
	 * 
	 * @param file
	 *            where the XML tree will be written to
	 * @param thDoc
	 *            the tree that should be written
	 * @return whether the file was actually written or not
	 */
	public boolean writeFile(File file, Document theDoc) {
		// specify output formating properties
		OutputFormat format = new OutputFormat(theDoc);
		format.setEncoding("UTF-8");
		format.setLineSeparator("\r\n");
		format.setIndenting(true);
		format.setIndent(3);
		format.setLineWidth(0);
		format.setPreserveSpace(false);
		format.setOmitDocumentType(true);

		// create the searlizer and have it print the document

		try {
			FileWriter fw = new FileWriter(file);
			XMLSerializer serializer = new XMLSerializer(fw, format);
			serializer.asDOMSerializer();
			serializer.serialize(theDoc);
			fw.close();
		} catch (java.io.IOException e) {
			System.err.println("Error outputing tree: " + e);
			return false;
		}
		return true;
	}
}

