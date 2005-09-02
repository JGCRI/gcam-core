package ModelInterface.ModelGUI2;

import org.w3c.dom.*;
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.*;
import com.sun.org.apache.xml.internal.serialize.OutputFormat;
import com.sun.org.apache.xml.internal.serialize.XMLSerializer;
//import org.apache.xpath.domapi.*;
import org.jfree.chart.JFreeChart;
//import org.w3c.dom.xpath.*;

import javax.xml.xpath.*;

//import org.apache.xpath.XPath;
//import org.apache.xpath.XPathContext;
//import org.apache.xml.utils.PrefixResolver;
//import org.apache.xml.utils.PrefixResolverDefault;
import javax.xml.transform.TransformerException;

import javax.swing.event.*;
import javax.swing.tree.TreeSelectionModel;

import javax.swing.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.*;
import java.util.regex.*;
import javax.swing.tree.TreePath;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import ModelInterface.InterfaceMain;
import ModelInterface.MenuAdder;

import ModelInterface.ModelGUI2.tables.*;

public class InputViewer implements ActionListener, TableModelListener, MenuAdder {

	private InputViewer thisViewer;

	private Document doc;

	private LSInput lsInput;

	private LSParser lsParser;

	private DOMImplementationLS implls;

	private int lastFlipX = 0;

	private int lastFlipY = 0;

	private JFrame parentFrame;

	private static String controlStr = "InputViewer";

	JSplitPane splitPane;

	JLabel infoLabel;

	//JTextField nameField;

	//JTextField attribField;

	//JTextField valueField;

	//Document lastDoc;

	//JTextArea textArea;

	JTree jtree;

	//JTable jTable;

	private JPopupMenu treeMenu;

	private TreePath selectedPath;

	private JDialog addChildDialog;

	private JPopupMenu tableMenu; // for new 'flip' right click option
	
	private JFrame chartWindow = null;
	
	private JPanel chartPanel = null;
	//private JLabel labelChart = null;

	XMLFilter xmlFilter = new XMLFilter();

	CSVFilter csvFilter = new CSVFilter();

	private TableSelector tableSelector;

	private File file;


	private int leftWidth;

	private Documentation documentation;

	//int windowHeight;
	//int windowWidth;
	//JFileChooser globalFC;

	public InputViewer(JFrame parentFrameIn) {
		try {
			System.setProperty(DOMImplementationRegistry.PROPERTY,
					//"com.sun.org.apache.xerces.internal.dom.DOMImplementationSourceImpl");
					"org.apache.xerces.dom.DOMImplementationSourceImpl");
			DOMImplementationRegistry reg = DOMImplementationRegistry
					.newInstance();
			implls = (DOMImplementationLS)reg.getDOMImplementation("XML 3.0");
			if (implls == null) {
				System.out
						.println("Could not find a DOM3 Load-Save compliant parser.");
				JOptionPane.showMessageDialog(parentFrame,
						"Could not find a DOM3 Load-Save compliant parser.",
						"Initialization Error", JOptionPane.ERROR_MESSAGE);
				return;
			}
			lsInput = implls.createLSInput();
			lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
		} catch (Exception e) {
			System.err.println("Couldn't initialize DOMImplementation: " + e);
			JOptionPane.showMessageDialog(parentFrame,
					"Couldn't initialize DOMImplementation\n" + e,
					"Initialization Error", JOptionPane.ERROR_MESSAGE);
			e.printStackTrace();
		}
		parentFrame = parentFrameIn;
		thisViewer = this;

		// Create a window to display the chart in.
		chartWindow = new JFrame( "Charts" );
		chartPanel = new JPanel();
		chartPanel.setLayout( new BoxLayout(chartPanel, BoxLayout.Y_AXIS));
		
		// Allow the chart panel to scroll if the user selects it.
		chartPanel.setAutoscrolls(true);
		chartPanel.addMouseMotionListener(new MouseMotionListener(){
		    public void mouseDragged(MouseEvent aEvent) {
			    System.out.println("Dragging");
		        // Use the drag even to force a scroll to the event position.
		        Rectangle currRect = new Rectangle(aEvent.getX(), aEvent.getY(), 1, 1);
		        chartPanel.scrollRectToVisible(currRect);
		    }

			public void mouseMoved(MouseEvent aEvent) {
				// Ignore mouse movement if not dragging.
			}
		}
		);
		chartWindow.setContentPane(new JScrollPane(chartPanel));
		((JScrollPane)chartWindow.getContentPane()).setPreferredSize(new Dimension(520,530));
		chartWindow.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				chartPanel.removeAll();
				//chartWindow.getContentPane().removeAll();
			}
		});
		final PropertyChangeListener savePropListener = new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent e) {
				if(e.getPropertyName().equals("Document-Modified")) {
					((InterfaceMain)parentFrame).getSaveMenu().setEnabled(true);
				} else if(e.getPropertyName().equals("Document-Save")) {
					((InterfaceMain)parentFrame).getSaveMenu().setEnabled(false);
				}
			}
		};

		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					if(evt.getOldValue().equals(controlStr)) {
						((InterfaceMain)parentFrame).getSaveMenu().removeActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().removeActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(false);
						((InterfaceMain)parentFrame).removePropertyChangeListener(savePropListener);
						//((InterfaceMain)parentFrame).getQuitMenu().removeActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveMenu().setEnabled(false);
						doc = null;
						documentation = null;
						parentFrame.getContentPane().removeAll();
						parentFrame.setTitle("ModelInterface");
						if(splitPane != null) {
							((InterfaceMain)parentFrame).getProperties().setProperty("dividerLocation", 
								 String.valueOf(splitPane.getDividerLocation()));
						}
					}
					if(evt.getNewValue().equals(controlStr)) {
						((InterfaceMain)parentFrame).getSaveMenu().addActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().addActionListener(thisViewer);
						((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(true);
						((InterfaceMain)parentFrame).addPropertyChangeListener(savePropListener);
						//((InterfaceMain)parentFrame).getQuitMenu().addActionListener(thisViewer);
						//((InterfaceMain)parentFrame).oldControl = "FileChooserDemo.File";
						leftWidth = Integer.parseInt(((InterfaceMain)parentFrame).
							getProperties().getProperty("dividerLocation", "200"));
					}
				}
			}
		});
		tableSelector = new TableSelector(parentFrame);
	}

	public void addMenuItems(InterfaceMain.MenuManager menuMan) {
		JMenuItem menuItem = new JMenuItem("XML file");
		menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_2,
				ActionEvent.ALT_MASK));
		menuItem.addActionListener(this);
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).
			getSubMenuManager(InterfaceMain.FILE_OPEN_SUBMENU_POS).addMenuItem(menuItem, 10);

		menuItem = new JMenuItem("CSV file");
		menuItem.addActionListener(this);
		menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).
			getSubMenuManager(InterfaceMain.FILE_OPEN_SUBMENU_POS).addMenuItem(menuItem, 20);
		int addedTo;
		//JMenu tableMenu = new JMenu("Table");
		addedTo = menuMan.addMenuItem(new JMenu("Table"), 10);
		final JMenuItem menuTableFilter = makeMenuItem("Filter");
		menuTableFilter.setEnabled(false);
		menuMan.getSubMenuManager(addedTo).addMenuItem(menuTableFilter, 0);
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent evt) {
				if(evt.getPropertyName().equals("Control")) {
					//if(evt.getOldValue().equals(controlStr)) {
						menuTableFilter.setEnabled(false);
					//}
				} else if(evt.getPropertyName().equals("Table")) {
					menuTableFilter.setEnabled(true);
				}
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
		Container contentPane = parentFrame.getContentPane();
		/*
		contentPane.removeAll();
		if(xmlDB != null) {
			xmlDB.closeDB();
			//menuManage.setEnabled(false);
			//menuExpPrn.setEnabled(false);
		}
		if (splitPane != null) {
			contentPane.remove(splitPane);
		}
		*/
		// Set up the tree
		jtree = new JTree(new DOMmodel(doc.getDocumentElement()));
		System.out.println("HERE");
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
		treeView.setPreferredSize(new Dimension(leftWidth, parentFrame.getHeight()));

		//jTable = new JTable();
		JScrollPane tableView = new JScrollPane(/* jTable */);
		//tableView.setPreferredScrollableViewportSize( new Dimension (
		// rightWidth, windowHeight ));
		tableView.setPreferredSize(new Dimension(parentFrame.getWidth()- leftWidth, parentFrame.getHeight()));

		// Build split-pane view
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, treeView,
				tableView);
		splitPane.setContinuousLayout(true);
		splitPane.setDividerLocation(leftWidth);
		splitPane.setPreferredSize(new Dimension(parentFrame.getWidth()+ 10, parentFrame.getHeight()+ 10));
		// Add GUI components
		contentPane.add("Center", splitPane);

		treeMenu = makePopupTreeMenu();

		//create the dialog for adding new node children, but leave invisible
		makeAddChildDialog();

		//this.show();
		//this.pack();
		parentFrame.setVisible(true);
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
				// probably the cancel, just return here to avoid exceptions
				return;
			}
			((InterfaceMain)parentFrame).fireControlChange(controlStr);
			displayJtree();
			//menuSave.setEnabled(true); // now save can show up
			//setTitle("["+file+"] - ModelGUI");
			parentFrame.setTitle("["+file+"] - ModelInterface");
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
			((InterfaceMain)parentFrame).fireControlChange(controlStr);
			displayJtree();
			//menuSave.setEnabled(true); // now save can show up
			//parentFrame.setTitle("["+file+"] - ModelGUI");
			parentFrame.setTitle("["+file+"] - ModelInterface");
		} else if (command.equals("Save")) {
			if (!(file.getAbsolutePath().endsWith(".xml"))) {
				status = saveFile();
			} else {
				status = saveFile(file);
			}
			if (!status) {
				JOptionPane.showMessageDialog(null,
						"IO error in saving file!!", "File Save Error",
						JOptionPane.ERROR_MESSAGE);
				return;
			}
			parentFrame.setTitle("["+file+"] - ModelInterface");
			((InterfaceMain)parentFrame).fireProperty("Document-Save", null, doc);
		} else if (command.equals("Save As")) {
			// Save a file
			status = saveFile();
			if (!status) {
				JOptionPane.showMessageDialog(null,
						"IO error in saving file!!", "File Save Error",
						JOptionPane.ERROR_MESSAGE);
				return;
			}
			parentFrame.setTitle("["+file+"] - ModelInterface");
			((InterfaceMain)parentFrame).fireProperty("Document-Save", null, doc);
		} else if (command.equals("Filter")) {
				/*
			try {
				if (((JTable) ((JScrollPane) splitPane.getRightComponent())
						.getViewport().getView()).getModel() instanceof TableSorter) {

					((BaseTableModel) ((TableSorter) ((JTable) ((JScrollPane) splitPane
							.getRightComponent()).getViewport().getView())
							.getModel()).getTableModel()).filterData(parentFrame);
				} else {
					((BaseTableModel) ((JTable) ((JScrollPane) splitPane
							.getRightComponent()).getViewport().getView())
							.getModel()).filterData(parentFrame);
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
				*/
			// new and old values should be??
			((InterfaceMain)parentFrame).fireProperty("Filter", null, 1);
		} else if(command.equals("Add Child")) {
			jtree.setSelectionPath(selectedPath);

			Node nodeClicked = ((DOMmodel.DOMNodeAdapter) jtree
					.getLastSelectedPathComponent()).getNode();
			if (nodeClicked.getNodeType() != Element.TEXT_NODE) { // can't
				// add child to text node
				showAddChildDialog();
			}

		} else if (command.equals("Delete Node")) {
			jtree.setSelectionPath(selectedPath);
			deleteNode();
		} else if (command.equals("Display Table")) {
			displayTable();
		} else if(command.equals("Annotate")) {
			jtree.setSelectionPath(selectedPath);

			Node nodeClicked = ((DOMmodel.DOMNodeAdapter) jtree
					.getLastSelectedPathComponent()).getNode();
			// should probably not be enabled if documentation is null
			if(documentation != null) {
				documentation.getDocumentation(nodeClicked);
			}
			/*
			String nodeCXPath = nodeToXPath(nodeClicked).toString();
			System.out.println("Node clicked XPath: "+nodeCXPath);
			//if (nodeClicked.getNodeType() != Element.TEXT_NODE) {
				try {
					PrefixResolver pr = new PrefixResolverDefault(doc.getDocumentElement());
					XPath xp = new XPath(xpStr, null, pr, XPath.MATCH);
					XPath xpCurrNode = new XPath(nodeCXPath, null, pr, XPath.MATCH);
					if(xp.getExpression().deepEquals(xpCurrNode.getExpression())) {
					//if(xp.getExpression().bool(new XPathContext())) {
						System.out.println("Wow it bool'ed");
					} else {
						System.out.println("Yea it didn't bool");
					}
					System.out.println("Merged XPath: "+meregeXPaths(xpStr, nodeCXPath));
				} catch(TransformerException te) {
					te.printStackTrace();
				}
			//}
			*/
		}
	}

	/* moved to it's correct place.. delete this
	private TreeSet doc1Nodes;
	private void evalDocumentationLink(String xpLink) {
		doc1Nodes = new TreeSet(new Comparator() {
			public int compare(Object obj1, Object obj2) {
				if(obj1.equals(obj2)) {
					return 0;
				} else {
					String node1Val = ((Node)obj1).getNodeValue();
					String node2Val = ((Node)obj2).getNodeValue();
					int ret = String.CASE_INSENSITIVE_ORDER.compare(node1Val, node2Val);
					if(ret == 0) {
						return 1;
					} else {
						return ret;
					}
				}
			}
		});

		try {
			XPath xpImpl = XPathFactory.newInstance().newXPath();
			XPathExpression xpe = xpImpl.compile(xpLink);
			NodeList nl = (NodeList)xpe.evaluate(doc.getDocumentElement(), XPathConstants.NODESET);
			for(int i = 0; i < nl.getLength(); ++i) {
				doc1Nodes.add(nl.item(i));
			}
			//System.out.println("Evaluated to: "+xpe.evaluate(doc.getDocumentElement(), XPathConstants.STRING));
		} catch(XPathExpressionException xpee) {
			xpee.printStackTrace();
		}
	}
	*/

	String meregeXPaths(String path1, String path2) {
		String[] path1Arr = path1.split("/");
		String[] path2Arr = path2.split("/");
		StringBuffer strBuff = new StringBuffer();
		if(path1Arr.length != path2Arr.length) {
			System.out.println("Can't merge "+path1+" and "+path2);
			return null;
		}
		for(int i = 1; i < path1Arr.length; ++i) {
			if(path1Arr[i].equals(path2Arr[i])) {
				strBuff.append("/").append(path1Arr[i]);
			} else if(path1Arr[i].indexOf('[') == -1 && path2Arr[i].indexOf('[') == -1) {
				return null;
			} else if(path1Arr[i].indexOf('[') != -1 && path2Arr[i].indexOf('[') != -1 && !path1Arr[i].substring(0, path1Arr[i].indexOf('[')).equals(path2Arr[i].substring(0, path2Arr[i].indexOf('[')))) {
				return null;
			} else if(path1Arr[i].indexOf('[') == -1 && path2Arr[i].indexOf('[') != -1) {
				strBuff.append("/").append(path2Arr[i]);
			} else {
				String[] attrs = path1Arr[i].substring(path1Arr[i].indexOf('[')+1, path1Arr[i].indexOf(']')).split(" or ");
				String p2Attr = path2Arr[i].substring(path2Arr[i].indexOf('[')+1, path2Arr[i].indexOf(']'));
				boolean found = false;
				for(int j = 0; j < attrs.length && !found; ++j) {
					if(attrs[j].equals(p2Attr)) {
						strBuff.append("/").append(path1Arr[i]);
						found = true;
					}
				}
				if(!found) {
					strBuff.append("/").append(path1Arr[i].substring(0,path1Arr[i].length()-1)).append(" or ").append(p2Attr).append("]");
				}
			}
		}
		return strBuff.toString();
	}
				
	StringBuffer nodeToXPath(Node n) {
		if(n.getNodeType() != Node.DOCUMENT_NODE) {
			StringBuffer buf = nodeToXPath(n.getParentNode());
			if(n.getNodeType() == Node.TEXT_NODE) {
				return buf.append("node()");
			}
			buf.append(n.getNodeName());
			NamedNodeMap nm = n.getAttributes();
			if(nm.getLength() > 0) {
				buf.append("[");
			}
			if(nm.getLength() > 1) {
				buf.append("(");
			}
			for(int i = 0; i < nm.getLength(); ++i) {
				buf.append("(@").append(nm.item(i).getNodeName()).append("='").append(nm.item(i).getNodeValue()).append("')");
				if(i+1 != nm.getLength()) {
					buf.append(" and ");
				}
			}
			if(nm.getLength() > 1) {
				buf.append(")");
			}
			if(nm.getLength() > 0) {
				buf.append("]");
			}
			return buf.append("/");
		} else {
			return new StringBuffer("/");
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
		menuItem.addActionListener(this);
		treeMenu.add(menuItem);
		treeMenu.add(new JSeparator());
		menuItem = new JMenuItem("Add Child");
		menuItem.addActionListener(this);
		treeMenu.add(menuItem);

		treeMenu.add(new JSeparator());
		menuItem = new JMenuItem("Delete Node");
		menuItem.addActionListener(this);
		treeMenu.add(menuItem);

		menuItem = new JMenuItem("Annotate");
		menuItem.addActionListener(this);
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
		flipItem.addMouseListener(new MouseAdapter() {
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
		});
		tableMenu.add(flipItem);
		
		final JMenuItem chartItem = new JMenuItem("Chart");
		chartItem.addMouseListener( new MouseAdapter(){
			public void mouseReleased(MouseEvent e) {
				final MouseEvent me = e;
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						JTable jTable = (JTable) ((JScrollPane) splitPane
								.getRightComponent()).getViewport().getView();

						// get the correct row and col which only matters for
						// mulitablemodel
						// then be sure to pass on the call to the correct table
						// model
						me.translatePoint(lastFlipX, lastFlipY);
						Point p = me.getPoint();
						int row = jTable.rowAtPoint(p);
						int col = jTable.columnAtPoint(p);

						JFreeChart chart;
						if (jTable.getModel() instanceof TableSorter) {
							chart = ((BaseTableModel) ((TableSorter) jTable
									.getModel()).getTableModel()).createChart(row, col);
						} else {
							chart = ((BaseTableModel) jTable.getModel())
									.createChart(row, col);
						}
						// Turn the chart into an image.
						BufferedImage chartImage = chart.createBufferedImage(
								500, 500);

						JLabel labelChart = new JLabel();
						labelChart.setIcon(new ImageIcon(chartImage));
						chartPanel.add(labelChart);
						chartPanel.add(Box.createVerticalStrut(10));

						chartWindow.pack();
						chartWindow.setVisible(true);
					}
				});
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

		int ans = JOptionPane.showConfirmDialog(parentFrame, message, "Delete Node",
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

		//display possible locations where to add node

		addChildDialog.pack();
		//center above the main window
		addChildDialog.setLocationRelativeTo(addChildDialog.getParent());
		addChildDialog.setVisible(true);
	}

	/**
	 * Creates the dialog box that will be made visible when the user choses to
	 * add a child to an esiting node in the tree.
	 */
	public void makeAddChildDialog() {
		addChildDialog = new JDialog(parentFrame, "Add Child Node", true);
		Container content = addChildDialog.getContentPane();
		content.setLayout(new BoxLayout(addChildDialog.getContentPane(),
				BoxLayout.Y_AXIS));

		infoLabel = new JLabel(".");
		final JTextField nameField = new JTextField();
		final JTextField attribField = new JTextField();
		final JTextField valueField = new JTextField();

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

		content.add(childPanel);
		content.add(new JSeparator(SwingConstants.HORIZONTAL));

		JPanel buttonPanel = new JPanel();
		//buttonPanel.setLayout(new GridLayout(0, 1, 5, 5));
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));

		JButton addNodeButton = new JButton("Add Node");
		addNodeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(addChildNode(nameField, attribField, valueField)) {
					addChildDialog.setVisible(false);
				}
			}
		});

		JButton cancelButton = new JButton("Cancel");
		cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				addChildDialog.setVisible(false);
			}
		});

		buttonPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		buttonPanel.add(Box.createHorizontalGlue());
		buttonPanel.add(addNodeButton);
		buttonPanel.add(Box.createRigidArea(new Dimension(5, 5)));
		buttonPanel.add(cancelButton);

		content.add(buttonPanel);

	}

	/**
	 * Creates the layout for the add node input part of the dialog
	 * 
	 * @return the panel which was created for the dialog layout
	 */
	/*
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
	*/

	/**
	 * Creates the layout for the add node button part of the dialog
	 * 
	 * @return the panel which was created for the dialog layout
	 */
	/*
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
	*/

	/**
	 * Takes the newly created node and tells the tree model to add it to the
	 * tree
	 */
	private boolean addChildNode(JTextField nameField, JTextField attrField, JTextField dataField) {
		String name = nameField.getText();
		String attr = attrField.getText();
		String data = dataField.getText();
		Element tempNode = null;
		if(name.equals("")) {
			JOptionPane.showMessageDialog(parentFrame, "You must supply a name", 
					"Invalid Name", JOptionPane.ERROR_MESSAGE);
			return false;
		} else {
			try {
				tempNode = doc.createElement(name);
			} catch(DOMException e) {
				if(e.code == DOMException.INVALID_CHARACTER_ERR) {
					JOptionPane.showMessageDialog(parentFrame, "Invalid XML name, please Change your Node Name", 
							"Invalid Name", JOptionPane.ERROR_MESSAGE);
				} else {
					JOptionPane.showMessageDialog(parentFrame, e, 
							"Invalid Name", JOptionPane.ERROR_MESSAGE);
				}
				return false;
			}
		}
		if(!attr.equals("")) {
			boolean gotSome = false;
			Pattern pat = Pattern.compile("\\s*(\\w+)=([^,]+)\\s*(,|\\z)");
			Matcher mt = pat.matcher(attr);
			while(mt.find()) {
				try {
					tempNode.setAttribute(mt.group(1), mt.group(2));
					gotSome = true;
				} catch(DOMException e) {
					if(e.code == DOMException.INVALID_CHARACTER_ERR) {
						JOptionPane.showMessageDialog(parentFrame, "Invalid XML attribute name, please check your attribute names", 
								"Invalid Attribute", JOptionPane.ERROR_MESSAGE);
					} else {
						JOptionPane.showMessageDialog(parentFrame, e, 
								"Invalid Name", JOptionPane.ERROR_MESSAGE);
					}
					return false;
				}
			}
			if(!gotSome) {
				// show error
				JOptionPane.showMessageDialog(parentFrame, "Please check the syntax of you Attributes", 
						"Invalid Attributes", JOptionPane.ERROR_MESSAGE);
				return false;
			}
		}
		if(!data.equals("")) {
			tempNode.appendChild(doc.createTextNode(data));
		}

		DOMmodel model = (DOMmodel) jtree.getModel();
		model.insertNodeInto(tempNode, selectedPath);
		nameField.setText("");
		attrField.setText("");
		dataField.setText("");
		return true;
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
		if (jtree != null && e.getType() == TableModelEvent.UPDATE) {
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
					BaseTableModel bt = getTableModelFromScrollPane((JScrollPane)splitPane.getRightComponent());
					/*
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
					*/
					bt.fireTableRowsUpdated(0, bt.getRowCount());
					((InterfaceMain)parentFrame).fireProperty("Document-Modified", null, doc);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}

		public void treeNodesInserted(TreeModelEvent e) {
			((InterfaceMain)parentFrame).fireProperty("Document-Modified", null, doc);
		}

		public void treeNodesRemoved(TreeModelEvent e) {
			((InterfaceMain)parentFrame).fireProperty("Document-Modified", null, doc);
		}

		public void treeStructureChanged(TreeModelEvent e) {
			((InterfaceMain)parentFrame).fireProperty("Document-Modified", null, doc);
		}
	}

	/**
	 * Gets the input values from the add child dialog and parses them makes
	 * sure they are valid then returns a new node as specified by the input in
	 * the dialog.
	 * 
	 * @return new node as specified in the dialog
	 */
	/*
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
*/

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
		//fc.setCurrentDirectory(globalFC.getCurrentDirectory());
		fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")));

		// Set filter for Java source files.
		fc.setFileFilter(xmlFilter);

		// Now open chooser
		int result = fc.showOpenDialog(parentFrame);

		if (result == JFileChooser.CANCEL_OPTION) {
			return true;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", fc.getCurrentDirectory().toString());
			doc = readXMLFile( file);
			String docLoc = doc.getDocumentElement().getAttribute("documentation");
			if(docLoc == null) {
				documentation = null;
			} else {
				documentation = new Documentation(doc, docLoc, lsParser, lsInput);
			}
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
		//fc.setCurrentDirectory(globalFC.getCurrentDirectory());
		fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")));

		// Set filter for Java source files.
		fc.setFileFilter(csvFilter);

		// Now open chooser
		int result = fc.showOpenDialog(parentFrame);

		if (result == JFileChooser.CANCEL_OPTION) {
			return true;
		} else if (result == JFileChooser.APPROVE_OPTION) {
			file = fc.getSelectedFile();
			//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", fc.getCurrentDirectory().toString());

			// inserted for opening file 2
			JFileChooser fc2 = new JFileChooser();
			fc2.setDialogTitle("Open Headers File");
			fc2.setFileSelectionMode(JFileChooser.FILES_ONLY);
			fc2.setCurrentDirectory(fc.getCurrentDirectory());
			int result2 = fc2.showOpenDialog(parentFrame);
			if (result2 == JFileChooser.CANCEL_OPTION) {
				return true;
			} else if (result2 == JFileChooser.APPROVE_OPTION) {
				file2 = fc2.getSelectedFile();
				//globalFC.setCurrentDirectory(fc2.getCurrentDirectory());
				((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", fc.getCurrentDirectory().toString());
				readCSVFile(file, file2);
				// DO STUFF WITH FILE1 AND FILE2
			}

		} else {
			return false;
		}
		return true;
	}

	boolean saveFile(File where) {
		return writeFile(where, doc);
	}

	boolean saveFile() {
		// save as..

		//File file = null;
		JFileChooser fc = new JFileChooser();

		// Start in current directory
		//fc.setCurrentDirectory(globalFC.getCurrentDirectory());
		fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")));

		// Set filter for xml files.
		fc.setFileFilter(xmlFilter); // *********************************

		// Set to a default name for save.
		fc.setSelectedFile(file);

		// Open chooser dialog
		int result = fc.showSaveDialog(parentFrame);

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
			//globalFC.setCurrentDirectory(fc.getCurrentDirectory());
			((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory", fc.getCurrentDirectory().toString());
			return writeFile(file, doc);
		} else {
			return false;
		}
	}


	/**
	 * Does the parsing of an XML file, and returns it
	 * 
	 * @param file
	 *            the file that will be parsed
	 * @return the parsed document
	 */
	public Document readXMLFile(File file) {
		try {
			lsInput.setByteStream(new FileInputStream(file));
			/*
			lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
			*/
			return lsParser.parse(lsInput);
		} catch (Exception e) {
			System.out.println("Got Exception while creating XML document: "
					+ e);
			JOptionPane.showMessageDialog(parentFrame,
					"Exception while creating XML document\n" + e, "Exception",
					JOptionPane.ERROR_MESSAGE);
		}
		return null;
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
								JOptionPane.showMessageDialog(parentFrame,
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
						JOptionPane.showMessageDialog(parentFrame,
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
			JOptionPane.showMessageDialog(parentFrame,
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

	private BaseTableModel getTableModelFromScrollPane(JScrollPane sp) {
		Object ret = ((JTable)sp.getViewport().getView()).getModel();
		if(ret instanceof TableSorter) {
			return (BaseTableModel)((TableSorter)ret).getTableModel();
		} else {
			return (BaseTableModel)ret;
		}
	}

	public void displayTable() {
		if (!jtree.getModel().isLeaf(jtree.getLastSelectedPathComponent())) {

			// find out the type of table and create it
			JScrollPane tableView = tableSelector.createSelection(selectedPath, doc,
					parentFrame, thisViewer);

			if (tableView == null) {
				return;
			}
			Object oldVal = null;
			if(((JScrollPane)splitPane.getRightComponent()).getViewport().getView() != null) {
				oldVal = getTableModelFromScrollPane((JScrollPane)splitPane.getRightComponent());
			}
			((InterfaceMain)parentFrame).fireProperty("Table", oldVal, getTableModelFromScrollPane(tableView));

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
			((JTable) tableView.getViewport().getView()).addMouseListener(new MouseAdapter() {
				public void mousePressed(MouseEvent e) {
					maybeShowPopup(e);
				}

				public void mouseReleased(MouseEvent e) {
					maybeShowPopup(e);
				}

				private void maybeShowPopup(MouseEvent e) {
					if (e.isPopupTrigger()) {
						MenuElement[] me = tableMenu.getSubElements();
						for (int i = 0; i < me.length; i++) {
							if (((JMenuItem) me[i]).getText()
								.equals("Flip")) {
								lastFlipX = e.getX();
								lastFlipY = e.getY();
								}
						}
						tableMenu.show(e.getComponent(), e.getX(), e.getY());
					}
				}
			});
		}

	}
}
