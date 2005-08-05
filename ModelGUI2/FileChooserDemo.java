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
import java.awt.geom.Point2D;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.*;
import javax.swing.tree.TreePath;

import org.jfree.report.JFreeReport;
import org.jfree.report.Group;
import org.jfree.report.modules.gui.base.ExportPluginFactory;
import org.jfree.report.JFreeReportBoot;
import org.jfree.report.ElementAlignment;
import org.jfree.report.ReportProcessingException;
import org.jfree.report.modules.gui.base.PreviewDialog;
//import org.jfree.report.elementfactory.TextFieldElementFactory;
import org.jfree.report.elementfactory.DrawableFieldElementFactory;
import org.jfree.ui.FloatDimension;
/*
import org.jfree.report.*;
import org.jfree.report.content.*;
import org.jfree.report.demo.*;
import org.jfree.report.demo.cards.*;
import org.jfree.report.demo.conditionalgroup.*;
import org.jfree.report.demo.form.*;
import org.jfree.report.demo.helper.*;
import org.jfree.report.demo.multireport.*;
import org.jfree.report.demo.sportscouncil.*;
import org.jfree.report.elementfactory.*;
import org.jfree.report.event.*;
import org.jfree.report.filter.*;
import org.jfree.report.filter.templates.*;
import org.jfree.report.function.*;
import org.jfree.report.modules.gui.base.*;
import org.jfree.report.modules.gui.base.components.*;
import org.jfree.report.modules.gui.base.resources.*;
import org.jfree.report.modules.gui.config.*;
import org.jfree.report.modules.gui.config.editor.*;
import org.jfree.report.modules.gui.config.model.*;
import org.jfree.report.modules.gui.config.xml.*;
import org.jfree.report.modules.gui.config.xml.*;
import org.jfree.report.modules.gui.converter.*;
import org.jfree.report.modules.gui.converter.components.*;
import org.jfree.report.modules.gui.converter.parser.*;
import org.jfree.report.modules.gui.csv.*;
import org.jfree.report.modules.gui.html.*;
import org.jfree.report.modules.gui.pdf.*;
import org.jfree.report.modules.gui.plaintext.*;
import org.jfree.report.modules.gui.print.*;
import org.jfree.report.modules.gui.rtf.*;
import org.jfree.report.modules.gui.xls.*;
import org.jfree.report.modules.misc.beanshell.*;
import org.jfree.report.modules.misc.configstore.base.*;
import org.jfree.report.modules.misc.configstore.filesystem.*;
import org.jfree.report.modules.misc.referencedoc.*;
import org.jfree.report.modules.misc.survey.*;
import org.jfree.report.modules.misc.tablemodel.*;
import org.jfree.report.modules.output.csv.*;
import org.jfree.report.modules.output.meta.*;
import org.jfree.report.modules.output.pageable.base.*;
import org.jfree.report.modules.output.pageable.base.operations.*;
import org.jfree.report.modules.output.pageable.base.output.*;
import org.jfree.report.modules.output.pageable.base.pagelayout.*;
import org.jfree.report.modules.output.pageable.graphics.*;
import org.jfree.report.modules.output.pageable.pdf.*;
import org.jfree.report.modules.output.pageable.plaintext.*;
import org.jfree.report.modules.output.support.itext.*;
import org.jfree.report.modules.output.support.pagelayout.*;
import org.jfree.report.modules.output.table.base.*;
import org.jfree.report.modules.output.table.csv.*;
import org.jfree.report.modules.output.table.html.*;
import org.jfree.report.modules.output.table.html.metaelements.*;
import org.jfree.report.modules.output.table.html.ref.*;
import org.jfree.report.modules.output.table.html.util.*;
import org.jfree.report.modules.output.table.rtf.*;
import org.jfree.report.modules.output.table.rtf.metaelements.*;
import org.jfree.report.modules.output.table.xls.*;
import org.jfree.report.modules.output.table.xls.metaelements.*;
import org.jfree.report.modules.output.table.xls.util.*;
import org.jfree.report.modules.output.xml.*;
import org.jfree.report.modules.parser.base.*;
import org.jfree.report.modules.parser.base.common.*;
import org.jfree.report.modules.parser.ext.*;
import org.jfree.report.modules.parser.ext.factory.datasource.*;
import org.jfree.report.modules.parser.ext.factory.elements.*;
import org.jfree.report.modules.parser.ext.factory.objects.*;
import org.jfree.report.modules.parser.ext.factory.stylekey.*;
import org.jfree.report.modules.parser.ext.factory.templates.*;
import org.jfree.report.modules.parser.ext.readhandlers.*;
import org.jfree.report.modules.parser.extwriter.*;
import org.jfree.report.modules.parser.simple.*;
import org.jfree.report.modules.parser.simple.readhandlers.*;
import org.jfree.report.resourceloader.*;
import org.jfree.report.states.*;
import org.jfree.report.style.*;
import org.jfree.report.util.*;
import org.jfree.report.util.beans.*;
import org.jfree.report.util.geom.*;
import org.jfree.report.util.serializers.*;
*/



import org.apache.poi.hssf.usermodel.*;
/*
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.SQLException;
*/

import com.sleepycat.dbxml.*;

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

	JMenuItem menuManage = null;

	JMenuItem menuExpPrn= null;

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
	
	private JPanel chartPanel = null;
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

	static XMLDB xmlDB;

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
		FileChooserDemo f = null;
		try {
			f = new FileChooserDemo("ModelGUI");
			//f.pack();
			f.setVisible(true);
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			if(f.xmlDB != null) {
				System.out.println("FINALLY");
				f.xmlDB.closeDB();
			}
		}
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
			//DocumentType DOCTYPE = impl.createDocumentType("recent", "", "");
			//lastDoc = impl.createDocument("", "recent", DOCTYPE);
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
			aNode = lastDoc.createElement("queries");
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
			aNode = lastDoc.createElement("queries");
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
		menuItem = new JMenuItem("DB Open");
		menuItem.addActionListener(this);
		submenu.add(menuItem);
		m.add(submenu);

		m.add(menuManage = makeMenuItem("Manage DB"));
		menuManage.setEnabled(false);
		m.add(menuExpPrn = makeMenuItem("Export / Print"));
		menuExpPrn.setEnabled(false);

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
		
		// Add adapter to catch window closing event.
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				dispose();
				updateRecentDoc();
				writeFile(recentFile, lastDoc); // NEWLY ADDED !!!!!!!!!!!
				if(xmlDB != null) {
					xmlDB.closeDB();
				}
				System.exit(0);
			}
		});

		this.getGlassPane().addMouseListener( new MouseAdapter() {});
		this.getGlassPane().setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

	}

	/**
	 * Creates a new JTree with the current doc, then sets up a splitPane to
	 * hold the JTree on the left, and and empty pane for future tables of the
	 * right. Also creates the listener for the JTree so right click options can
	 * be handled.
	 */
	public void displayJtree() {
		Container contentPane = getContentPane();
		contentPane.removeAll();
		if(xmlDB != null) {
			xmlDB.closeDB();
			menuManage.setEnabled(false);
			menuExpPrn.setEnabled(false);
		}
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
			setTitle("["+file+"] - ModelGUI");

		} else if (command.equals("DB Open")) {
			JFileChooser fc = new JFileChooser();
			fc.setDialogTitle("Choose XML Database");

			// Choose only files, not directories
			fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

			// Start in current directory
			fc.setCurrentDirectory(globalFC.getCurrentDirectory());

			fc.setFileFilter(new javax.swing.filechooser.FileFilter() {
				public boolean accept(File f) {
					return f.getName().toLowerCase().endsWith(".dbxml") || f.isDirectory();
				}
				public String getDescription() {
					return "BDB XML Container (*.dbxml)";
				}
			});

			// Now open chooser
			int result = fc.showOpenDialog(this);
			if( result == JFileChooser.APPROVE_OPTION ) {
				globalFC.setCurrentDirectory(fc.getCurrentDirectory());
				menuManage.setEnabled(true);
				menuSave.setEnabled(false);
				copyMenu.setEnabled(false);
				pasteMenu.setEnabled(false);
				menuTableFilter.setEnabled(false);
				xmlDB = new XMLDB(fc.getSelectedFile().toString(), this);
				createTableSelector();
				setTitle("["+fc.getSelectedFile()+"] - ModelGUI");
			}

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
			setTitle("["+file+"] - ModelGUI");
		} else if (command.equals("Save")) {
			// Save a file
			status = saveFile();
			if (!status) {
				JOptionPane.showMessageDialog(null,
						"IO error in saving file!!", "File Save Error",
						JOptionPane.ERROR_MESSAGE);
			}
		} else if (command.equals("Export / Print")) {
			System.out.println("HERE");
			createReport();
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
		} else if(command.equals("Manage DB")) {
			manageDB();
		} else if (command.equals("Quit")) {
			dispose();
			updateRecentDoc();
			writeFile(recentFile, lastDoc); // NEWLY ADDED !!!!!!!!!!!
			if(xmlDB != null) {
				xmlDB.closeDB();
			}
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
		if(queries != null) {
			res = (XPathResult)xpeImpl.createExpression("/recent/queries", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
			tempNode = res.iterateNext();
			tempNode.getParentNode().replaceChild(queries.getAsNode(lastDoc), tempNode);
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
			doc = readXMLFile( file);
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

	boolean saveFile() {

		File file = null;
		JFileChooser fc = new JFileChooser();

		// Start in current directory
		fc.setCurrentDirectory(globalFC.getCurrentDirectory());

		// Set filter for xml files.
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
	 * Does the parsing of an XML file, and returns it
	 * 
	 * @param file
	 *            the file that will be parsed
	 * @return the parsed document
	 */
	public Document readXMLFile(File file) {
		try {
			lsInput.setByteStream(new FileInputStream(file));
			lsParser = implls.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);
			lsParser.setFilter(new ParseFilter());
			return lsParser.parse(lsInput);
			//removeEmptyTextNodes(doc.getDocumentElement());
		} catch (Exception e) {
			System.out.println("Got Exception while creating XML document: "
					+ e);
			JOptionPane.showMessageDialog(this,
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


	protected Vector getScenarios() {
		XmlValue temp;
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario");
			while(res.hasNext()) {
				temp = res.next();
				/*
				key = "Doc Name: "+temp.asDocument().getName()+"   Scn Name: "+xmlDB.getAttr(temp, "name")+
					"   Date: "+xmlDB.getAttr(temp, "date");
					*/
				//ret.add(temp.asDocument().getName()+" "+xmlDB.getAttr(temp, "name")+ " "+xmlDB.getAttr(temp, "date"));
				XmlDocument tempDoc = temp.asDocument();
				ret.add(tempDoc.getName()+" "+XMLDB.getAttr(temp, "name")+ " "+XMLDB.getAttr(temp, "date"));
				tempDoc.delete();
				temp.delete();
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		xmlDB.printLockStats("getScenarios");
		return ret;
	}

	protected Vector getRegions() {
		xmlDB.setQueryFunction("distinct-values(");
		Vector ret = new Vector();
		try {
			XmlResults res = xmlDB.createQuery("/scenario/world/region/@name");
			while(res.hasNext()) {
				ret.add(res.next().asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		ret.add("Global");
		xmlDB.setQueryFunction("");
		xmlDB.printLockStats("getRegions");
		return ret;
	}

	protected QueryTreeModel getQueries() {
		Vector ret = new Vector();
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(lastDoc);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/recent/queries", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		return new QueryTreeModel(res.iterateNext());
	}

	protected void createFilteredQuery(Vector scns, int[] scnSel/*, Vector regions, int[]regionSel*/) {
		StringBuffer ret = new StringBuffer("/");
		boolean added = false;
		for(int i = 0; i < scnSel.length; ++i) {
			String[] attrs = ((String)scns.get(scnSel[i])).split("\\s");
			if(!added) {
				ret.append("scenario[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(attrs[1]).append("' and @date='").append(attrs[2]).append("')");
		}
		ret.append(" ]/world/");
		System.out.println(ret);
		xmlDB.setQueryFilter(ret.toString());
	}
	protected Vector scns;
	protected JList scnList;
	protected JList regionList;
	protected Vector regions;
	protected BaseTableModel bt;
	protected JScrollPane jsp;
	protected JSplitPane SP;
	protected QueryTreeModel queries;
	protected void createTableSelector() {
		JPanel listPane = new JPanel();
		JLabel listLabel;
		JPanel allLists = new JPanel();
		final JPanel all = new JPanel();
		scns = getScenarios();
		regions = getRegions();
		queries = getQueries();
		//final JList scnList = new JList(scns);
		scnList = new JList(scns);
		//final JList regionList = new JList(regions);
		regionList = new JList(regions);
		final JTree queryList = new JTree(queries);
		queryList.setSelectionRow(0);
		for(int i = 0; i < queryList.getRowCount(); ++i) {
			queryList.expandRow(i);
		}
		//queryList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		final JSplitPane sp = new JSplitPane();
		sp.setLeftComponent(null);
		sp.setRightComponent(null);

		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Scenario");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
		JScrollPane listScroll = new JScrollPane(scnList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		//listPane.setPreferredSize(new Dimension(150, 250));
		//listPane.add(Box.createVerticalStrut(10));
		//listPane.add(new JSeparator(SwingConstants.HORIZONTAL));

		allLists.setLayout( new BoxLayout(allLists, BoxLayout.X_AXIS));
		allLists.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		allLists.add(listPane);
		//all.add(Box.createHorizontalGlue());
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Regions");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
		listScroll = new JScrollPane(regionList);
		listScroll.setPreferredSize(new Dimension(150, 150));
		listPane.add(listScroll);
		allLists.add(listPane);
		allLists.add(Box.createHorizontalStrut(10));

		listPane = new JPanel();
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		//listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		//listPane.add(Box.createVerticalGlue());
		listLabel = new JLabel("Queries");
		listPane.add(listLabel);
		//listPane.add(Box.createVerticalStrut(10));
		listScroll = new JScrollPane(queryList);
		listScroll.setPreferredSize(new Dimension(150, 100));
		listPane.add(listScroll);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout( new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.add(Box.createHorizontalGlue());
		JButton createButton = new JButton("Create");
		JButton removeButton = new JButton("Remove");
		final JButton runQueryButton = new JButton("Query");
		final JButton editButton = new JButton("Edit");
		editButton.setEnabled(false);
		runQueryButton.setEnabled(false);
		buttonPanel.add(createButton);
		buttonPanel.add(removeButton);
		buttonPanel.add(editButton);
		buttonPanel.add(runQueryButton);
		listPane.add(buttonPanel);

		allLists.add(listPane);
		//all.setLayout( new BoxLayout(all, BoxLayout.PAGE_AXIS));
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		//all.setOpaque(false);
		//all.setBackground(new Color(200,200,200));
		all.add(allLists, BorderLayout.PAGE_START);
		JPanel ANGRY = new JPanel();
		ANGRY.setLayout( new BoxLayout(ANGRY, BoxLayout.X_AXIS));
		ANGRY.add(sp);
		ANGRY.add(Box.createHorizontalGlue());
		all.add(ANGRY);


		queryList.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
				if(queries.isLeaf(e.getPath().getLastPathComponent())) {
					editButton.setEnabled(true);
					runQueryButton.setEnabled(true);
				} else {
					editButton.setEnabled(false);
					runQueryButton.setEnabled(false);
				}
			}
		});

		createButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(thisFrame, "Please select a Query or Query Group before createing", 
						"Create Query Error", JOptionPane.ERROR_MESSAGE);
					return;
				}

				QueryGenerator qg = new QueryGenerator(thisFrame); 
				if(qg.getXPath().equals("")) {
					return;
				} else if(qg.getXPath().equals("Query Group")) {
					queries.add(queryList.getSelectionPath(), qg.toString());
				} else {
					queries.add(queryList.getSelectionPath(), qg);
				}
				queryList.updateUI();
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					// error none selected
					JOptionPane.showMessageDialog(thisFrame, "Please select a Query or Query Group to Remove", 
						"Query Remove Error", JOptionPane.ERROR_MESSAGE);
				} else {
					queries.remove(queryList.getSelectionPath());
					queryList.updateUI();
				}
			}
		});
		runQueryButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int[] scnSel = scnList.getSelectedIndices();
				int[] regionSel = regionList.getSelectedIndices();
				if(scnSel.length == 0) {
					// error
					JOptionPane.showMessageDialog(thisFrame, "Please select Scenarios to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					//batchQuery(new File("bq.xml"), new File("c:\\test.xls"));
				} else if(regionSel.length == 0) {
					JOptionPane.showMessageDialog(thisFrame, "Please select Regions to run the query against", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(thisFrame, "Please select a query to run", 
						"Run Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else {
					createFilteredQuery(scns, scnSel/*, regions, regionSel*/);
					// table stuff
					//System.out.println(queries.get(queryList.getSelectedIndex()));
					//QueryGenerator qg = (QueryGenerator)queries.get(queryList.getSelectedIndex());
					QueryGenerator qg = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
						thisFrame.getGlassPane().setVisible(true);
					if(qg.isGroup()) {
						bt = new MultiTableModel(qg, regionList.getSelectedValues(), thisFrame);
			jTable = new JTable(bt);
	  		jTable.getModel().addTableModelListener((FileChooserDemo)thisFrame);

			//jTable.setAutoResizeMode(JTABLE.AUTO_RESIZE_OFF);

			jTable.setCellSelectionEnabled(true);
			jTable.getColumnModel().getColumn(0).setCellRenderer(((MultiTableModel)bt).getCellRenderer(0,0));
			jTable.getColumnModel().getColumn(0).setCellEditor(((MultiTableModel)bt).getCellEditor(0,0));
			int j = 1;
			while( j < jTable.getRowCount()) {
				jTable.setRowHeight(j,200);
				j += 2;
			}
			//jTable.setRowHeight(200);
			CopyPaste copyPaste = new CopyPaste( jTable );
			jsp = new JScrollPane(jTable);
			sp.setLeftComponent(jsp);
			sp.setDividerLocation(((FileChooserDemo)thisFrame).getWidth());
			System.out.println("Should be displaying");
				thisFrame.setVisible(true);
				//menuSave.setEnabled(true);
				menuExpPrn.setEnabled(true);
						thisFrame.getGlassPane().setVisible(false);
						return;
					}
			//BaseTableModel bt = new ComboTableModel((QueryGenerator)queries.get(queryList.getSelectedIndex()), thisFrame);
			bt = new ComboTableModel(qg, regionList.getSelectedValues(), thisFrame);
			JFreeChart chart = bt.createChart(0,0);
			//TableSorter sorter = new TableSorter(bt);
			jTable = new JTable(bt);
			// Should the listener be set like so..
			jTable.getModel().addTableModelListener((FileChooserDemo)thisFrame);
	  		//sorter.setTableHeader(jTable.getTableHeader());

			jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

			javax.swing.table.TableColumn col;
			int j = 0;
			while(j < jTable.getColumnCount()) {
				col = jTable.getColumnModel().getColumn(j);
				if(jTable.getColumnName(j).equals("")) {
					col.setPreferredWidth(75);
				} else {
					col.setPreferredWidth(jTable.getColumnName(j).length()*5+30);
				}
				j++;
			}
						BufferedImage chartImage = chart.createBufferedImage(
								350, 350);

						JLabel labelChart = new JLabel();
						labelChart.setIcon(new ImageIcon(chartImage));
			//all.add(new JScrollPane(jTable));
			//JSplitPane sp = new JSplitPane();
						/*
						JSplitPane tempSP = new JSplitPane();
			tempSP.setLeftComponent(new JScrollPane(jTable));
			tempSP.setRightComponent(labelChart);
						tempSP.setDividerLocation(((FileChooserDemo)thisFrame).getWidth()-350-15);
						jsp = new JScrollPane(tempSP);
						*/
			sp.setLeftComponent(new JScrollPane(jTable));
			sp.setRightComponent(labelChart);
						sp.setDividerLocation(((FileChooserDemo)thisFrame).getWidth()-350-15);
						SP = sp;
						//jsp = new JScrollPane(sp);
						//all.setAlignmentY(Component.LEFT_ALIGNMENT);
						//all.add(Box.createVerticalStrut(10));
			//thisFrame.getContentPane().remove(all);
						//all.add(sp, BorderLayout.CENTER);
						/*
			thisFrame.getContentPane().add(new JScrollPane(sp), BorderLayout.PAGE_START);
						System.out.println(""+thisFrame.getContentPane().getComponentCount());
						System.out.println(thisFrame.getComponent(0));
						*/
				thisFrame.setVisible(true);
				//menuSave.setEnabled(true);
				menuExpPrn.setEnabled(true);
						thisFrame.getGlassPane().setVisible(false);
				}
			}
		});

		editButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if(queryList.getSelectionCount() == 0) {
					JOptionPane.showMessageDialog(thisFrame, "Please select a query to edit", 
						"Edit Query Error", JOptionPane.ERROR_MESSAGE);
					// error
				} else {
					QueryGenerator tempQG = (QueryGenerator)queryList.getSelectionPath().getLastPathComponent();
					String oldTitle = tempQG.editDialog();
				}
			}
		});




				Container contentPane = getContentPane();
				if (splitPane != null) {
					contentPane.remove(splitPane);
				}
				contentPane.add(new JScrollPane(all), BorderLayout.PAGE_START);
				//contentPane.add(new JScrollPane(all));
				this.setVisible(true);
	}
	private void manageDB() {
		final JDialog filterDialog = new JDialog(this, "Manage Database", true);
		JPanel listPane = new JPanel();
		JPanel buttonPane = new JPanel();
		JButton addButton = new JButton("Add");
		JButton removeButton = new JButton("Remove");
		JButton doneButton = new JButton("Done");
		listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
		Container contentPane = filterDialog.getContentPane();

		//Vector scns = getScenarios();
		final JList list = new JList(scns);

		addButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser fc = new JFileChooser();
				fc.setDialogTitle("Open XML File");

				// Choose only files, not directories
				fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

				// Start in current directory
				fc.setCurrentDirectory(globalFC.getCurrentDirectory());

				// Set filter for Java source files.
				fc.setFileFilter(xmlFilter);

				// Now open chooser
				int result = fc.showOpenDialog(thisFrame);

				if (result == JFileChooser.APPROVE_OPTION) {
					globalFC.setCurrentDirectory(fc.getCurrentDirectory());
					thisFrame.getGlassPane().setVisible(true);
					xmlDB.addFile(fc.getSelectedFile().toString());
					scns = getScenarios();
					list.setListData(scns);
					thisFrame.getGlassPane().setVisible(false);
				}
			}
		});
		removeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Object[] remList = list.getSelectedValues();
				for(int i = 0; i < remList.length; ++i) {
					xmlDB.removeDoc(((String)remList[i]).substring(0, 
							((String)remList[i]).indexOf(' ')));
					//System.out.println(((String)remList[i]).substring(0, ((String)remList[i]).indexOf(' ')));
				}
				scns = getScenarios();
				list.setListData(scns);
			}
		});
		doneButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				xmlDB.addVarMetaData(thisFrame);
				scnList.setListData(scns);
				regions = getRegions();
				regionList.setListData(regions);
				filterDialog.setVisible(false);
			}
		});

		buttonPane.setLayout( new BoxLayout(buttonPane, BoxLayout.X_AXIS));
		buttonPane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
		buttonPane.add(addButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(removeButton);
		buttonPane.add(Box.createHorizontalStrut(10));
		buttonPane.add(doneButton);
		buttonPane.add(Box.createHorizontalGlue());

		JScrollPane sp = new JScrollPane(list);
		sp.setPreferredSize(new Dimension(300, 300));
		listPane.add(new JLabel("Scenarios is Database:"));
		listPane.add(Box.createVerticalStrut(10));
		listPane.add(sp);
		listPane.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(listPane, BorderLayout.PAGE_START);
		contentPane.add(buttonPane, BorderLayout.PAGE_END);
		filterDialog.pack();
		filterDialog.setVisible(true);
	}
	public void setEnableManageDB(boolean enable) {
		menuManage.setEnabled(enable);
	}
	public void createReport() {
		if(jTable == null || jsp == null || jTable.getRowCount() == 0) {
			// error
			return;
		}
		JFreeReportBoot.getInstance().start();
		JFreeReport report = new JFreeReport();
		java.awt.print.PageFormat pageFormat = new java.awt.print.PageFormat();
		pageFormat.setOrientation(java.awt.print.PageFormat.LANDSCAPE);
		report.setPageDefinition(new org.jfree.report.SimplePageDefinition(pageFormat));
		DrawableFieldElementFactory factory = new DrawableFieldElementFactory();
		Group g = new Group();
		float div = 1;
		int numRows = 1;
		if(jTable.getModel() instanceof MultiTableModel) {
			numRows = (int)jTable.getRowCount()/2;
			div = (float)(jTable.getRowCount()/2);
		} else {
			jsp.getVerticalScrollBar().setMaximum((int)SP.getPreferredSize().getHeight());
			jsp.getHorizontalScrollBar().setMaximum((int)SP.getPreferredSize().getWidth());
			jsp.setViewportView(SP);
		}
		factory.setAbsolutePosition(new Point2D.Float(0, 0));
		//factory.setMinimumSize(new FloatDimension((float)800, (float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div))));
		//factory.setMaximumSize(new FloatDimension((float)800, (float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div))));
		//System.out.println("JSP MH: "+jsp.getVerticalScrollBar().getMaximum());
		//System.out.println("SP PF: "+SP.getPreferredSize());
		//System.out.println("H: "+(float)((jsp.getVerticalScrollBar().getMaximum()) /div));
		//System.out.println("Rows: "+jTable.getRowCount());
		/*
		System.out.println("Total Before: "+jTable.getPreferredSize());
		System.out.println("H before: "+(float)(jTable.getPreferredSize().getHeight()/(jTable.getRowCount()/div)));
		*/
		factory.setMinimumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
				//		/(jTable.getRowCount()/div))));
		factory.setMaximumSize(new FloatDimension((float)800, (float)(jsp.getVerticalScrollBar().getMaximum()/div)));
				//		/(jTable.getRowCount()/div))));
		factory.setFieldname("0");
		g.addField("0");
		g.getHeader().addElement(factory.createElement());
		g.getHeader().setPagebreakBeforePrint(true);
		report.addGroup(g);
		Vector fieldList = new Vector(numRows+1);
		fieldList.add("0");
		for(int i = 1; i < numRows; ++i) {
			g = new Group();
			factory.setFieldname(String.valueOf(i));
			fieldList.add(String.valueOf(i));
			g.setFields(fieldList);
			g.getHeader().addElement(factory.createElement());
			g.getHeader().setPagebreakBeforePrint(true);
			report.addGroup(g);
		}


		/*
		Object[] cNames = {"stuff"};
		Object[][] rowData = new Object[1][1];
		rowData[0][0] = (new org.jfree.ui.Drawable() {
			public void draw(java.awt.Graphics2D graphics, java.awt.geom.Rectangle2D bounds) {
				System.out.println("Got dims: "+bounds);
				graphics.scale(.70,.70);
				System.out.println("Pref size: "+jTable.getPreferredSize());
				jTable.printAll(graphics);
				//graphics.transform(java.awt.geom.AffineTransform.getScaleInstance(.5,1));
				System.out.println("has dims: "+graphics.getClipBounds());
				//graphics.clipRect((int)bounds.getMinX(), (int)bounds.getMinY(), (int)bounds.getWidth(), (int)bounds.getHeight());
			}
		});
		report.setData(new javax.swing.table.DefaultTableModel(rowData, cNames)); 
		*/
		report.setData(new javax.swing.table.AbstractTableModel() {
			public int findColumn(String cName) {
				return Integer.parseInt(cName);
			}
			public String getColumnName(int col) {
				return String.valueOf(col);
			}
			public int getColumnCount() {
				return (int)jTable.getRowCount()/2;
			}
			public int getRowCount() {
				return 1;
			}
			public Object getValueAt(int row, int col) {
				final int colf = col;
				return (new org.jfree.ui.Drawable() {
					public void draw(java.awt.Graphics2D graphics, java.awt.geom.Rectangle2D bounds) {
						double scaleFactor = bounds.getWidth() / jsp.getHorizontalScrollBar().getMaximum();
						//double scaleFactor = bounds.getWidth() / SP.getPreferredSize().getWidth();
						//System.out.println("BNDS: "+bounds);
						//System.out.println("SP PF: "+SP.getPreferredSize());
						//System.out.println("SF: "+scaleFactor);
						graphics.scale(scaleFactor, scaleFactor);
							graphics.translate((double)0, 0-bounds.getHeight()*colf);
						if(!(jTable.getModel() instanceof MultiTableModel)) {
							System.out.println("Printing all");
							//graphics.translate((double)0, 0-20-bounds.getHeight()*colf);
							((JScrollPane)SP.getLeftComponent()).printAll(graphics);
						} else {
							jTable.printAll(graphics);
						}

						graphics.setColor(Color.WHITE);
						graphics.fillRect(0, (int)bounds.getHeight()*(1+colf), (int)graphics.getClipBounds().getWidth(), (int)bounds.getHeight());
					}
				});
			}
		});

		try {
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.xls.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.plaintext.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.csv.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.html.Enable", "false");
			report.getReportConfiguration().setConfigProperty("org.jfree.report.modules.gui.rtf.Enable", "false");
			report.getReportConfiguration().setConfigProperty(MyExcelExportPlugin.enableKey, "true");
			ExportPluginFactory epf = ExportPluginFactory.getInstance();
			MyExcelExportPlugin.bt = bt;
			epf.registerPlugin(MyExcelExportPlugin.class, "20", MyExcelExportPlugin.enableKey);
			PreviewDialog preview = new PreviewDialog(report, this, true);
			preview.setTitle(getTitle()+" - Export Preview");
			/*
			preview.addWindowListener(new WindowAdapter() {
				public void windowClosing(final WindowEvent e) {
					e.getWindow().setVisible(false);
				}
			});
			*/
			preview.pack();
			preview.setVisible(true);
			//preview.close();
		} catch(ReportProcessingException e) {
			e.printStackTrace();
		}
	}
	protected void batchQuery(File queryFile, File excelFile) {
		Document queries = readXMLFile( queryFile );
		XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(queries);
		XPathResult res = (XPathResult)xpeImpl.createExpression("/queries/node()", xpeImpl.createNSResolver(queries.getDocumentElement())).evaluate(queries.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		Node tempNode;
		int[] scnSel;
		HSSFWorkbook wb = null;
		HSSFSheet sheet = null;
		QueryGenerator qgTemp = null;
		Vector tempScns = new Vector();
		Vector tempRegions = new Vector();
		if(excelFile.exists()) {
			try {
				wb = new HSSFWorkbook(new FileInputStream(excelFile));
			} catch (IOException ioe) {
				ioe.printStackTrace();
				return;
			}
		}
		if(wb == null) {
			wb = new HSSFWorkbook();
		}
		while((tempNode = res.iterateNext()) != null) {
			tempScns.removeAllElements();
			tempRegions.removeAllElements();
			NodeList nl = tempNode.getChildNodes();
			for(int i = 0; i < nl.getLength(); ++i) {
				Element currEl = (Element)nl.item(i);
				if(currEl.getNodeName().equals("scenario")) {
					tempScns.add("a "+currEl.getAttribute("name")+' '+currEl.getAttribute("date"));
				} else if(currEl.getNodeName().equals("region")) {
					tempRegions.add(currEl.getAttribute("name"));
				} else {
					qgTemp = new QueryGenerator(currEl);
				}
			}
			scnSel = new int[tempScns.size()];
			for(int i = 0; i < scnSel.length; ++i) {
				scnSel[i] = i;
			}
			createFilteredQuery(tempScns, scnSel);
			sheet = wb.createSheet("Sheet"+String.valueOf(wb.getNumberOfSheets()+1));
			if(qgTemp.isGroup()) {
				(new MultiTableModel(qgTemp, tempRegions.toArray(), this)).exportToExcel(sheet, wb, sheet.createDrawingPatriarch());
			} else {
				(new ComboTableModel(qgTemp, tempRegions.toArray(), this)).exportToExcel(sheet, wb, sheet.createDrawingPatriarch());
			}
		}
		try {
			FileOutputStream fos = new FileOutputStream(excelFile);
			wb.write(fos);
			fos.close();
		} catch(IOException ioe) {
			ioe.printStackTrace();
		}
	}
}

