
import org.w3c.dom.*;
import org.w3c.dom.ls.*;
import org.w3c.dom.bootstrap.*;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.apache.xpath.domapi.*;
import org.w3c.dom.xpath.*;
import javax.swing.event.*;
import javax.swing.tree.TreeSelectionModel;


import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.tree.TreePath;



public class FileChooserDemo extends JFrame
	   implements ActionListener, TableModelListener
{

	private Document doc;
	private LSInput lsInput;
	private LSParser lsParser;
	private DOMImplementationLS implls;
	
	int lastFlipX = 0;
	int lastFlipY = 0;

  JMenuItem menuOpenX = null;
  JMenuItem menuOpenC = null;
  JMenuItem menuSave  = null;
  JMenuItem menuClose = null;
  JMenuItem menuTableFilter = null;
  JMenuItem menuTableAdd = null;
  JMenuItem menuTableBuildDemandComp = null;
  JMenuItem menuTableBuildSAM = null;
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

  XMLFilter xmlFilter = new XMLFilter();
  CSVFilter csvFilter = new CSVFilter();
  File file;
  File recentFile = new File("recent.xml");


  JFileChooser globalFC; // for saving last current directory

  //DOMTreeBuilder tree = new DOMTreeBuilder();
  DataTableModel tableModel;
  private Vector tables = null;

  static final int windowHeight = 460;
  static final int leftWidth = 300;
  static final int rightWidth = 340;
  static final int windowWidth = leftWidth + rightWidth;

  public static void main(String [] args)
  {
  	
	  //Schedule a job for the event-dispatching thread:
	  //creating and showing this application's GUI.
	  UIManager u = new UIManager();
	  try {
	  	u.setLookAndFeel(u.getSystemLookAndFeelClassName());
	  } catch (Exception e) {
		System.out.println("Error setting look and feel: "+e);
	  }
	  javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				createAndShowGUI();
			}
	  });

  }

  public static void createAndShowGUI() {
	  // Can pass frame title in command line arguments
	  FileChooserDemo f = new FileChooserDemo("ModelGUI");
	  //f.pack();
	  f.setVisible(true);
	  //f.show();
  }

  // Create a frame with JTextArea and a menubar
  // with a "File" dropdown menu.
  FileChooserDemo(String title)
  {
	super(title);
	
	globalFC = new JFileChooser();
	//globalFC.setCurrentDirectory( new File(".") );

	try {
		System.setProperty(DOMImplementationRegistry.PROPERTY,
			   "org.apache.xerces.dom.DOMImplementationSourceImpl");
		DOMImplementationRegistry reg = DOMImplementationRegistry.newInstance();
		DOMImplementation impl = reg.getDOMImplementation( "XML 3.0" );
		if (impl == null) {
			System.out.println(
				  "Could not find a DOM3 Load-Save compliant parser.");
			JOptionPane.showMessageDialog(this, "Could not find a DOM3 Load-Save compliant parser.", "Initialization Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		implls = (DOMImplementationLS) impl;
		lsInput = implls.createLSInput();
		DocumentType DOCTYPE = impl.createDocumentType("recent", "","");
		lastDoc = impl.createDocument("", "recent", DOCTYPE);
	} catch (Exception e) {
		System.err.println("Couldn't initialize DOMImplementation: "+e);
		JOptionPane.showMessageDialog(this, "Couldn't initialize DOMImplementation\n"+e, "Initialization Error", JOptionPane.ERROR_MESSAGE);
	}
	
	try {
	  lsInput.setByteStream(new FileInputStream(recentFile));
	  lsParser = implls.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS, null);
	  lsParser.setFilter(new ParseFilter());
	  lastDoc = lsParser.parse(lsInput);

	  XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(lastDoc);
	  XPathResult res = (XPathResult)xpeImpl.createExpression("//recent/lastDirectory/node()", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		
	  Node tempNode;
	  while( (tempNode = res.iterateNext()) != null ){
		String pathDirectory = tempNode.getNodeValue();	
		globalFC.setCurrentDirectory( new File( pathDirectory) );
	  }

	} catch (Exception e) {
		System.out.println("exception " + e);
		System.out.println("so that hopefully means no file, so i'll just use default");
		globalFC.setCurrentDirectory( new File(".") );
		
		Node aNode = lastDoc.createElement("lastDirectory");
		aNode.appendChild( lastDoc.createTextNode(".") );
		lastDoc.getDocumentElement().appendChild(aNode);
	}

	Container contentPane = getContentPane();

	// Create a user interface.
	contentPane.setLayout( new BorderLayout() );


	// Use the helper method makeMenuItem
	// for making the menu items and registering
	// their listener.
	JMenu m = new JMenu("File");
	JMenu submenu;
	JMenuItem menuItem;

	//m.add(menuOpenX  = makeMenuItem("Open"));
	//m.add(menuOpenC = makeMenuItem("Open"));


	//a submenu
	m.addSeparator();
	submenu = new JMenu("Open ...");
	submenu.setMnemonic(KeyEvent.VK_S);

	menuItem = new JMenuItem("XML file");
	menuItem.setAccelerator(KeyStroke.getKeyStroke(
			KeyEvent.VK_2, ActionEvent.ALT_MASK));
	menuItem.addActionListener(this);
	submenu.add(menuItem);

	menuItem = new JMenuItem("CSV file");
	menuItem.addActionListener(this);
	submenu.add(menuItem);
	m.add(submenu);


	m.add(menuSave  = makeMenuItem("Save"));
	menuSave.setEnabled(false); // save will first be gray since no file open
	m.add(menuClose = makeMenuItem("Quit"));


	JMenu tableMenu = new JMenu("Table");
	tableMenu.add(menuTableFilter = makeMenuItem("Filter"));
	tableMenu.add(menuTableAdd = makeMenuItem("Add Data"));
	tableMenu.add(menuTableBuildDemandComp = makeMenuItem("Build Demand Components"));
	tableMenu.add(menuTableBuildSAM = makeMenuItem("Build SAM"));
	menuTableFilter.setEnabled(false);
	menuTableAdd.setEnabled(false);
	menuTableBuildDemandComp.setEnabled(false);
	menuTableBuildSAM.setEnabled(false);

	JMenuBar mb = new JMenuBar();
	mb.add(m);
	mb.add(tableMenu);

	setJMenuBar(mb);
	setSize(800,800);

	// Add adapter to catch window closing event.
	addWindowListener( new WindowAdapter() {
		public void windowClosing(WindowEvent e)
		{
		  dispose();
		  updateRecentDoc();
		  writeFile( recentFile, lastDoc ); // NEWLY ADDED !!!!!!!!!!!
		  System.exit(0);
		}
	  }
	);

  }

  public void displayJtree() {
	  Container contentPane = getContentPane();
	  if (splitPane != null ){
	  	contentPane.remove(splitPane);
	  }
	  	// Set up the tree
	  jtree = new JTree(new DOMmodel(doc.getDocumentElement()));
	jtree.setEditable(true);
	jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
	jtree.setShowsRootHandles(true);
	jtree.getModel().addTreeModelListener(new MyTreeModelListener());

	  
  jtree.addTreeSelectionListener(new TreeSelectionListener() {
	  public void valueChanged(TreeSelectionEvent e) {
		  System.out.println("So it changed");
		  jtree.makeVisible(e.getPath());
		  if(jtree.isVisible(e.getPath())) {
		  	System.out.println("is visible");
		  }
		  if(jtree.isPathSelected(e.getPath())) {
			System.out.println("is same");
		  }
		  if(jtree.isExpanded(e.getPath())) {
			System.out.println("is expanded");
		  }
		  /*
		  TreePath[] paths = jtree.getSelectionPaths();
		  if (paths != null) {
				System.out.println("in valueChanged paths! "+e+" --- "+e.getPath());
			  //clearDisplay();
			  //for (int j = 0; j < paths.length; j++) {
				//  AdapterNode node = (AdapterNode)paths[j].getLastPathComponent();
				 // displayValue(node.getText(), false);
			  //}
		  }
		  */
	  }
  });

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
				 selectedPath = jtree.getClosestPathForLocation(e.getX(), e.getY());
				 System.out.println("Path: "+selectedPath);
				 System.out.println("Lead: "+jtree.getLeadSelectionPath());
			   	jtree.setSelectionPath(selectedPath);
				MenuElement[] me = treeMenu.getSubElements();
			        for (int i = 0; i < me.length; i++) {
					if (((JMenuItem)me[i]).getText().equals("Display Table")) {
						if (jtree.getModel().isLeaf(jtree.getLastSelectedPathComponent())) {
							((JMenuItem)me[i]).setEnabled(false);
						} else {
							// HERE TO DISABLE TABLES set false 
							((JMenuItem)me[i]).setEnabled(true);
						}
					}
					if (((JMenuItem)me[i]).getText().equals("Add Child")) {
						Node nodeClicked = ((DOMmodel.DOMNodeAdapter)jtree.getLastSelectedPathComponent()).getNode();

						if ( nodeClicked.getNodeType() == Element.TEXT_NODE ) {
							((JMenuItem)me[i]).setEnabled(false);
						} else {
							// HERE TO DISABLE TABLES set false 
							((JMenuItem)me[i]).setEnabled(true);
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
	  treeView.setPreferredSize(new Dimension( leftWidth, windowHeight ));

	  //jTable = new JTable();
          JScrollPane tableView = new JScrollPane(/*jTable*/);
          //tableView.setPreferredScrollableViewportSize( new Dimension ( rightWidth, windowHeight ));
	  tableView.setPreferredSize( new Dimension (rightWidth, windowHeight));

	  // Build split-pane view
	   splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, treeView, tableView);
	   splitPane.setContinuousLayout( true );
	   splitPane.setDividerLocation( leftWidth );
	   splitPane.setPreferredSize( new Dimension( windowWidth + 10, windowHeight+10 ));
	   // Add GUI components
	   contentPane.add("Center", splitPane );

	   treeMenu = makePopupTreeMenu();
	   
	   //create the dialog for adding new node children, but leave invisible
	   makeAddChildDialog();

	   menuTableBuildDemandComp.setEnabled(true);
	   menuTableBuildSAM.setEnabled(true);

	   //this.show();
	   //this.pack();
	   this.setVisible(true);
   }

  // Process events from the chooser.
  public void actionPerformed( ActionEvent e )
  {
	boolean status = false;
	String command = e.getActionCommand();
	if ( command.equals("XML file") ){
		// Open a file
		status = openXMLFile();
		if( !status){
			JOptionPane.showMessageDialog( null, "Error opening file!",
				"File Open Error", JOptionPane.ERROR_MESSAGE);
		}
		if (doc == null) {
			//probably the cancell, just return here to avoid exceptions
			return;
		}
		displayJtree();
		menuSave.setEnabled(true); // now save can show up

	}
	else if(command.equals("CSV file") ){
		// Open a file
		status = openCSVFile();
		if( !status){
			JOptionPane.showMessageDialog( null, "Error opening file!",
				"File Open Error", JOptionPane.ERROR_MESSAGE);
		}
		if (doc == null) {
			//probably the cancell, just return here to avoid exceptions
			return;
		}
		displayJtree();
		menuSave.setEnabled(true); // now save can show up
	}
	else if(command.equals("Save") ){
		// Save a file
		status = saveFile();
		if( !status){
			JOptionPane.showMessageDialog( null, "IO error in saving file!!",
				"File Save Error", JOptionPane.ERROR_MESSAGE);
		}
		//remember the cancel case if stuff is added here in the future
	}
	else if (command.equals("Filter")) {
		//tableModel.updateDataFilter(this);
		((MultiTableModel)jTable.getModel()).filterData(this);
	}
	else if (command.equals("Build Demand Components")) {
		buildDemandComponents();
	}
	else if (command.equals("Build SAM")) {
		buildSAM();
	}
	else if(command.equals("Quit") ){
		dispose();
		updateRecentDoc();
		writeFile( recentFile, lastDoc ); // NEWLY ADDED !!!!!!!!!!!
	}
  }
  
  public void updateRecentDoc(){ // updates recentDoc with most current globalFC
	XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(lastDoc);
	XPathResult res = (XPathResult)xpeImpl.createExpression("//recent/lastDirectory/node()", xpeImpl.createNSResolver(lastDoc.getDocumentElement())).evaluate(lastDoc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		
	Node tempNode;
	while( (tempNode = res.iterateNext()) != null ){
	  tempNode.setNodeValue( globalFC.getCurrentDirectory().toString() );
	  System.out.println("just updated the recent doc!");
	}
  }
  
  // newest table stuff ...
  public ArrayList chooseTableHeaders( TreePath path ){
	final ArrayList selected = new ArrayList();

	final Object[] itemsObjs = path.getPath();
	String[] items = new String[ itemsObjs.length ];
	for(int i=0; i<itemsObjs.length; i++){
		Node currNode = ((DOMmodel.DOMNodeAdapter)itemsObjs[i]).getNode();
		if( currNode.hasAttributes() ){ // print out first attribute, just to help
			NamedNodeMap nodeMap = currNode.getAttributes();
			items[i] = currNode.getNodeName() + " (" + nodeMap.item(0).getNodeName() + " = " + nodeMap.item(0).getNodeValue() + ")"; 
		}else{
			items[i] = currNode.getNodeName();
		}
	}
	//String[] items = { "one", "two", "three", "four" };
	final JList list = new JList(items);
	list.setSelectionMode(DefaultListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
	JScrollPane scrollingList = new JScrollPane(list);
    		
	final JDialog filterDialog = new JDialog(this, "Please choose two headers:", true);
	filterDialog.setSize(500,400);
	filterDialog.setLocation(100,100);
	filterDialog.setResizable(false);
	
	final JButton nextButton = new JButton(" Finished With Selection ");
	nextButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			System.out.println("pushed next button");
			int[] selectedIndices = list.getSelectedIndices();
			if( selectedIndices.length == 2 ){
				selected.add( itemsObjs[ selectedIndices[0] ] );
				selected.add( itemsObjs[ selectedIndices[1] ] );
				filterDialog.dispose();
			}else{
				// make user try again ..
				JOptionPane.showMessageDialog(null, "Error: You must choose exactly two (2)!");
			}
		}
	});

	JPanel buttonPane = new JPanel();
	buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
	buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	buttonPane.add(Box.createHorizontalGlue());
	buttonPane.add(nextButton);
	buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));

	final JLabel listLabel = new JLabel();
	listLabel.setHorizontalAlignment(JLabel.LEFT);

	JPanel listPane = new JPanel();
	listPane.setLayout( new BoxLayout(listPane, BoxLayout.Y_AXIS));
	listPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	listPane.add(Box.createVerticalGlue());
	listPane.add(listLabel);
	listPane.add(Box.createVerticalStrut(10));
	JScrollPane listScroll = new JScrollPane(list);
	listScroll.setPreferredSize(new Dimension(150, 750));
	listPane.add(listScroll);
	listPane.add(Box.createVerticalStrut(10));
	listPane.add(new JSeparator(SwingConstants.HORIZONTAL));

	Container filterContent = filterDialog.getContentPane();
	//filterContent.add(new JSeparator(SwingConstants.HORIZONTAL));
	filterContent.add(listPane, BorderLayout.CENTER);
	filterContent.add(buttonPane, BorderLayout.PAGE_END);
	filterDialog.setContentPane(filterContent);
	filterDialog.show();
   		
  	return selected; //arraylist with the two selected nodes
  }
  // end newest table stuff



  // This "helper method" makes a menu item and then
  // registers this object as a listener to it.
  private JMenuItem makeMenuItem(String name)
  {
	JMenuItem m = new JMenuItem( name );
	m.addActionListener( this );
	return m;
  }

  private JPopupMenu makePopupTreeMenu() {
	   treeMenu = new JPopupMenu();
	   JMenuItem menuItem = new JMenuItem("Display Table");
	   menuItem.addMouseListener(new MouseListener() {
		   public void mouseReleased(MouseEvent e) {
			if (!jtree.getModel().isLeaf(jtree.getLastSelectedPathComponent())) {
			   Node temp = ((DOMmodel.DOMNodeAdapter)jtree.getLastSelectedPathComponent()).getNode();
			   // HERE TO DISABLE TABLE remark function call
			   //buildTable(treePathtoXPath(selectedPath, temp, 0)); 
			   tables = null;
			   Object[] path = selectedPath.getPath();
	   		   wild = chooseTableHeaders(selectedPath);
	   		   wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
	   		   wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		       	   buildRegionYearTable(treePathtoXPath(selectedPath, temp, 0)); 
			   /*
			   wild.add(null);
			   for(int i = 0; i < path.length; i++) {
				String curr = ((DOMmodel.DOMNodeAdapter)path[i]).getNode().getNodeName();
				if( ((DOMmodel.DOMNodeAdapter)path[i]).getNode().hasAttributes() && 
					!curr.equals((String)wild.get(0)) && !curr.equals((String)wild.get(1))) {
					wild.set(2, curr);
			   		buildRegionYearTable(treePathtoXPath(selectedPath, temp, 0)); 
				}
			   }
			   */
			   MultiTableModel multiTable = new MultiTableModel(tables, filterMaps);
			   jTable = new JTable(multiTable);
			   jTable.getColumnModel().getColumn(0).setCellRenderer(multiTable.getCellRenderer(0,0));
			   jTable.getColumnModel().getColumn(0).setCellEditor(multiTable.getCellEditor(0,0));
			   jTable.setRowHeight(200);
	  		   menuTableFilter.setEnabled(true);
			   //jTable.setPreferredScrollableViewportSize(jTable.getPreferredScrollableViewportSize());
	  // putting flip code here
	  
		tableMenu = makePopupTableMenu();

		//	listen for right click on the table
	   jTable.addMouseListener(new MouseAdapter() {
		   public void mousePressed(MouseEvent e) {
			   maybeShowPopup(e);
		   }
		   public void mouseReleased(MouseEvent e) {
			   maybeShowPopup(e);
		   }
		   private void maybeShowPopup(MouseEvent e) {
			   if (e.isPopupTrigger()) {
				   //selectedPath = jtree.getClosestPathForLocation(e.getX(), e.getY());
				  MenuElement[] me = tableMenu.getSubElements();
					  for (int i = 0; i < me.length; i++) {
					  if (((JMenuItem)me[i]).getText().equals("Flip")) {
							System.out.println("Flip menu activated");
							System.out.println("flipped original x and y are " + e.getX() + " " + e.getY());
							lastFlipX = e.getX();
							lastFlipY = e.getY();
					  }
				  }
				  tableMenu.show(e.getComponent(), e.getX(), e.getY());
			   }
		   }
	   });
			   JScrollPane tableView = new JScrollPane(jTable);
	  		   splitPane.setRightComponent(tableView);
			   //jtree.setSelectionPath(selectedPath);
               System.out.println("RIGHT CLICKED DISPLAY TABLE!");
			   //showAddChildDialog();
			}
		   }
		   public void mouseClicked(MouseEvent e) {
			   //shouldn't the action go here
	   	   }
		   public void mousePressed(MouseEvent e) {}
		   public void mouseEntered(MouseEvent e) {}
		   public void mouseExited(MouseEvent e) {}
	   });
	   treeMenu.add(menuItem);
	   treeMenu.add(new JSeparator());
	   menuItem = new JMenuItem("Add Child");
	   menuItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
			   jtree.setSelectionPath(selectedPath);

			   Node nodeClicked = ((DOMmodel.DOMNodeAdapter)jtree.getLastSelectedPathComponent()).getNode();
			   if( nodeClicked.getNodeType() != Element.TEXT_NODE ){ // can't add child to text node
					System.out.println("RIGHT CLICKED ADD CHILD!");
			   		showAddChildDialog();
			   }

			}
			public void mouseClicked(MouseEvent e) {}
			public void mousePressed(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}
		});
	   treeMenu.add(menuItem);
	   
	  treeMenu.add(new JSeparator());
	  menuItem = new JMenuItem("Delete Node");
	  menuItem.addMouseListener(new MouseListener() {
		  public void mouseReleased(MouseEvent e) {
			  jtree.setSelectionPath(selectedPath);
              System.out.println("RIGHT CLICKED DELETE NODE");
			  deleteNode();
		  }
		  public void mouseClicked(MouseEvent e) {}
		  public void mousePressed(MouseEvent e) {}
		  public void mouseEntered(MouseEvent e) {}
		  public void mouseExited(MouseEvent e) {}
	  });
	  treeMenu.add(menuItem);

	   return treeMenu;
   }
  
  // new code for 'flip' ..
  private JPopupMenu makePopupTableMenu() {
		tableMenu = new JPopupMenu();
		JMenuItem menuItem = new JMenuItem("Flip");
		menuItem.addMouseListener(new MouseListener() {
			public void mouseReleased(MouseEvent e) {
				System.out.println("RIGHT CLICKED FLIP!, do stuff HERE...");
				e.translatePoint( lastFlipX, lastFlipY );
				Point p = e.getPoint();
				int row = jTable.rowAtPoint( p );
				int col = jTable.columnAtPoint( p );
				System.out.println("Source: "+e.getSource()+" Point:"+p+" row: "+row+" col: "+col);
				
				((NewDataTableModel)((JTable)((JScrollPane)jTable.getValueAt(row, col)).getViewport().getView()).getModel()).flip();
				// CALL FLIP METHOD HERE !!!!!
			}
			public void mouseClicked(MouseEvent e) {
				//shouldn't the action go here
			}
			public void mousePressed(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}
		});
		tableMenu.add(menuItem);
		return tableMenu; }
  // end new code for flip ..
  
  
  private void removeEmptyTextNodes(Node curr) {
	  NodeList nl = curr.getChildNodes();
	  Node temp;
	  Vector deleteList = new Vector();
	  for (int i = 0; i < nl.getLength(); i++) {
		  temp = nl.item(i);
		  if (temp.getNodeType() == Node.TEXT_NODE && temp.getNodeValue().matches("\\s*")) {
			  //curr.removeChild(temp);
			  //nl = curr.getChildNodes();
			  //i = -1;
			  deleteList.add(temp);
		  } else {
			  removeEmptyTextNodes(temp);
		  }
	  }
	  for (int i = 0; i < deleteList.size(); i++) {
		  curr.removeChild((Node)deleteList.get(i));
	  }
  }

   private void deleteNode() {
	   //Node currNode = ((DOMmodel.DOMNodeAdapter)selectedPath.getLastPathComponent()).getNode();
	   String message = "Are you sure you want to delete this node";
        
	   if (jtree.getModel().isLeaf(selectedPath.getLastPathComponent())) {
		   message += "?";
	   } else {
		   message += " and all of its children?";
	   }
        
	   int ans = JOptionPane.showConfirmDialog(this, message, "Delete Node", JOptionPane.YES_NO_OPTION);
        
	   if (ans == JOptionPane.NO_OPTION) return;
        
	   //delete the node
	   ((DOMmodel)jtree.getModel()).removeNodeFrom(selectedPath);   
   }
   
   private void showAddChildDialog() {
		  infoLabel.setText("Adding child to " + selectedPath.getLastPathComponent());
        
		  nameField.setText("");
		  attribField.setText("");
		  valueField.setText("");
        
		  //display possible locations where to add node
        
        
		  addChildDialog.pack();
		  //center above the main window
		  addChildDialog.setLocationRelativeTo(addChildDialog.getParent());
		  addChildDialog.show();
	  }

   /** Creates the dialog box that will be made visible when the user choses to
	* add a child to an esiting node in the tree. */
   public void makeAddChildDialog() {
   	
   		System.out.println("in makeAddChildDialog");
   	
	   addChildDialog = new JDialog(this, "Add Child Node", true);
	   Container content = addChildDialog.getContentPane();
	   content.setLayout(new BoxLayout(addChildDialog.getContentPane(), BoxLayout.X_AXIS));

	   content.add(makeAddNodePanel());
	   content.add(new JSeparator(SwingConstants.VERTICAL));
	   content.add(makeAddChildButtonPanel());
   }

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
       

	   JLabel attribLabel = new JLabel("Node Attribute(s) (optional list in the form name=node name, year=1975)");
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

   private JPanel makeAddChildButtonPanel() {   
	   JPanel buttonPanel = new JPanel();
	   buttonPanel.setLayout(new GridLayout(0,1,5,5));
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
        
	   buttonPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
	   buttonPanel.add(addNodeButton);
	   buttonPanel.add(Box.createRigidArea(new Dimension(0,5)));
	   buttonPanel.add(cancelButton);
	   buttonPanel.add(Box.createVerticalGlue());
        
	   JPanel tempPanel = new JPanel();
	   tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
	   tempPanel.add(new JSeparator(SwingConstants.VERTICAL));
	   tempPanel.add(buttonPanel);
        
	   return buttonPanel;
   }

 private void addChildNode() {
	   //build the new child from info entered into Add Child dialog
	   Node newChild = extractNewChild();
        
	   if(newChild != null){
		   DOMmodel model = (DOMmodel)jtree.getModel();
		   model.addTreeModelListener(new MyTreeModelListener());
		   model.insertNodeInto(newChild, selectedPath);
  		   jtree.scrollPathToVisible(new TreePath(newChild));

       }else{
       		System.out.println("NO CHILD ADDED");
       }

   }

 private ArrayList wild;
 private XPathExpression treePathtoXPath(TreePath tp, Node currNode, int flag) {
           XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
           String pathStr = "";
           Object[] path = tp.getPath();
           Node tempNode;
           for (int i = 0; i < path.length-1; i++) {
	           tempNode= ((DOMmodel.DOMNodeAdapter)path[i]).getNode();
		   if(flag == 0) {
                   	pathStr = pathStr + tempNode.getNodeName() + "/";
		   } else if(flag == 1) {
                   	   pathStr = pathStr + tempNode.getNodeName(); 
			   Vector attrs = getAttrsNoRegionYear(tempNode);
			   if(attrs.size() > 0) {
				   pathStr = pathStr + "[";
			   }
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "(@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"')";
				   if(j < attrs.size()-1) {
					   pathStr = pathStr + " and ";
				   } else { 
					   pathStr = pathStr + "]";
				   }
			   }
			   pathStr = pathStr + "/";
		   }
           }
           pathStr = "//" + pathStr + currNode.getNodeName();
           if (flag == 1 &&currNode.hasAttributes() && !getTextData(currNode).equals("")) {
		   if(flag == 0) {
                   	pathStr = pathStr + "[@" + getOneAttrVal(currNode) + "]";
		   } else if(flag == 1) {
			   Vector attrs = getAttrsNoRegionYear(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "[@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"']";
			   }
			   pathStr = pathStr+ "/node()";
		   }
           }
           else if (flag == 1 && currNode.hasAttributes()) {
		   if (flag == 0) {
                   	pathStr = pathStr + "[@" + getOneAttrVal(currNode) + "]/node()";
		   } else if(flag == 1) {
			   Vector attrs = getAttrsNoRegionYear(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr = pathStr + "@" + ((Node)attrs.get(j)).getNodeName()+"='"+((Node)attrs.get(j)).getNodeValue()+"'";
			   }
			   pathStr = pathStr+ "/node()";
		   }
           }
           else {
                   pathStr = pathStr + "/node()";
           }
	   System.out.println(pathStr);
           return xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(currNode));
 }

 /*
  private XPathExpression treePathtoXPath(TreePath tp, Node currNode) {
	  XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
	  String pathStr;
	  /*
	  String[] path = tp.toString().substring(1, tp.toString().length()-1).split("[,]\\s*");
	  for (int i = path.length-1;  i < path.length; i++) {
		  if (path[i].indexOf(' ') != -1) {
			  String[] temp = path[i].split(" ");
		  	pathStr = temp[0];
			for (int j = 1; j < temp.length; j+=3) {
				pathStr = pathStr+"[@"+temp[j]+temp[j+1]+"'"+temp[j+2]+"']";
			}
		  }
		  else {
		  	pathStr = path[i];
		  }
	  }
	  if (currNode.hasAttributes() && !getTextData(currNode).equals("")) {
	  	pathStr = "//" + currNode.getParentNode().getNodeName() + "/" + currNode.getNodeName() + "[@" + getOneAttrVal(currNode) + "]";
	  } 
	  else if (currNode.hasAttributes()) {
	  	pathStr = "//" + currNode.getParentNode().getNodeName() + "/" + currNode.getNodeName() + "[@" + getOneAttrVal(currNode) + "]/node()";
	  }
	  else {
		  pathStr = "//" + currNode.getNodeName() + "/node()";
	  }
	  return xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(currNode));
  }
  */
  
  // ********* newly added **************
  // for the tablechangedmodel listener
  public void tableChanged(TableModelEvent e) {
	  if(1==1) {
	  	return;
	  }
	  System.out.println("!!! "+e);
	  System.out.println("Source: "+e.getSource()+" type: "+e.getType()+" update: "+TableModelEvent.UPDATE);
	  if(e.getType() != TableModelEvent.UPDATE) {
		  System.out.println("Ignoring changed table");
		  return;
	  }
	  if(e.getSource().toString().matches("TableSorter.*") && e.getType() == TableModelEvent.UPDATE) { // Ignore table sorting events
		  System.out.println("Ignoring table sorting");
		  return;
	  }
	  int row = e.getFirstRow();
	  int column = e.getColumn();
	  //DataTableModel model = (DataTableModel)e.getSource();
	  String columnName = tableModel.getColumnName(column);
	  Object newValue = tableModel.getValueAt(row, column);

	  System.out.println("row is: " + row + " and col is: " + column);
	  System.out.println("newValue is " + newValue + "!!! yay!!!!!!! :)");
	  System.out.println("printing out columnName " + columnName);
	  
	  System.out.println("time to alter the tree.... dun dun dun");
	  
	  System.out.println("printing out 'treepath': " + tableModel.getValueAt(row, tableModel.getColumnCount()));
	  TreePath pathAltered = ((TreePath)tableModel.getValueAt(row, tableModel.getColumnCount()));
	  jtree.getModel().valueForPathChanged(pathAltered, newValue);
	  /*
	  //jtree.setSelectionPath(pathAltered);
	  Node parent = ((DOMmodel.DOMNodeAdapter)pathAltered.getLastPathComponent()).getNode();
	  parent.setNodeValue( newValue.toString() );
	  jtree.setExpandsSelectedPaths(true);
	  jtree.setLeadSelectionPath(pathAltered);
  	  jtree.scrollPathToVisible(pathAltered);
	  jtree.setSelectionPath(pathAltered);
			System.out.println(jtree.getSelectionPath());
			System.out.println(jtree.getLeadSelectionPath());
	  System.out.println("parent "+parent+" has "+parent.getChildNodes().getLength()+" children");
	  NodeList children = parent.getChildNodes();
	  for(int i=0; i < children.getLength(); i++){
		  System.out.println("Searching for child");
		if ( children.item(i).getNodeType() == Element.TEXT_NODE ){
			System.out.println("Found Child");
			//children.item(i).setNodeValue( (String)newValue );
			children.item(i).setNodeValue( newValue.toString() );
			jtree.clearSelection();
			jtree.setSelectionPath(pathAltered.pathByAddingChild( ((DOMmodel)jtree.getModel()).getAdapterNode(children.item(i))) );
			//jtree.setSelectionPath(pathAltered.pathByAddingChild( jtree.getModel().getChild(((DOMmodel)jtree.getModel()).getAdapterNode(parent), i)));
			System.out.println(jtree.getSelectionPath());
			System.out.println(jtree.getLeadSelectionPath());
			if(jtree.getExpandsSelectedPaths()) {
				System.out.println("Should make visible");
			}
		}
	  }
	  */
  }
  
  private TreeMap addToDataTree(Node currNode, TreeMap dataTree) {
	  if (currNode.getNodeType() == Node.DOCUMENT_NODE) {
		  return dataTree;
	  }
	  TreeMap tempMap = addToDataTree(currNode.getParentNode(), dataTree);
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && currNode.getNodeName().equals("subsector") ) {
		  return tempMap;
	  }
	  if(currNode.hasAttributes() && !currNode.getNodeName().equals((String)wild.get(0)) && !currNode.getNodeName().equals((String)wild.get(1))) {
		String attr = currNode.getNodeName()+":"+getOneAttrVal(currNode);
		if(!tempMap.containsKey(attr)) {
			tempMap.put(attr, new TreeMap());
		}
		//((TreeMap)tempMap.get(attr)).put(dataKey, tempNode);
		return (TreeMap)tempMap.get(attr);
	  }
	  return tempMap;
  }

  private void recAddTables(TreeMap dataTree, Map.Entry parent, TreeSet regions, TreeSet years, String title) {
	Iterator it = dataTree.entrySet().iterator();
	while(it.hasNext()) {
		Map.Entry me = (Map.Entry)it.next();
		if(me.getValue() instanceof Node) {
	  		NewDataTableModel tM = new NewDataTableModel(regions, (String)wild.get(0), years, (String)wild.get(1), title+'/'+(String)parent.getKey(), (TreeMap)parent.getValue(),doc); 
	  		jTable = new JTable(tM);
	  		jTable.getModel().addTableModelListener(this);

	  		//jTable = new JTable(tableModel);
	  		jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

	  		javax.swing.table.TableColumn col;
	  		Iterator i = regions.iterator();
	  		int j = 0;
	  		while(i.hasNext()) {
		  		col = jTable.getColumnModel().getColumn(j);
		  		col.setPreferredWidth(((String)i.next()).length()*5+30);
		  		j++;
	  		}
			CopyPaste copyPaste = new CopyPaste( jTable );
	  		JScrollPane tableView = new JScrollPane(jTable);
	  		if(tables == null) {
		  		tables = new Vector();
	  		}
			//tables.add(title+"/"+(String)me.getKey());
			tables.add(title+"/");
	  		tables.add(tableView);
			return;
		} else {
			recAddTables((TreeMap)me.getValue(), me, regions, years, title+'/'+(String)me.getKey());
		}
	}
  }

  private LinkedHashMap filterMaps; // for now HERE
  private void buildRegionYearTable(XPathExpression xpe) {
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  Object[] regionAndYear;
	  TreeSet regions = new TreeSet();
	  TreeSet years = new TreeSet();
	  filterMaps = new LinkedHashMap();
	  //TreeMap data = new TreeMap();
	  //String old3DVar = null;
	  TreeMap dataTree = new TreeMap();
	  //try {
	  	while ((tempNode = res.iterateNext()) != null) {
			regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode(), filterMaps);
			regions.add(regionAndYear[0]);
			years.add(regionAndYear[1]);
			addToDataTree(tempNode, dataTree).put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
			//data.put((String)regionAndYear[0]+(String)regionAndYear[1], tempNode);
			/*
			if(!thrdDimToData.containsKey(regionAndYear[2])) {
				thrdDimToData.put(regionAndYear[2], new TreeMap());
			}
			((TreeMap)thrdDimToData.get(regionAndYear[2])).put((String)regionAndYear[0]+(String)regionAndYear[1], tempNode);
			*/
		}
		recAddTables(dataTree, null, regions, years, "");
		/*
		Iterator it = thrdDimToData.entrySet().iterator();
		while(it.hasNext()) {
			Map.Entry me = (Map.Entry)it.next();
	  		NewDataTableModel tM = new NewDataTableModel(regions, (String)wild.get(0), years, (String)wild.get(1), (String)me.getKey(), (TreeMap)me.getValue(), doc); 
	  		jTable = new JTable(tM);
	  		jTable.getModel().addTableModelListener(this);

	  		//jTable = new JTable(tableModel);
	  		jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

	  		javax.swing.table.TableColumn col;
	  		Iterator i = regions.iterator();
	  		int j = 0;
	  		while(i.hasNext()) {
		  		col = jTable.getColumnModel().getColumn(j);
		  		col.setPreferredWidth(((String)i.next()).length()*5+30);
		  		j++;
	  		}
	  		JScrollPane tableView = new JScrollPane(jTable);
	  		if(tables == null) {
		  		tables = new Vector();
	  		}
	  		tables.add(tableView);
	  		


		}
		*/


	  //Container c = new Container();
	  //c.add(tableView);
	  //JScrollPane tableView = (JScrollPane)splitPane.getRightComponent();
	  /*
	  if(tableView == null) {
		  System.out.println("HERe1");
		  //tableView = new JScrollPane(jTable);
	  } else {
		  System.out.println("HERe2");
	  	//tableView.add(jTable);
		//tableView.validate();
	  }
	  */
	  //splitPane.setRightComponent(tableView);
	  //menuTableFilter.setEnabled(true);
	  /*} catch(Exception e) {
		  if(e.getMessage() == null) {
		  	System.out.println("Error Building table by region and year:"+e);
		  } else {
		  	System.out.println(e.getMessage());
		  }
	  }*/
	  
	  
	   
  }

  private Object[] getRegionAndYearFromNode(Node n, Map filterMaps) {
	  /*
	  Node temp;
	  String region = null;
	  String year = null;
	  */
	  Vector ret = new Vector(2,0);
	  do {
		  if(n.getNodeName().equals((String)wild.get(0)) || n.getNodeName().equals((String)wild.get(1))) {
			  //ret.add(n.getAttributes().getNamedItem("name").getNodeValue());
			  if(!getOneAttrVal(n).equals("fillout=1")) {
			  	ret.add(getOneAttrVal(n));
			  } else {
			        ret.add(getOneAttrVal(n, 1));
			  }

		  } else if(n.hasAttributes()) {
			  HashMap tempFilter;
	           	  if (filterMaps.containsKey(n.getNodeName())) {
	                          tempFilter = (HashMap)filterMaps.get(n.getNodeName());
                          } else {
                                  tempFilter = new HashMap();
                          }
			  String attr = getOneAttrVal(n);
			  if (!tempFilter.containsKey(attr)) {
                          	tempFilter.put(attr, new Boolean(true));
                          	filterMaps.put(n.getNodeName(), tempFilter);
			  }
		  }
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
  }

  private void buildTable(XPathExpression xpe){
	  jTable = null;
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  Vector cols = new Vector();
	  Vector tempVector;
	  Vector rows = new Vector();
	  HashMap filterMaps; 
	  HashMap tempMap;
	  Vector path;
	  if (tableModel == null) {
		  filterMaps = new HashMap();
	  } else {
		  filterMaps = tableModel.getFilterMaps();
	  }
	  while ((tempNode = res.iterateNext()) != null) {
		  if (cols.isEmpty()) {
			  cols = recBuildParentList(tempNode, cols, null);
		  }
		  path = new Vector();
		  tempVector = new Vector();
		  tempVector = recBuildParentList(tempNode, tempVector, path);
		  tempVector.addElement(new TreePath(path.toArray()));
		  for (int i = 0; i < tempVector.size() -2; i++) {
	           	  if (filterMaps.containsKey(cols.get(i))) {
	                          tempMap = (HashMap)filterMaps.get(cols.get(i));
                          } else {
                                  tempMap = new HashMap();
                          }
			  if (!tempMap.containsKey(tempVector.get(i))) {
                          	tempMap.put(tempVector.get(i), new Boolean(true));
                          	filterMaps.put(cols.get(i), tempMap);
			  }
          	  }
		  rows.addElement(tempVector);
	  }
	  tableModel = new DataTableModel(cols, rows, filterMaps, DataTableModel.NORMAL_TABLE, this);
	  TableSorter sorter = new TableSorter(tableModel);
	  jTable = new JTable(sorter);

	  jTable.getModel().addTableModelListener(this);

	  sorter.setTableHeader(jTable.getTableHeader());
	  //jTable = new JTable(tableModel);
	  jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	  
	  jTable.setCellSelectionEnabled(true);

	  javax.swing.table.TableColumn col;
	  for (int i = 0; i < cols.size(); i++) {
		  col = jTable.getColumnModel().getColumn(i);
		  col.setPreferredWidth(((String)cols.get(i)).length()*5+30);
	  }
	  JScrollPane tableView = new JScrollPane(jTable);
	  splitPane.setRightComponent(tableView);
	  menuTableFilter.setEnabled(true);
	  

	  
  }

  private void buildDemandComponents() {
  	XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
	String pathStr = "/scenario/world/regionCGE";
	String currRegionName;
	String currYear;
	String[][] dcTable;
	int pos;
        
	XPathResult res = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	Node tempNode;
	Node tempNodeInner;
	XPathResult resInner;
	String[] colNames = {"year", "region", "Indusrty", "Intermediate Production", "Consumption", "Investment",
		"Government", "Trade", "Total"};
	Vector cols = new Vector();
	Vector rows = new Vector();
	Vector tempVector;
	for(int i = 0; i < colNames.length; i++) {
		cols.add(colNames[i]);
	}
	HashMap filterMaps; 
	HashMap tempMap;
	if (tableModel == null) {
		filterMaps = new HashMap();
	} else {
		filterMaps = tableModel.getFilterMaps();
	}
	colNames = null;
	while ((tempNode = res.iterateNext()) != null) {
		currYear = ((Element)tempNode.getParentNode()).getAttribute("year");
		currRegionName = ((Element)tempNode).getAttribute("name");
		dcTable = new String[24][9];
		initDCTable(dcTable, currYear, currRegionName);

		// get info for production technologies
		pathStr = "./productionSector/subsector/productionTechnology[(@year <= "+currYear+")]/productionInput/demandCurrency/text()";
		int x = 0;
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null );
		while ((tempNodeInner = resInner.iterateNext()) != null) {
			pos = getPosForInputName(((Element)tempNodeInner.getParentNode().getParentNode()).getAttribute("name"));
			dcTable[pos][3] = addString(dcTable[pos][3], tempNodeInner.getNodeValue());
			dcTable[pos][8] = addString(dcTable[pos][8], tempNodeInner.getNodeValue());
			x++;
		}
		System.out.println("this many: "+x);

		// get info for consumers
		pathStr = "./finalDemandSector/subsector/consumer[(@year = "+currYear+")]/demandInput/demandCurrency/text()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null );
		while ((tempNodeInner = resInner.iterateNext()) != null) {
			pos = getPosForInputName(((Element)tempNodeInner.getParentNode().getParentNode()).getAttribute("name"));
			dcTable[pos][4] = addString(dcTable[pos][4], tempNodeInner.getNodeValue());
			dcTable[pos][8] = addString(dcTable[pos][8], tempNodeInner.getNodeValue());
		}

		// get info for govt consumers
		pathStr = "./finalDemandSector/subsector/govtConsumer[(@year = "+currYear+")]/productionInput/demandCurrency/text()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null );
		while ((tempNodeInner = resInner.iterateNext()) != null) {
			pos = getPosForInputName(((Element)tempNodeInner.getParentNode().getParentNode()).getAttribute("name"));
			dcTable[pos][6] = addString(dcTable[pos][6], tempNodeInner.getNodeValue());
			dcTable[pos][8] = addString(dcTable[pos][8], tempNodeInner.getNodeValue());
		}

		// get info for invest consumers
		pathStr = "./finalDemandSector/subsector/investConsumer[(@year = "+currYear+")]/productionInput/demandCurrency/text()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null );
		while ((tempNodeInner = resInner.iterateNext()) != null) {
			pos = getPosForInputName(((Element)tempNodeInner.getParentNode().getParentNode()).getAttribute("name"));
			dcTable[pos][5] = addString(dcTable[pos][5], tempNodeInner.getNodeValue());
			dcTable[pos][8] = addString(dcTable[pos][8], tempNodeInner.getNodeValue());
		}

		// get info for trade consumers
		pathStr = "./finalDemandSector/subsector/tradeConsumer[(@year = "+currYear+")]/productionInput/demandCurrency/text()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null );
		while ((tempNodeInner = resInner.iterateNext()) != null) {
			pos = getPosForInputName(((Element)tempNodeInner.getParentNode().getParentNode()).getAttribute("name"));
			dcTable[pos][7] = addString(dcTable[pos][7], tempNodeInner.getNodeValue());
			dcTable[pos][8] = addString(dcTable[pos][8], tempNodeInner.getNodeValue());
		}

		updateTable(dcTable, rows, cols, filterMaps);

	}
	tableModel = new DataTableModel(cols, rows, filterMaps, DataTableModel.DEMAND_COMPONENTS_TABLE, this);
	TableSorter sorter = new TableSorter(tableModel);
	jTable = new JTable(sorter);

	jTable.getModel().addTableModelListener(this);

	sorter.setTableHeader(jTable.getTableHeader());
	//jTable = new JTable(tableModel);
	jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	  
	jTable.setCellSelectionEnabled(true);

	javax.swing.table.TableColumn col;
	for (int i = 0; i < cols.size(); i++) {
		col = jTable.getColumnModel().getColumn(i);
		col.setPreferredWidth(((String)cols.get(i)).length()*5+30);
	}
	JScrollPane tableView = new JScrollPane(jTable);
	splitPane.setRightComponent(tableView);
	menuTableFilter.setEnabled(true);
  }

  private void updateTable(String[][] table, Vector rows, Vector cols, HashMap filterMaps) {
	  Vector tempVector;
	  HashMap tempMap;
	for(int i =0; i < table.length; i++) {
		tempVector = new Vector();
		for(int j = 0; j< table[i].length; j++) {
			System.out.print(table[i][j]+"\t");
			tempVector.add(table[i][j]);
			if( j <= 2 ) {
           	  		if (filterMaps.containsKey(cols.get(j))) {
                   			tempMap = (HashMap)filterMaps.get(cols.get(j));
                       		} else { 
                       			tempMap = new HashMap();
                       		}
		  		if (!tempMap.containsKey(tempVector.get(j))) {
                       			tempMap.put(tempVector.get(j), new Boolean(true));
                       			filterMaps.put(cols.get(j), tempMap);
		  		}
			}
		}
		rows.add(tempVector);
		System.out.println("");
	}
	System.out.println("");
  }

  private void initDCTable(String[][] dcTable, String currYear, String currRegionName) {
	  for(int i =0; i < dcTable.length; i++) {
		  dcTable[i][0] = currYear;
		  dcTable[i][1] = currRegionName;
		  switch(i) {
			  case 0: dcTable[i][2] = "AgricultureOther"; break;
			  case 18: dcTable[i][2] = "AnimalProducts"; break;
			  case 23: dcTable[i][2] = "Capital"; break;
			  case 11: dcTable[i][2] = "Cement"; break;
			  case 10: dcTable[i][2] = "Chemicals"; break;
			  case 4: dcTable[i][2] = "CoalProduction"; break;
			  case 5: dcTable[i][2] = "CokeProduction"; break;
			  case 2: dcTable[i][2] = "CrudeOilProduction"; break;
			  case 1: dcTable[i][2] = "ETE"; break;
			  case 6: dcTable[i][2] = "ElectricityGeneration"; break;
			  case 20: dcTable[i][2] = "FoodProcessing"; break;
			  case 19: dcTable[i][2] = "Forestry"; break;
			  case 16: dcTable[i][2] = "FrghtTransport"; break;
			  case 17: dcTable[i][2] = "GrainsOilSeeds"; break;
			  case 14: dcTable[i][2] = "IndustriesOther"; break;
			  case 21: dcTable[i][2] = "Labor"; break;
			  case 3: dcTable[i][2] = "NaturalGasProduction"; break;
			  case 8: dcTable[i][2] = "NaturalGasTD"; break;
			  case 13: dcTable[i][2] = "NonFerrousMetals"; break;
			  case 15: dcTable[i][2] = "PassTransport"; break;
			  case 7: dcTable[i][2] = "RefinedOil"; break;
			  case 12: dcTable[i][2] = "Steel"; break;
			  case 9: dcTable[i][2] = "WoodProducts"; break;
			  case 22: dcTable[i][2] = "Land"; break;
		  }
		  for(int j = 3; j < dcTable[i].length; j++) {
			  dcTable[i][j] = "0";
		  }
	  }
  }

  private int getPosForInputName(String name) {
	  if(name.equals("AgricultureOther")) {
		  return 0;
	  } else if (name.equals("AnimalProducts")) {
		  return 18;
	  } else if (name.equals("Capital")) {
		  return 23;
	  } else if (name.equals("Cement")) {
		  return 11;
	  } else if (name.equals("Chemicals")) {
		  return 10;
	  } else if (name.equals("CoalProduction")) {
		  return 4;
	  } else if (name.equals("CokeProduction")) {
		  return 5;
	  } else if (name.equals("CrudeOilProduction")) {
		  return 2;
	  } else if (name.equals("ETE")) {
		  return 1;
	  } else if (name.equals("ElectricityGeneration")) {
		  return 6;
	  } else if (name.equals("FoodProcessing")) {
		  return 20;
	  } else if (name.equals("Forestry")) {
		  return 19;
	  } else if (name.equals("FrghtTransport")) {
		  return 16;
	  } else if (name.equals("FrghtTranport")) { // because it is misspelled in the model's data, leave until fixed there
		  return 16;
	  } else if (name.equals("GrainsOilSeeds")) {
		  return 17;
	  } else if (name.equals("IndustriesOther")) {
		  return 14;
	  } else if (name.equals("Labor")) {
		  return 21;
	  } else if (name.equals("NaturalGasProduction")) {
		  return 3;
	  } else if (name.equals("NaturalGasTD")) {
		  return 8;
	  } else if (name.equals("NonFerrousMetals")) {
		  return 13;
	  } else if (name.equals("PassTransport")) {
		  return 15;
	  } else if (name.equals("RefinedOil")) {
		  return 7;
	  } else if (name.equals("Steel")) {
		  return 12;
	  } else if (name.equals("Land")) {
		  return 22;
	  } else if (name.equals("WoodProducts")) {
		  return 9;
	  }
	  System.out.println("Couldn't find index for "+name);
	  return -1;
  }

  private void buildSAM() {
  	XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
	String pathStr = "/scenario/world/regionCGE";
	String currRegionName;
	String currYear;
	String[][] samTable;
	int pos;

	XPathResult res = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	Node tempNode;
	Node tempNodeInner;
	XPathResult resInner;
	String[] colNames = {"year", "region", "Receipts", "Activities", "Commodities", "Land",
		"Labor", "Capital", "Household", "Enterprises", "Government", "Capital Account", "Rest Of World", "Total"};
	Vector cols = new Vector();
	Vector rows = new Vector();
	Vector tempVector;
	for(int i = 0; i < colNames.length; i++) {
		cols.add(colNames[i]);
	}
	HashMap filterMaps; 
	HashMap tempMap;
	if (tableModel == null) {
		filterMaps = new HashMap();
	} else {
		filterMaps = tableModel.getFilterMaps();
	}
	colNames = null;
	while ((tempNode = res.iterateNext()) != null) {
		currYear = ((Element)tempNode.getParentNode()).getAttribute("year");
		currRegionName = ((Element)tempNode).getAttribute("name");
		samTable = new String[11][14];
		initSAMTable(samTable, currYear, currRegionName);
		pathStr = "./finalDemandSector[@name=\"Household\"]/subsector/consumer[@year="+currYear+"]/Expenditure/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			if(tempNodeInner.getNodeName().equals("consumtion") || tempNodeInner.getNodeName().equals("consumption")) {
				samTable[1][8] = addString(samTable[1][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][8] = addString(samTable[10][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[1][13] = addString(samTable[1][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("transfers")) {
				samTable[5][8] = addString(samTable[5][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][8] = addString(samTable[10][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[5][13] = addString(samTable[5][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("directTaxes")) {
				samTable[7][8] = addString(samTable[7][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][8] = addString(samTable[10][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[7][13] = addString(samTable[7][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("savings")) {
				samTable[8][8] = addString(samTable[8][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][8] = addString(samTable[10][8], tempNodeInner.getFirstChild().getNodeValue());
				samTable[8][13] = addString(samTable[8][13], tempNodeInner.getFirstChild().getNodeValue());
			}
		}

		pathStr = "./finalDemandSector[@name=\"Government\"]/subsector/govtConsumer[@year="+currYear+"]/Expenditure/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			if(tempNodeInner.getNodeName().equals("subsidy")) {
				samTable[0][10] = addString(samTable[0][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][10] = addString(samTable[10][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[0][13] = addString(samTable[0][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("consumption")) {
				samTable[1][10] = addString(samTable[1][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][10] = addString(samTable[10][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[1][13] = addString(samTable[1][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("transfers")) {
				samTable[5][10] = addString(samTable[5][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][10] = addString(samTable[10][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[5][13] = addString(samTable[5][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("savings")) {
				samTable[8][10] = addString(samTable[8][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][10] = addString(samTable[10][10], tempNodeInner.getFirstChild().getNodeValue());
				samTable[8][13] = addString(samTable[8][13], tempNodeInner.getFirstChild().getNodeValue());
			}
		}

		pathStr = "./finalDemandSector[@name=\"Trade\"]/subsector/tradeConsumer[@year="+currYear+"]/Expenditure/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			if(tempNodeInner.getNodeName().equals("totalImports")) {
				samTable[0][12] = addString(samTable[0][12], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][12] = addString(samTable[10][12], tempNodeInner.getFirstChild().getNodeValue());
				samTable[0][13] = addString(samTable[0][13], tempNodeInner.getFirstChild().getNodeValue());
			}
		}

		pathStr = "./finalDemandSector[@name=\"Investment\"]/subsector/investConsumer[@year="+currYear+"]/Expenditure/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			if(tempNodeInner.getNodeName().equals("investment")) {
				samTable[1][11] = addString(samTable[1][11], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][11] = addString(samTable[10][11], tempNodeInner.getFirstChild().getNodeValue());
				samTable[1][13] = addString(samTable[1][13], tempNodeInner.getFirstChild().getNodeValue());
			}
		}

		pathStr = "./productionSector/subsector/productionTechnology[@year <= "+currYear+"]/Expenditure/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			System.out.println("Got results: "+tempNodeInner);
			if(tempNodeInner.getNodeName().equals("intermediateInput")) {
				samTable[1][3] = addString(samTable[1][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][3] = addString(samTable[10][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[1][13] = addString(samTable[1][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("wages")) {
				samTable[3][3] = addString(samTable[3][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][3] = addString(samTable[10][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[3][13] = addString(samTable[3][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("landRents")) {
				samTable[2][3] = addString(samTable[2][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][3] = addString(samTable[10][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[2][13] = addString(samTable[2][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("rentals")) {
				samTable[4][3] = addString(samTable[4][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][3] = addString(samTable[10][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[4][13] = addString(samTable[4][13], tempNodeInner.getFirstChild().getNodeValue());
				samTable[6][7] = addString(samTable[6][7], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][7] = addString(samTable[10][7], tempNodeInner.getFirstChild().getNodeValue());
				samTable[6][13] = addString(samTable[6][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("indirectTaxes")) {
				samTable[7][3] = addString(samTable[7][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][3] = addString(samTable[10][3], tempNodeInner.getFirstChild().getNodeValue());
				samTable[7][13] = addString(samTable[7][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("sales")) {
				samTable[0][4] = addString(samTable[0][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][4] = addString(samTable[10][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[0][13] = addString(samTable[0][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("tariffs")) {
				samTable[7][4] = addString(samTable[7][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][4] = addString(samTable[10][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[7][13] = addString(samTable[7][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("imports")) {
				samTable[9][4] = addString(samTable[9][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][4] = addString(samTable[10][4], tempNodeInner.getFirstChild().getNodeValue());
				samTable[9][13] = addString(samTable[9][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("dividends")) {
				samTable[5][9] = addString(samTable[5][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][9] = addString(samTable[10][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[5][13] = addString(samTable[5][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("directTaxes")) {
				samTable[7][9] = addString(samTable[7][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][9] = addString(samTable[10][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[7][13] = addString(samTable[7][13], tempNodeInner.getFirstChild().getNodeValue());
			}
			else if(tempNodeInner.getNodeName().equals("retainedEarnings")) {
				samTable[8][9] = addString(samTable[8][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[10][9] = addString(samTable[10][9], tempNodeInner.getFirstChild().getNodeValue());
				samTable[8][13] = addString(samTable[8][13], tempNodeInner.getFirstChild().getNodeValue());
			}
		}

		pathStr = "./factorSupply[@name=\"Land\"]/supply/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			System.out.println("Got results");
			samTable[5][5] = addString(samTable[5][5], tempNodeInner.getNodeValue());
			samTable[10][5] = addString(samTable[10][5], tempNodeInner.getNodeValue());
			samTable[5][13] = addString(samTable[5][13], tempNodeInner.getNodeValue());
		}

		pathStr = "./factorSupply[@name=\"Labor\"]/supply/node()";
		resInner = (XPathResult)xpeImpl.createExpression(pathStr, xpeImpl.createNSResolver(doc.getDocumentElement())).evaluate(tempNode, XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
		while((tempNodeInner = resInner.iterateNext()) != null) {
			System.out.println("Got results");
			samTable[5][6] = addString(samTable[5][6], tempNodeInner.getNodeValue());
			samTable[10][6] = addString(samTable[10][6], tempNodeInner.getNodeValue());
			samTable[5][13] = addString(samTable[5][13], tempNodeInner.getNodeValue());
		}
		updateTable(samTable, rows, cols, filterMaps);
	}
	tableModel = new DataTableModel(cols, rows, filterMaps, DataTableModel.SAM, this);
	TableSorter sorter = new TableSorter(tableModel);
	jTable = new JTable(sorter);

	jTable.getModel().addTableModelListener(this);

	sorter.setTableHeader(jTable.getTableHeader());
	//jTable = new JTable(tableModel);
	jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	  
	jTable.setCellSelectionEnabled(true);

	javax.swing.table.TableColumn col;
	for (int i = 0; i < cols.size(); i++) {
		col = jTable.getColumnModel().getColumn(i);
		col.setPreferredWidth(((String)cols.get(i)).length()*5+30);
	}
	JScrollPane tableView = new JScrollPane(jTable);
	splitPane.setRightComponent(tableView);
	menuTableFilter.setEnabled(true);
  }

  private void initSAMTable(String[][] samTable, String currYear, String currRegionName) {
	  for(int i =0; i < samTable.length; i++) {
		  samTable[i][0] = currYear;
		  samTable[i][1] = currRegionName;
		  switch(i) {
			  case 0: samTable[i][2] = "Activities"; break;
			  case 1: samTable[i][2] = "Commodities"; break;
			  case 2: samTable[i][2] = "Factors: Land"; break;
			  case 3: samTable[i][2] = "Factors: Labor"; break;
			  case 4: samTable[i][2] = "Factors: Capital"; break;
			  case 5: samTable[i][2] = "Household"; break;
			  case 6: samTable[i][2] = "Enterprises"; break;
			  case 7: samTable[i][2] = "Government"; break;
			  case 8: samTable[i][2] = "Capital Account"; break;
			  case 9: samTable[i][2] = "Rest Of World"; break;
			  case 10: samTable[i][2] = "Totals"; break;
			  default: samTable[i][2] = ""; break;
		  }
		  for(int j = 3; j < samTable[i].length; j++) {
			  samTable[i][j] = "0";
		  }
	  }
  }

  private String addString(String a, String b) {
	  try {
	  	return (new Double(Double.parseDouble(a) + Double.parseDouble(b))).toString();
	  } catch (Exception e) {
		  System.out.println("Got an exception a: "+a+" b: "+b);
		  return "0";
	  }
  }

  private Vector recBuildParentList(Node currNode, Vector currList, Vector path) {
	  if (currNode.getParentNode() == null) {
		  return currList;
	  }
	  currList = recBuildParentList(currNode.getParentNode(),currList, path);
	  if (path == null) {
		  if (currNode.hasAttributes()) {
			  currList.addElement(currNode.getNodeName() +" "+getOneAttrVal(currNode).substring(0,getOneAttrVal(currNode).indexOf("=")));
		  }
		  if (!getTextData(currNode).equals("")) {
		  	currList.addElement(currNode.getNodeName());
		  }
	  } else {
		  //path.addElement(new jtree.getModel().DOMNodeAdapter (currNode));
		  path.addElement(((DOMmodel)jtree.getModel()).getAdapterNode(currNode));
		  if (currNode.hasAttributes()) {
			  currList.addElement(getOneAttrVal(currNode).substring(getOneAttrVal(currNode).indexOf('=')+1));
		  }
		  if (!getTextData(currNode).equals("")) {
		  	  currList.addElement(getTextData(currNode));
		  }
		  else {
			  //currList.addElement("");
		  }
	  }
	  return currList;
  }

  private String getOneAttrVal(Node node) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  return nodeMap.item(0).getNodeName() +"="+ nodeMap.item(0).getNodeValue();
	  //return ((Element)node).getAttribute(nodeMap.item(0).getNodeValue());
  }

  private String getOneAttrVal(Node node, int pos) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  return nodeMap.item(pos).getNodeName() +"="+ nodeMap.item(pos).getNodeValue();
	  //return ((Element)node).getAttribute(nodeMap.item(0).getNodeValue());
  }

  private Vector getAttrsNoRegionYear(Node node) {
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector")) || ((String)wild.get(2)).matches(".*[Ss]ector")) && node.getNodeName().equals("subsector") ) {
		  return new Vector();
	  }

	  if(node.getNodeName().equals((String)wild.get(0)) || node.getNodeName().equals((String)wild.get(1)) 
			   || node.getNodeName().equals((String)wild.get(2)) ) {
		 return new Vector();
	  }
	  NamedNodeMap nodeMap = node.getAttributes();
	  Node tempNode;
	  Vector ret = new Vector();
	  for(int i = 0; i < nodeMap.getLength(); i++) {
		  tempNode = nodeMap.item(i);
	  	  //if(!tempNode.getNodeName().equals("year")) {
			  ret.add(tempNode);
		  //}
	  }
	  return ret;
  }

  private String getTextData(Node node) {
	  NodeList nl = node.getChildNodes();
	  for (int i = 0; i < nl.getLength(); i++) {
		  if (nl.item(i).getNodeType() == Node.TEXT_NODE) {
			  return nl.item(i).getNodeValue();
		  }
	  }
	  return "";
  }

class MyTreeModelListener implements TreeModelListener {
		public void treeNodesChanged(TreeModelEvent e) {
			System.out.println("treenodes have changed!");
			//jtree.setSelectionPath(e.getTreePath());
			//System.out.println(jtree.getSelectionPath());
			//System.out.println(jtree.getLeadSelectionPath());
			if(jTable != null) {
				Node node = ((DOMmodel.DOMNodeAdapter)e.getTreePath().getLastPathComponent()).getNode();
				//((DataTableModel)((TableSorter)jTable.getModel()).getTableModel()).setValueAt(node.getNodeValue(), e.getTreePath());
			}

			 /*
			try {
				int index = e.getChildIndices()[0];
				System.out.println("index is " + index);
				node = ((Node)(jtree.getModel().getChild(node, index)));
			} catch (Exception exc) {
				System.out.println("exception: "+exc);
			}*/

			System.out.println("The user has finished editing the node!!!!!!!!!!!!");
		}

		public void treeNodesInserted(TreeModelEvent e) {
		}
		public void treeNodesRemoved(TreeModelEvent e) {
		}
		public void treeStructureChanged(TreeModelEvent e) {
		}
    }

     
	private Node extractNewChild() {
		String nodeName = nameField.getText().trim();
		String attribs = attribField.getText().trim();
		String value = valueField.getText().trim();
		int sIndex, eIndex, index;
        boolean repeat = true;
		//create new node with given name
		Element newNode = null;
		
		while(repeat){
			try{
				newNode = doc.createElement(nodeName);
				repeat = false;
			}catch(Exception nameExc){
				JOptionPane.showMessageDialog(null, "Invalid node name!");
				Object[] possibilities = null;
				String s2 = (String)JOptionPane.showInputDialog(
									null,
									"Please try again, press cancel or leave blank to cancel 'Add Child'",
									"Re-enter a valid node name)",
									JOptionPane.PLAIN_MESSAGE,
									null,
									possibilities,
									null);	
				nodeName = s2;
				if(s2 == null || s2.length() == 0){
					return null;
				}
				System.out.println("BAD NAME .. "+nameExc);
			} 
		}
    
		repeat = true; //for detecting bad attributes
		while(repeat){
        
			//add all attributes to the new node, if the exist
			//  hopefully a comma-seporated list of attributes in the form: name=nodeName, year=1975, ...
			if (attribs.length() > 0) {
				StringTokenizer st = new StringTokenizer(attribs, ",", false);
				String attrib, val, strboth;
		
				try{
					int numTokens = st.countTokens();
					for(int i=0; i<numTokens; i++){
						strboth = st.nextToken().trim();
						StringTokenizer stInner = new StringTokenizer(strboth, "=", false);
						attrib = stInner.nextToken().trim();
						val = stInner.nextToken().trim();
						newNode.setAttribute(attrib, val);
					}	
					repeat = false;	

				}catch(Exception se){
					JOptionPane.showMessageDialog(null, "Syntax for attribute(s) incorrect!");
					Object[] possibilities = null;
					String s = (String)JOptionPane.showInputDialog(
										null,
										"Please try again, leave blank if you don't want any attributes, press cancel to cancel entire 'Add Child', othwerwise attributes must of form: attrname1=attrval1,attrname2=attrval2, ..",
										"Re-enter attribute(s)",
										JOptionPane.PLAIN_MESSAGE,
										null,
										possibilities,
										null);		
					if ((s != null) && (s.length() > 0)) {
						attribs = s;

					}
					if( s == null){
						return null;
					}
					if( s.length() == 0){ // no attribute
						repeat = false;
					}

					System.out.println("BAD ATTRIBUTE ADDED!!!!");
				}
			}else{
				repeat = false;
			}	
		
		} // while !okaytogoon
        
		if (value.length() > 0) {
			Text tempText = doc.createTextNode(value);
			newNode.appendChild(tempText);
		}
	
		return newNode;
	}

  boolean openXMLFile(){

	  JFileChooser fc = new JFileChooser();
	  fc.setDialogTitle("Open XML File");

	  // Choose only files, not directories
	  fc.setFileSelectionMode( JFileChooser.FILES_ONLY);

	  // Start in current directory
	  fc.setCurrentDirectory(globalFC.getCurrentDirectory());

	  // Set filter for Java source files.
	  fc.setFileFilter(xmlFilter);

	  // Now open chooser
	  int result = fc.showOpenDialog(this);

	  if( result == JFileChooser.CANCEL_OPTION){
		  return true;
	  }
	  else if( result == JFileChooser.APPROVE_OPTION){
		file = fc.getSelectedFile();
		globalFC.setCurrentDirectory(fc.getCurrentDirectory());
		readXMLFile( file);

		//textArea.setText("Opened");
	  }
	  else
	  {
		return false;
	  }
	  return true;
   }

   boolean openCSVFile(){

	 File file2;
	 JFileChooser fc = new JFileChooser();
	 fc.setDialogTitle("Open CSV File");

	 // Choose only files, not directories
	 fc.setFileSelectionMode( JFileChooser.FILES_ONLY);

	 // Start in current directory
	 fc.setCurrentDirectory(globalFC.getCurrentDirectory());

	 // Set filter for Java source files.
	 fc.setFileFilter(csvFilter);

	 // Now open chooser
	 int result = fc.showOpenDialog(this);

	 if( result == JFileChooser.CANCEL_OPTION){
		 return true;
	 }
	 else if( result == JFileChooser.APPROVE_OPTION){
		file = fc.getSelectedFile();
		globalFC.setCurrentDirectory(fc.getCurrentDirectory());

		// inserted for opening file 2
		JFileChooser fc2 = new JFileChooser();
		fc2.setDialogTitle("Open Headers File");
		fc2.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fc2.setCurrentDirectory(fc.getCurrentDirectory());
		int result2 = fc2.showOpenDialog(this);
		if(result2 == JFileChooser.CANCEL_OPTION){
			return true;
		}
		else if(result2 == JFileChooser.APPROVE_OPTION){
			file2 = fc2.getSelectedFile();
			globalFC.setCurrentDirectory(fc2.getCurrentDirectory());
			readCSVFile(file, file2);
			// DO STUFF WITH FILE1 AND FILE2
		}

	 }
	 else{
		return false;
		 }
	 return true;
	 }


  /**
   Use a JFileChooser in Save mode to select files
   to open. Use a filter for FileFilter subclass to select
   for *.java files. If a file is selected, then write the
   the string from the textarea into it.
   */
   boolean saveFile(){

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

	  if( result == JFileChooser.CANCEL_OPTION){
		  return true;
	  }
	  else if( result == JFileChooser.APPROVE_OPTION){
		file = fc.getSelectedFile();
		if (!file.getName().matches("[.]")) {
			if( !(file.getAbsolutePath().endsWith(".xml")) ){
				file = new File(file.getAbsolutePath()+".xml");
			}
		}
		if( file.exists()){
			int response = JOptionPane.showConfirmDialog(null,
				"Overwrite existing file?","Confirm Overwrite",
				 JOptionPane.OK_CANCEL_OPTION,
				 JOptionPane.QUESTION_MESSAGE);
			//if they hit cancell it gives and error message, so i
			//made it return true, that could be a problem in the future
			if( response == JOptionPane.CANCEL_OPTION) return true;
		}
		globalFC.setCurrentDirectory(fc.getCurrentDirectory());
		return writeFile(file, doc);
	  }
	  else{
		return false;
	  }
  }


  public void readXMLFile(File file){
	  try {
		lsInput.setByteStream(new FileInputStream(file));
		lsParser = implls.createLSParser(DOMImplementationLS.MODE_SYNCHRONOUS, null);
		lsParser.setFilter(new ParseFilter());
		doc = lsParser.parse(lsInput);
		//removeEmptyTextNodes(doc.getDocumentElement());
	  } catch (Exception e) {
		  System.out.println("Got Exception while creating XML document: "+e);
		  JOptionPane.showMessageDialog(this, "Exception while creating XML document\n"+e, "Exception", JOptionPane.ERROR_MESSAGE);
	  }
  }

  public void readCSVFile(File file, File file2){
	StringTokenizer st;
	String intValueStr;
	String strToReplace;
	int counter;
	int intValue;
	int dollarindex = 0;
	String inputLine;

	ArrayList dataArr;
	HashMap nickNameMap = new HashMap(); // shortname -> long string to append to end
	HashMap tableIDMap = new HashMap(); // tableID -> long string of headers
	DOMTreeBuilder tree = new DOMTreeBuilder();

	try{

	FileInputStream hashfis = new FileInputStream(file2);
	DataInputStream hashfin = new DataInputStream(hashfis);
	BufferedReader hashInput = new BufferedReader(new InputStreamReader(hashfin));
	hashInput.readLine(); // ignores first line of file
	inputLine = hashInput.readLine().trim();
	while(inputLine != null && inputLine.charAt(0) == '$'){ // read in header nick names
		st = new StringTokenizer(inputLine, ",", false);
		intValueStr = st.nextToken(); // $nickname
		inputLine = inputLine.substring(intValueStr.length()+1).trim();
		nickNameMap.put(intValueStr, inputLine);
		if( (inputLine = hashInput.readLine()) != null){
			inputLine.trim();
		}
	}
	while(inputLine != null){
		if(!inputLine.equals("")){
			st = new StringTokenizer(inputLine, ",", false);
			intValueStr = st.nextToken(); // numID
			inputLine = inputLine.substring(intValueStr.length()+1); // everything but numID
			try{
				intValue = Integer.parseInt(intValueStr);

				inputLine = inputLine.replaceAll( "[,][\\s]*[,]", "" ); // gets rid of end commas
				if(inputLine.endsWith(",")){ // gets ride of last comma if there is one
					inputLine = inputLine.substring(0, inputLine.length()-1);
				} // extra commas are now all gone

				dollarindex = 0;
				while( (dollarindex = inputLine.indexOf('$')) != -1 ){
					counter = dollarindex;
					while(counter < inputLine.length() && inputLine.charAt(counter) != ','){
						counter++;
					}
					strToReplace = inputLine.substring(dollarindex, counter);
					if(nickNameMap.containsKey(strToReplace)){
						//strToReplace = strToReplace.substring(1);
						//strToReplace = "^[.]*"+strToReplace+"[.]*$";
						inputLine = inputLine.replaceAll("\\"+strToReplace,
							((String)nickNameMap.get(strToReplace)));
					}
					else{
						System.out.println("***Couldn't find replacement for "+strToReplace+"!***");
						JOptionPane.showMessageDialog(this, "Couldn't find replacement for "+strToReplace, "Warning", JOptionPane.WARNING_MESSAGE);
					}
				}
				tableIDMap.put( new Integer(intValue) , inputLine );
			}catch(NumberFormatException e){
				System.out.println("*** Hashtable file formatted incorrectly ***"+e);
				JOptionPane.showMessageDialog(this, "Hashtable file formatted incorrectly\n"+e, "Exception", JOptionPane.ERROR_MESSAGE);
			}
		}
		if ( (inputLine = hashInput.readLine()) != null) {
			inputLine.trim();
		}
	}

	// tableIDMap should now be all set up ...

	FileInputStream fis = new FileInputStream(file);
	DataInputStream fin = new DataInputStream(fis);
	BufferedReader stdInput = new BufferedReader(new InputStreamReader(fin));

	inputLine = stdInput.readLine().trim();				// read one line of input

	while( inputLine != null ){
		while( inputLine != null && ! inputLine.startsWith("INPUT_TABLE") ){
			inputLine = stdInput.readLine();
		}
		if (inputLine == null) {
			break;
		}
		stdInput.readLine(); // reads/ignores "Variable ID" line
		inputLine = stdInput.readLine().trim(); // should have just the id number
		st = new StringTokenizer(inputLine, ",", false);
		intValue = Integer.parseInt(st.nextToken());

		if(tableIDMap.containsKey( new Integer(intValue))){
			tree.setHeader( ((String)tableIDMap.get( new Integer(intValue) )));
			stdInput.readLine(); // ignores this line
			stdInput.readLine(); // ignores header line

			inputLine = stdInput.readLine().trim();		// start reading in data
			while( inputLine != null && !inputLine.equals("") && inputLine.charAt(0) != ','){
				st = new StringTokenizer(inputLine, ",", false);
				int NUM_COLS = st.countTokens();
				dataArr = new ArrayList(NUM_COLS);
				for(int i=0; i < NUM_COLS; i++){
					dataArr.add(i, (st.nextToken()).trim() );
				}	// one line of data stores in arraylist
				tree.addToTree(dataArr);
				//makeTree( rootElement, docName );
				dataArr.clear();
				if ( (inputLine = stdInput.readLine()) != null) {
					inputLine.trim();
				}
			}
		}
		else{
			System.out.println("***Warning: skipping table: " + intValue+"!***");
		}

		if ( (inputLine = stdInput.readLine()) != null) {
				inputLine.trim();
		}
	}

	doc = tree.getDoc();
	//tree.outputTree(args[2]);

	fin.close();
	hashfin.close();

	} catch(Exception e){
		System.out.println("Excpetion thrown while trying to read csv and header files "+e);
		StackTraceElement[] s = e.getStackTrace();
		for (int i = 0; i < s.length; i++) {
			System.out.println(s[i]);
		}
		JOptionPane.showMessageDialog(this, "Excpetion thrown while trying to read csv and header files\n"+e, "Exception", JOptionPane.ERROR_MESSAGE);
	}
  }

  public boolean writeFile(File file, Document theDoc){
	// specify output formating properties
	OutputFormat format = new OutputFormat(theDoc);
	format.setEncoding("UTF-8");
	format.setLineSeparator("\n");
	format.setIndenting(true);
	format.setIndent(3);
	format.setLineWidth(0);
	format.setPreserveSpace(false);
	format.setOmitDocumentType(true);

	// create the searlizer and have it print the document

	try {
		FileWriter fw = new FileWriter(file);
		XMLSerializer serializer = new XMLSerializer (fw, format);
		serializer.asDOMSerializer();
		serializer.serialize(theDoc);
		fw.close();
	} catch (java.io.IOException e) {
		System.err.println("Error outputing tree: "+e);
		return false;
	}
	return true;
  }

}

