
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

	private JFrame thisFrame;
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

  static final int windowHeight = 460;
  static final int leftWidth = 300;
  static final int rightWidth = 340;
  static final int windowWidth = leftWidth + rightWidth;

  static String[] names = {"Single Table", "Multi Tables", "Both Tables"};
  static JFrame frame; // goes with radio buttons

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
	  thisFrame = this;
	
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
	menuTableFilter.setEnabled(false);
	menuTableAdd.setEnabled(false);

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
			   	jtree.setSelectionPath(selectedPath);
				MenuElement[] me = treeMenu.getSubElements();
			        for (int i = 0; i < me.length; i++) {
					if (((JMenuItem)me[i]).getText().equals("Display Table")) {
						if (jtree.getModel().isLeaf(jtree.getLastSelectedPathComponent())) {
							((JMenuItem)me[i]).setEnabled(false);
						} else {
							((JMenuItem)me[i]).setEnabled(true);
						}
					}
					if (((JMenuItem)me[i]).getText().equals("Add Child")) {
						Node nodeClicked = ((DOMmodel.DOMNodeAdapter)jtree.getLastSelectedPathComponent()).getNode();

						if ( nodeClicked.getNodeType() == Element.TEXT_NODE ) {
							((JMenuItem)me[i]).setEnabled(false);
						} else {
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
		try {
			((BaseTableModel)((JTable)((JScrollPane)splitPane.getRightComponent()).getViewport().getView()).getModel()).filterData(this);
			// NOT THE BEST WAY TO SET ROW HEIGHT
			int j = 1;
			JTable jTable = (JTable)((JScrollPane)splitPane.getRightComponent()).getViewport().getView();
			while( j < jTable.getRowCount()) {
				jTable.setRowHeight(j-1,16);
				jTable.setRowHeight(j,200);
				j += 2;
			}
		} catch (UnsupportedOperationException uoe) {
			JOptionPane.showMessageDialog( null, "This table does not support filtering", "Table Filter Error", JOptionPane.ERROR_MESSAGE);
		}
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
				RadioButton.showDialog(frame, null, "", "Choose Table Viewing Type", names,"");
				JScrollPane tableView = RadioButton.createSelection(selectedPath, doc, thisFrame);
	  			splitPane.setRightComponent(tableView);
	  		        menuTableFilter.setEnabled(true);
				tableMenu = makePopupTableMenu();
	   			((JTable)tableView.getViewport().getView()).addMouseListener(new MouseAdapter() {
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
				e.translatePoint( lastFlipX, lastFlipY );
				Point p = e.getPoint();
				JTable jTable = (JTable)((JScrollPane)splitPane.getRightComponent()).getViewport().getView();
				int row = jTable.rowAtPoint( p );
				int col = jTable.columnAtPoint( p );
				
				((BaseTableModel)jTable.getModel()).flip(row, col);
			}
			public void mouseClicked(MouseEvent e) {
				//shouldn't the action go here
			}
			public void mousePressed(MouseEvent e) {}
			public void mouseEntered(MouseEvent e) {}
			public void mouseExited(MouseEvent e) {}
		});
		tableMenu.add(menuItem);
		return tableMenu; 
  }
  // end new code for flip ..
  
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


  // ********* newly added **************
  // for the tablechangedmodel listener
  public void tableChanged(TableModelEvent e) {
	  if(e.getType() == TableModelEvent.UPDATE) {
		((DOMmodel)jtree.getModel()).fireTreeNodesChanged(new TreeModelEvent(e.getSource(), selectedPath));
	  }
  }

	class MyTreeModelListener implements TreeModelListener {
		public void treeNodesChanged(TreeModelEvent e) {
			try {
				if(e.getSource() instanceof DOMmodel) {
					BaseTableModel bt = (BaseTableModel)((JTable)((JScrollPane)splitPane.getRightComponent()).getViewport().getView()).getModel();
					bt.fireTableRowsUpdated(0,bt.getRowCount());
				}
			} catch(Exception ex) {}
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

