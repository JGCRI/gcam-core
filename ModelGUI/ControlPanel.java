/*
 * ControlPanel.java
 *
 * Created on May 19, 2003, 11:54 AM
 */

package ModelGUI;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.text.*;
import javax.swing.tree.*;
import java.io.*;
import java.lang.*;
import java.util.*;
import java.util.zip.*;
import java.awt.*;
import java.awt.event.*;
import javax.xml.parsers.*;
import org.w3c.dom.NodeList;
import org.jdom.*;
import org.jdom.input.*;
import org.jdom.output.*;

/** This class provides the Grapical User Interface that interacts
 *  with an XML file. The GUI has a tree view of the XML file and is able
 * to create tabular views of selected nodes.
 *
 * <br> ControlPanel makes several assumptions about the nature of the XML file.
 * To display correctly, the file must have a world node. Children of the world
 * must include modeltime and region. Relevant data is expected to be stored 
 * under the corresponding region node.
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 *
 */

public class ControlPanel extends javax.swing.JFrame {
    
    private JTextField jTFfileName;
    private JPanel browsePanel;
    private JPanel treePanel;
    private JPanel dataPanel;
    private JScrollPane dataView;
    private JPanel queryPanel;
    private JTextField valuePane;
    private JSplitPane splitPane;
    private JPanel tableHeaderPanel;
    
    private JDialog addChildDialog;
    private JDialog cloneNodeDialog;
    private JLabel infoLabel;
    private JTextField nameField;
    private JTextField attribField;
    private JTextField valueField;
    
    private JButton displayButton;
    private JButton selectNodesButton;
    
    private AdapterNode rootTreeNode;
    private MapNode rootMapNode;
    private ModelTime modelTime;
    
    private JTree tree;
    private org.jdom.Document document;
    private java.util.List ansestorNodes;
    private JPopupMenu treeMenu;
    private TreePath selectedPath;
    
    private JTable table;
    private int tableFlag;
    private JButton showTableButton;
    private JPopupMenu tableMenu;
    
    static boolean nodeValueChangedFlag;
    
    private boolean showNames;
    private JComboBox listTableBox;
    private JList regionBox;
    
    private Vector queryControls;
    private Vector attributeControls;
    private Vector mapPointers;
    private boolean tableExistsFlag;
    
    private static JFileChooser fc;
    static Toolkit toolkit;
    
    private static int LIST = 0;
    private static int TABLE = 1;
    private final static int DEFAULT_COMPONENT_HEIGHT = 20;
    protected final static String DEFAULT_SELECTION_STRING = "--chose one--";
    protected final static String DEFAULT_PLURAL_STRING = "-- all --";
    
    /** Creates a new instance of ControlPanel. Currently, displayes a field 
     * requesting the name of the file to be viewed and manipulated. Eventually, 
     * this field should be part of a standard File menu. Feature to implement:
     * list of recently opened files. */
    public ControlPanel() {
        initComponents();
        
        //initialize global flags
        tableExistsFlag = false;
        nodeValueChangedFlag = false;
        fc = new JFileChooser( "." );
        XMLFileFilter filter = new XMLFileFilter();
        fc.setFileFilter(filter);
        toolkit = this.getToolkit();
        
        this.getContentPane().setLayout(new BorderLayout());
        
        jTFfileName = new JTextField();
        jTFfileName.setPreferredSize(new Dimension(300, DEFAULT_COMPONENT_HEIGHT));
        jTFfileName.setMaximumSize(new Dimension(1200, DEFAULT_COMPONENT_HEIGHT));
        
        JButton browseButton = new JButton("Browse...");
        browseButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jBbrowseActionPerformed(evt);
            }
        });
        
        browsePanel = new JPanel();
        browsePanel.setLayout(new BoxLayout(browsePanel, BoxLayout.X_AXIS));
        browsePanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createTitledBorder(
        "Please select the XML file you wish to view"),BorderFactory.createEmptyBorder(5,5,5,5)));
        
        browsePanel.add(jTFfileName);
        browsePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        browsePanel.add(browseButton);
        
        this.getContentPane().add(browsePanel, BorderLayout.NORTH);
        
        displayButton = new JButton("Show XML Tree");
        displayButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                displayButtonActionPerformed(evt);
            }
        });
        
        treePanel = new JPanel();
        treePanel.setLayout(new BoxLayout(treePanel, BoxLayout.Y_AXIS));
        treePanel.setBorder(BorderFactory.createEmptyBorder(0,5,5,5));
        treePanel.add(displayButton);
        
        this.getContentPane().add(treePanel, BorderLayout.CENTER);
        
        //let the window listener handle closing operations -
        //  this allowes user to select Cancel from the Save-before-closing dialog
        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        
        pack();
    }
    
    /** This method is called from within the constructor to
     * initialize the form. The form is primarily blank and most
     * components are dynamically created in the constructor.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    private void initComponents() {//GEN-BEGIN:initComponents

        addComponentListener(new java.awt.event.ComponentAdapter() {
            public void componentResized(java.awt.event.ComponentEvent evt) {
                frameResized(evt);
            }
        });

        addWindowListener(new java.awt.event.WindowAdapter() {
            public void windowClosing(java.awt.event.WindowEvent evt) {
                exitForm(evt);
            }
        });

        pack();
    }//GEN-END:initComponents
    
    private void frameResized(java.awt.event.ComponentEvent evt) {//GEN-FIRST:event_frameResized

    }//GEN-LAST:event_frameResized
    
    
    private void selectNodesButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_selectNodesButtonActionPerformed

    }//GEN-LAST:event_selectNodesButtonActionPerformed
    
    /** This function is called when the user enters the name of a file
     *  to be displayed and clicks on the corresponding displayButton.
     *
     * @param evt the event generated by clicking on displayButton*/
    private void displayButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_displayButtonActionPerformed
          
            //attempt to read in the XML file into
            //  the private org.w3c.dom.Document document
        String fileName = jTFfileName.getText();
            boolean success = readXMLFile(fileName);
            if (success) {
                fc.setCurrentDirectory(new File(fileName));
                displayTree();
            }
    }//GEN-LAST:event_displayButtonActionPerformed
    
    
    /** This function is called when the user clicks on the jBbrowse
     * button, which brings up a file selection dialog box.
     * The name of the selected file is entered into the editable text box and 
     * the file will be opened if the user clicks on the displayButton.
     *
     * @param evt event generated by clicking on jBbrowse*/
    private void jBbrowseActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jBbrowseActionPerformed
        //Create a file chooser dialogue box
        //final JFileChooser fc = new JFileChooser();
        
        //get the complete name of the selected file
        int returnVal = fc.showOpenDialog(this.getContentPane());
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            jTFfileName.setText(file.getAbsolutePath());
        }
        
    }//GEN-LAST:event_jBbrowseActionPerformed
    
    /** Exits the Application. if the contents of the tree (or any of its nodes)
     * has been modified, ask user to save changes before exiting.
     * NOTE: this feature has not been extensively tested and may not work for all
     * possible methods of modifying the tree. */
    private void exitForm(java.awt.event.WindowEvent evt) {//GEN-FIRST:event_exitForm
        if (nodeValueChangedFlag == true) {
            int answer = JOptionPane.showConfirmDialog(this.getContentPane(), "Do you want to save changes to the XML file?");
            if (answer == JOptionPane.CANCEL_OPTION) return;
            else if (answer == JOptionPane.YES_OPTION) {
                saveFile();
            } 
        } 
        System.exit(0);
    }//GEN-LAST:event_exitForm
    
    /** The entry point of the program. Sets the look and feel of the GUI to
     * the current OS and requests a new instance of ControlPanel.
     *
     * @param args the command line arguments */
    public static void main(String args[]) {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        } catch (Exception e) { }
        
        new ControlPanel().show();
    }
    
    /** Takes the full name of an XML file and attempts to read
     * the XML into a JDOM parser.
     *
     * @param fileName name of file to be read and displayed
     * @return true if the read was successful */   
    public boolean readXMLFile(String fileName) {
        SAXBuilder parser = new SAXBuilder();
        try {
            document = parser.build(new File(fileName));
            AdapterNode tempRoot = new AdapterNode(document.getRootElement());
            rootMapNode = new MapNode(tempRoot);
            modelTime = new ModelTime(tempRoot);
            return true;
        } catch (Exception e){
            System.out.println("Exception " + e + " in function readXMLFile");
            return false;
        }
    }
    
    /** Creates a visual representation of the JDOM generated in readXMLFile.
     * The JTree appears in left panel of a split pane, the text panel 
     * on the right allows user to see and edit values of nodes. The right panel
     * also holds the holds the so-called "query" line used to generate a tabular
     * view of specified nodes. The tabular view, when active, appears under the 
     * query line. 
     *
     * Future Feature: change the apperance of the query line to reflect the
     * hierarchy of the XML file. Instead of displaying node names one adjecent 
     * to another, add new nodes bellow and indented from the position of the 
     * parent.
     */
    public void displayTree() {
        int leftWidth = 350;
        int rightWidth = 350;
        int windowHeight = 500;
        int windowWidth = leftWidth + rightWidth;
        
        tree = new JTree(new JDomToTreeModelAdapter(document));
        //allow multiple nodes to be selected simultaneously
        tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
        //Listen for when the selection changes.
        tree.addTreeSelectionListener(new TreeSelectionListener() {
            public void valueChanged(TreeSelectionEvent e) {
                TreePath[] paths = tree.getSelectionPaths();
                if (paths != null) {
                    clearDisplay();
                    for (int j = 0; j < paths.length; j++) {
                        AdapterNode node = (AdapterNode)paths[j].getLastPathComponent();
                        displayValue(node.getText(), false);
                    }
                }
            }
        });
        
        //listen for right click on the tree
        tree.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                maybeShowPopup(e);
            }
            public void mouseReleased(MouseEvent e) {
                maybeShowPopup(e);
            }
            private void maybeShowPopup(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    treeMenu.show(e.getComponent(), e.getX(), e.getY());
                    selectedPath = tree.getClosestPathForLocation(e.getX(), e.getY());
                }
            }
        });
        
        //set the icons of the tree to be blank
        DefaultTreeCellRenderer renderer = new DefaultTreeCellRenderer();
        Icon icon = null;
        renderer.setLeafIcon(icon);
        renderer.setClosedIcon(icon);
        renderer.setOpenIcon(icon);
        tree.setCellRenderer(renderer);
        
        // Build left-side view
        JScrollPane treeView = new JScrollPane(tree);
        treeView.setPreferredSize(new Dimension(leftWidth, windowHeight));
        treeView.setMinimumSize(new Dimension(50, 50));
        
        // Build right-side view
        // start with text field to show values of nodes in tree
        valuePane = new JTextField();
        valuePane.setEditable(true);
        valuePane.setPreferredSize(new Dimension(100, DEFAULT_COMPONENT_HEIGHT));
        valuePane.setMinimumSize(new Dimension(30, DEFAULT_COMPONENT_HEIGHT));
        valuePane.setMaximumSize(new Dimension(2000, DEFAULT_COMPONENT_HEIGHT));
        valuePane.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //when user hits enter after updating content of valuePane, the new value will be saved into the tree node
                AdapterNode currNode = (AdapterNode)tree.getLastSelectedPathComponent();
                String newVal = valuePane.getText();
                
                currNode.setText(newVal);
                nodeValueChangedFlag = true;
            }
        });
        
        JPanel tempValPanel = new JPanel();
        tempValPanel.setBorder(BorderFactory.createEmptyBorder(0,0,10,0));
        tempValPanel.setLayout(new BoxLayout(tempValPanel, BoxLayout.X_AXIS));
        tempValPanel.add(new JLabel("Node Value:"));
        tempValPanel.add(Box.createRigidArea(new Dimension(5,0)));
        tempValPanel.add(valuePane);
        
        //More right-side view
        // build panel that'll house query line for the table view
        makeQueryPanel(true);
        
        JPanel bigDataPane = new JPanel();
        bigDataPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        bigDataPane.setLayout(new BorderLayout());
        bigDataPane.add(tempValPanel, BorderLayout.NORTH);
        
        dataPanel = new JPanel();
        dataPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        dataPanel.setLayout(new BoxLayout(dataPanel, BoxLayout.Y_AXIS));
        dataPanel.add(queryPanel);
        dataPanel.addComponentListener(new ComponentListener() {
            public void componentHidden(ComponentEvent e) {}
            public void componentMoved(ComponentEvent e) {} 
            public void componentResized(ComponentEvent e) {
                JScrollBar bar = dataView.getHorizontalScrollBar();
                bar.setValue(bar.getMaximum());                
            }
            public void componentShown(ComponentEvent e) {}

        });
        
        dataView = new JScrollPane(dataPanel);
        dataView.setPreferredSize(new Dimension(rightWidth, windowHeight));
        dataView.setMinimumSize(new Dimension(50, 50));
        dataView.getHorizontalScrollBar().setUnitIncrement(10);
        dataView.getHorizontalScrollBar().setBlockIncrement(50);

        bigDataPane.add(dataView);
        
        //make button that will display the table
        showTableButton = new JButton("Show Query Result");
        //showTableButton.setAlignmentX(Box.LEFT_ALIGNMENT);
        //showTableButton.setAlignmentY(Box.TOP_ALIGNMENT);
        showTableButton.setMaximumSize(new Dimension(150, DEFAULT_COMPONENT_HEIGHT));
        showTableButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //try {
                readTableData();
                /*} catch (Exception ex) {
                    System.out.println("I've failed miserably");
                    System.out.println(ex);
                }*/
            }
        });
        showTableButton.setEnabled(false);
        dataPanel.add(Box.createRigidArea(new Dimension(0,5)));
        JPanel p = new JPanel(new BorderLayout());
        p.add(showTableButton, BorderLayout.EAST);
        p.setMaximumSize(new Dimension(2000, DEFAULT_COMPONENT_HEIGHT));
        dataPanel.add(p);
        dataPanel.add(Box.createVerticalGlue());
        dataPanel.add(Box.createRigidArea(new Dimension(0,10)));
        
        // Build split-pane view
        splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT, treeView, bigDataPane);
        splitPane.setContinuousLayout(true);
        splitPane.setDividerLocation(leftWidth);
        splitPane.setPreferredSize(new Dimension(windowWidth + 10, windowHeight+10));
        splitPane.setMinimumSize(new Dimension(windowWidth + 10, windowHeight+10));
        splitPane.setMaximumSize(new Dimension(2000, 1500));
        
        // Build the pop-up menus that are displayed by right-clicking
        treeMenu = makePopupTreeMenu();     
        tableMenu = makePopupTableMenu();
        
        JButton saveAllButton = new JButton("Save File");
        saveAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                saveFile();
            }
        });
        
        treePanel.add(Box.createRigidArea(new Dimension(0,5)));
        treePanel.add(splitPane);
        treePanel.add(Box.createRigidArea(new Dimension(0,5)));
        treePanel.add(saveAllButton);
        
        //create the dialog for adding new node children, but leave invisible
        makeAddChildDialog();
        makeCloneNodeDialog();
        
        //pack() appeares necessary because the outer dimention of ControlPanel
        //  change. repaint() is safer, but not powerful enough in this case.
        pack();
    }
    
    /** Creates the popup menu that will appear when the user right-clicks on 
     * the tree view. Currently, the menu items are:
     *      Add Child - adds a new node as the last child of selected node. 
     *      Show Table and Path - generates the query line associated with
     *          selected node and, if possible, displays the value(s) in a table.
     *      Delete Node - deletes selected node and its children
     * Future Feature: a Clone Node option. The code for the popup is in place, 
     *      but the functionality is not fully implemented */
    private JPopupMenu makePopupTreeMenu() {
        treeMenu = new JPopupMenu();
        JMenuItem menuItem = new JMenuItem("Add Child");
        menuItem.addMouseListener(new MouseListener() {
            public void mouseReleased(MouseEvent e) {
                tree.setSelectionPath(selectedPath);
                
                showAddChildDialog();
            }
            public void mouseClicked(MouseEvent e) {}
            public void mousePressed(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        treeMenu.add(menuItem);
        
        /*menuItem = new JMenuItem("Clone Node");
        menuItem.addMouseListener(new MouseListener() {
            public void mouseReleased(MouseEvent e) {
                tree.setSelectionPath(selectedPath);
                
                cloneNode();
            }
            public void mouseClicked(MouseEvent e) {}
            public void mousePressed(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        treeMenu.add(menuItem);
        
        /*menuItem = new JMenuItem("Print Path");
        menuItem.addMouseListener(new MouseListener() {
            public void mouseReleased(MouseEvent e) {
                tree.setSelectionPath(selectedPath);
         
                System.out.println("Node's path is " + selectedPath);
            }
            public void mouseClicked(MouseEvent e) {}
            public void mousePressed(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        treeMenu.add(menuItem);*/
        
        treeMenu.add(new JSeparator());
        menuItem = new JMenuItem("Show Table and Path");
        menuItem.addMouseListener(new MouseListener() {
            public void mouseReleased(MouseEvent e) {
                tree.setSelectionPath(selectedPath);
                
                makeNewThread();
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
                tree.setSelectionPath(selectedPath);
                
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
    
    /** Creates the popup menu displayed when a user right-clicks on the table
     * view. The perpose is primarily to add more intuitive copy-pasting 
     * capabilities. Currently, copying and pasting is only possible with 
     * Ctrl-C and Ctrl-V. 
     * NOTE: This function is a work in progress and does nothing.*/
    private JPopupMenu makePopupTableMenu() {
        tableMenu = new JPopupMenu();
        //JMenuItem menuItem = new JMenuItem("Copy");
        JMenuItem menuItem = new JMenuItem(new DefaultEditorKit.CopyAction());
        menuItem.setText("Copy");

        menuItem.addMouseListener(new MouseListener() {
            public void mouseReleased(MouseEvent e) {  
//System.out.println("copy requested");

            /*InputMap imap = this.getInputMap();
    imap.put(KeyStroke.getKeyStroke("ctrl X"),
        TransferHandler.getCutAction().getValue(Action.NAME));
    imap.put(KeyStroke.getKeyStroke("ctrl C"),
        TransferHandler.getCopyAction().getValue(Action.NAME));
    imap.put(KeyStroke.getKeyStroke("ctrl V"),
        TransferHandler.getPasteAction().getValue(Action.NAME));*/

                //copyTableCells();
            }
            public void mouseClicked(MouseEvent e) {}
            public void mousePressed(MouseEvent e) {}
            public void mouseEntered(MouseEvent e) {}
            public void mouseExited(MouseEvent e) {}
        });
        tableMenu.add(menuItem);
                
        return tableMenu;
    }
    
    /** Creates the JPanel that will hold the query line. The appearantly 
     * accessive use of new JPanel objects is necessary to support desired layout.
     *
     * @param firstAttempt boolean indicating whether the query panel is being 
     *      created for the first time while this instance of ControlPanel has
     *      been running */
    private void makeQueryPanel(boolean firstAttempt) {
        if (firstAttempt) {
            
            queryPanel = new JPanel();
            queryPanel.setLayout(new BoxLayout(queryPanel, BoxLayout.X_AXIS));
            
            queryPanel.add(new JLabel("View a table of  "));
            
            String[] array2 = {DEFAULT_SELECTION_STRING, "Names", "Values"};
            JComboBox box = new JComboBox(array2);
            box.setMaximumSize(new Dimension(100, DEFAULT_COMPONENT_HEIGHT));
            box.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    JComboBox temp = (JComboBox)e.getSource();
                    if (temp.getSelectedIndex() == 1) showNames = true;
                    else showNames = false;
                    if (regionBox == null) displayRegionBox();
                }
            });
            queryPanel.add(box);
            
        }
        queryPanel.add(Box.createHorizontalGlue());
    }
   
    /** Creates the JList widget that will hold the names of all regions 
     * represented in the XML file. This widget is part of queryPanel. */
    private void displayRegionBox() {
        //remove the horizontal glue to display components adjecently
        queryPanel.remove(queryPanel.getComponents().length-1);
        
        //find the first mention of "World", Region nodes will be its children
        rootTreeNode = (AdapterNode)tree.getModel().getRoot();
        if (!rootTreeNode.hasChild("region")){
            findRoot(rootTreeNode, "region");
        }
       
        mapPointers = new Vector();
        mapPointers.addElement(rootMapNode.getDescendant("world"));
                
        //get the names of all regions
        MapNode regionNode = rootMapNode.getDescendant("region");
        Vector regionNames = regionNode.getPossibleNames(false);
        mapPointers.addElement(regionNode);
        
        queryPanel.add(new JLabel("  for region(s)  "));
        regionBox = new JList(regionNames.toArray());
        regionBox.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED),
        BorderFactory.createEmptyBorder(0,3,0,3)));
        
        //add listener - when user chooses region(s), display the next nodes in the hierarchy
        regionBox.addListSelectionListener(new ListSelectionListener() {
            public void valueChanged(ListSelectionEvent e) {
                if (e.getValueIsAdjusting() == false) {
                    showTableButton.setEnabled(true);
                    
                    JList temp = (JList)e.getSource();
                    if (mapPointers.size() < 3) addQueryControl(temp, 1);
                }
            }
        });
        
        if (attributeControls == null) attributeControls = new Vector();
        if (queryControls == null) queryControls = new Vector();
        queryControls.addElement(regionBox);
        
        queryPanel.add(regionBox);
        
        //put back the horizontal glue to make sure componenets of queryPanel are left justified
        queryPanel.add(Box.createHorizontalGlue());
        
        queryPanel.revalidate();
        repaint();
    }
    
    /** Removes GUI components that sequentially follow <code> curr </code> 
     * components in <code> parent </code> container.
     *
     * @param parent Container in the GUI from which elements are to be deleted
     * @param curr Component that will be the most recently added component 
     *      inside <code> parent </code> once removeComponent returns
     * @param index index into the queryControls array where the pointer to 
     *      each control inside <code> parent </code> is stored */    
    public void removeComponents(Container parent, Component curr, int index) {
        Component[] comps = parent.getComponents();
        boolean remove = false;
        
        //remove components from the panel
        for (int j = 0; j < comps.length; j++) {
            if (remove) {
                parent.remove(comps[j]);
            }
            if (!remove && curr.equals(comps[j])) {
                remove = true;
            }
        }
        
        repaint();
        
        //remove components from the control arrays (add one because indexes start at 0)
        //if (rootPointers != null && rootPointers.size() > index+2) rootPointers.setSize(index+2);
        if (mapPointers != null && mapPointers.size() > index+2) mapPointers.setSize(index+2);
        if (queryControls != null && queryControls.size() > index+1) queryControls.setSize(index+1);
        if (attributeControls != null && attributeControls.size() > index+1) attributeControls.setSize(index+1);
    }
    
    /** Creates a new JComboBox and adds it at end of the query line. The combo 
     * box holds the names of all children of its parent combo box. (The first 
     * one holds the names of all nodes that are possible children of a region
     * node.) When the user selects a specific name from the combo box, this 
     * generates an event that creates the next combo box. When the query line 
     * is complete, these combo boxes specify the paths to nodes in the tree.
     *
     * @param predecessor a reference to the combo box holding the parent of 
     *      the nodes whose names will be displayed
     * @param index index into the queryControls array specifying in which 
     *      position in the array the new combo box is to be stored */
    private void addQueryControl(Component predecessor, int index) {
        //get the parent of the nodes whose names will populate the new combo box
        MapNode mp = (MapNode)mapPointers.elementAt(index);
        boolean hasAttribute = false;
        if (mp.hasPossibleNames()) hasAttribute = true; 
        
        //check if combo box already exists, remove all traces of it
        if ((queryControls.size() > index) || (!hasAttribute && attributeControls.size() > index-1)) {
            removeComponents(queryPanel, predecessor, index-1); 
        } else {    //just elliminate that Horizontal glue that's the last component
            queryPanel.remove(queryPanel.getComponents().length-1);
        }
        
        //if node is leaf, no additional boxes need to be added
        if (!mp.hasChildren()) {
            queryPanel.add(Box.createHorizontalGlue());
            queryPanel.revalidate();
            repaint();
        
            return;
        }
        
        //if node does have children
        Vector names = new Vector();
        MapNode kid;
        names.addElement(DEFAULT_SELECTION_STRING);
        Vector children = mp.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            kid = (MapNode)it.next();
            names.addElement(kid.getNodeName());
        }
        
        JComboBox comboBox;
        
        //create new entirely component
        comboBox = new JComboBox(names);
        comboBox.setMaximumSize(new Dimension(1000, DEFAULT_COMPONENT_HEIGHT));
        comboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JComboBox temp = (JComboBox)e.getSource();
                String name = temp.getSelectedItem().toString();
                int index = queryControls.indexOf(temp);
                
                MapNode parent = (MapNode)mapPointers.elementAt(index);
                MapNode mp = parent.getChild(name);
                if (mapPointers.size() > index+1) {
                    mapPointers.setElementAt(mp, index+1);
                } else {
                    mapPointers.addElement(mp);
                }

                //check if node has an attribute name
                if (mp.hasPossibleNames()) {
                    addAttributeControl((Component)e.getSource(), name, index);
                } else {    //node does not have "name", only children                                         
                    addQueryControl((Component)e.getSource(), index+1);
                }
            }
        });
        
        comboBox.setName(names.toString());
        
        queryControls.addElement(comboBox);
        
        queryPanel.add(new JLabel("  of  "));
        queryPanel.add(comboBox);
        queryPanel.add(Box.createHorizontalGlue());
        
        queryPanel.revalidate();
        repaint();
    }
    
    /** Creates a JComboBox with a list of all all possible "name" attributes
     * of the node in the preceding combo box. This function is called by 
     * addQueryControl only if the node in the last query control has a name. 
     * The combo box must have at least two options ("--chose one--", name)
     * because the user's selection generates an event that creates the next
     * combo box.
     *
     * @param predecessor the combo box that contains the node name of the
     *      element whose "name" attribute is to be displayed
     * @param nodeName node name of target element
     * @param index index into the attributeControls array at which the new
     *      combo box is to be stored */
    private void addAttributeControl(Component predecessor, String nodeName, int index){
        //get the parent of the nodes whose names will populate the new combo box        
        JComboBox tempBox = (JComboBox)queryControls.elementAt(index);
        MapNode mp = (MapNode)mapPointers.elementAt(index+1);
        
        Vector names = new Vector();
        if (index > 1 && attributeControls.size() > index-1) {
            tempBox = (JComboBox)attributeControls.elementAt(index-1);
            if (tempBox != null) {
                //String parentName = tempBox.getSelectedItem().toString();
                names = mp.getPossibleNames(tempBox.getSelectedItem().toString());
            }
        } else {
            names = mp.getPossibleNames(false);
        }
        
        if (names.size() != 1) names.insertElementAt(DEFAULT_PLURAL_STRING, 0);
        names.insertElementAt(DEFAULT_SELECTION_STRING, 0);
        
        JComboBox comboBox;
        
        //check if combo box already exists and remove all of its successors if necessary
        if (attributeControls.size() > index || attributeControls.size() >= index) {
            removeComponents(queryPanel, predecessor, index);
        } else {
            //remove the horizontal glue to display components adjecently
            queryPanel.remove(queryPanel.getComponents().length-1);
        }
        
        //create new component
        comboBox = new JComboBox(names);
        comboBox.setMaximumSize(new Dimension(1000, DEFAULT_COMPONENT_HEIGHT));
        comboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                //get the specific selection of a nodes's name attribute
                JComboBox temp = (JComboBox)e.getSource();
                String attribVal = temp.getSelectedItem().toString();
                int index = attributeControls.indexOf(temp);
                
                //get the name of that node
                temp = (JComboBox)queryControls.elementAt(index);
                String nodeName = temp.getModel().getSelectedItem().toString();
                
                //find node with nodeName and "name" attribute = attribVal
                //currRootPointer = (AdapterNode)rootPointers.elementAt(index);
                MapNode mp = (MapNode)mapPointers.elementAt(index);
                MapNode newPointer = mp.getChild(nodeName);

                if (!newPointer.isLeaf()) {
                    //create new component with the nodes's set of chilren
                    addQueryControl((Component)e.getSource(), index+1);
                }
                
            }
        });
        
        if (attributeControls.size() < index+1) {
            attributeControls.setSize(index+1);
        }
        attributeControls.setElementAt(comboBox, index);
        
        queryPanel.add(Box.createRigidArea(new Dimension(5,0)));
        queryPanel.add(comboBox);
        queryPanel.add(Box.createHorizontalGlue());
        
        queryPanel.revalidate();
        repaint();
        //notifyAll();
    }
    
    /* Recursively searches for the first node that has at least one child with 
     * the specified name.Currently used to find the parent of the region nodes.
     * The function is void but sets the rootTreeNode field.
     *
     * @param currRoot AdapterNode where the search for new root is to begin
     * @param targetName node name of element whose parent the function needs 
     *      to find
     */
    private void findRoot(AdapterNode currRoot, String targetName) {
        if (currRoot.hasChild(targetName)) {
            rootTreeNode = currRoot;
            return;
        }
        
        java.util.List children = currRoot.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            findRoot((AdapterNode)it.next(), targetName);
        }
    }
    
    /** Prints out the value of a tree node in the adjecent pane. Currently used
     * to differentiate the node attributes from its text - the attributes
     * are displayed in the JTree as part of the node title while the value is
     * visible only when the element is selected.
     *
     * @param val the node's value that will be displyed
     * @param replace whether the value already in the text field should be 
     *      overwritten or appended to */    
    public void displayValue(String val, boolean replace) {
        if (replace == true)
            valuePane.setText(val);
        else {
            String temp = valuePane.getText().trim();
            if (temp.length() != 0) valuePane.setText(temp + '\n' + val);
            else valuePane.setText(val);
        }
    }
    
    /** Clears the text field used to display a node's text/value */
    public void clearDisplay() {
        valuePane.setText("");
    }
    
    /** Creates the buttons that serve as row labels along the left side of the 
     * table. The first button int he array (index = 0) shows the name of
     * node whose values are being displayed in the table.
     * Future Features: Clicking on any row header should select the entire row
     * (presumably for copy-pasting purposes). Clicking on the first button 
     * should select the entire table.
     * 
     * @param headings Vector of names that are to appear on the buttons
     *      The number of buttons created is equal to headings.size()
     * @param varName name of the variable currently being displays in the 
     *      table's cells - displayed in the corner cell to the left of the 
     *      column titles and above the row titles
     */
    private JPanel makeLefter(Vector headings, String varName) {
        //create vertical table labels
        JPanel lefterPanel = new JPanel();
        
        Rectangle buttonSize = table.getCellRect(0, 0, true);
        buttonSize = new Rectangle((int)buttonSize.getWidth()+500, (int)buttonSize.getHeight());
        Rectangle blankSize = new Rectangle((int)buttonSize.getWidth(), (int)buttonSize.getHeight()+4);
        lefterPanel.setLayout(new BoxLayout(lefterPanel, BoxLayout.Y_AXIS));
        
        JButton[] lefters = new JButton[headings.size()];
        JButton b = new JButton(varName);
        b.setPreferredSize(blankSize.getSize());
        b.setMinimumSize(blankSize.getSize());
        b.setMaximumSize(blankSize.getSize());
        /*b.addActionListener(new ActionListener() {
            public void actionPerformed (ActionEvent e) {
                //select the entire table
                table.clearSelection();
                //table.changeSelection(0, 0, true, false);
                //table.changeSelection(table.getRowCount()-1, table.getColumnCount()-1, false, true);
                table.getSelectionModel().setSelectionInterval(0, table.getColumnCount()-1);
                //table.grabFocus();
                repaint();
            }
        });*/
        lefterPanel.add(b);
        
        for (int j = 0; j < headings.size(); j++) {
            //create an array of non-editable buttons
            lefters[j] = new JButton(headings.elementAt(j).toString());
            lefters[j].setPreferredSize(buttonSize.getSize());
            lefters[j].setMaximumSize(buttonSize.getSize());
            lefters[j].setName(String.valueOf(j));
            lefters[j].setHorizontalAlignment(SwingConstants.RIGHT);
            /*lefters[j].addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    JButton button = (JButton)e.getSource();
                    int rowIndex = Integer.parseInt(button.getName());
                    int modifiers = e.getModifiers();
                    int numCols = table.getColumnCount();
             
                    //if Control button held, keep previous selection and add new row
                    if ((modifiers & ActionEvent.SHIFT_MASK) == ActionEvent.SHIFT_MASK) {
                        table.changeSelection(rowIndex, numCols-1, false, true);
                    } else {
                        table.changeSelection(rowIndex, 0, false, false);
                        table.changeSelection(rowIndex, numCols-1, false, true);
                    }
                    repaint();
                }
            }); */
            lefterPanel.add(lefters[j]);
        }
        
        return lefterPanel;
    }
    
    /** Interprets the query line and determines the node values that will be
     * displayed in the JTable. Depending on the user's specifications, 
     * traverses branches of the tree to pull out appropriate values and stores 
     * these in a one-dimentional Vector. Stores the ansestral names of these
     * nodes in a seporate Vector. */
    public void readTableData() {
        Vector values = new Vector();
        Vector header = new Vector();
        Vector lefter = new Vector();
        Vector queue = new Vector();
        int index;
        String nodeName;
        String attribVal = "";
        JComboBox currComboBox;
        java.util.List childNodes;
        boolean timeInterval = false;
        Integer lastYear = new Integer(modelTime.getStart());
        
        if (tableExistsFlag) {
            int compCount = dataPanel.getComponentCount();
            //remove the currely showing table, it's the last component
            dataPanel.remove(compCount-1);
        }
        
        //get the name of the last element among the nodes to compaire and know when to stop
        JComboBox lastBox = (JComboBox)queryControls.lastElement();
        String finalNodeName = lastBox.getSelectedItem().toString();
        
        //if (listTableBox.getSelectedIndex() == LIST) {
        String regionName;
        Object[] regions = regionBox.getSelectedValues();
        
        //perform a bredth-first traversal of the tree looking for nodes that fit a path
        //  described by the array of query and attribute controls
        //for each region, place the appropriate nodes in the queue
        //  remove nodes from the begining of the queue and add all appropriate children to the end of the queue
        for (int j = 0; j < regions.length; j++) {
            regionName = regions[j].toString();
            AdapterNode currNode = rootTreeNode.getChild("region", regionName);
            currNode.setIndex(1);
            queue.addElement(currNode);
            
            while (!queue.isEmpty()) {
                currNode = (AdapterNode)queue.remove(0);
                index = currNode.getIndex();
                
                //get the name of the current node
                currComboBox = (JComboBox)queryControls.elementAt(index);
                nodeName = currComboBox.getSelectedItem().toString();
                
                //get the desired attribute name of the current node, if any
                attribVal = "";
                if (!attributeControls.isEmpty() && attributeControls.size() > index
                && attributeControls.elementAt(index) != null) {
                    //get the "name" attribute if appropriate
                    currComboBox = (JComboBox)attributeControls.elementAt(index);
                    attribVal = currComboBox.getSelectedItem().toString();
                    if (attribVal.equals(DEFAULT_SELECTION_STRING)) attribVal = "";
                }
                
                //after obtaining the node name and name attribute of the current node,
                //  decide it is an intermediate node or an end target whoes values are to be displayed
                if(nodeName.equals(finalNodeName)) {
                    //remember the name of the parent of the node whose value
                    //  will be displayed in the left-side header

                    String lefterName = currNode.toLefterString();
                    String prevName;
                    if (!lefter.isEmpty()) {
                        prevName = (String)lefter.lastElement();
                        if (!(prevName.equals(lefterName))) lefter.add(lefterName);
                    } else lefter.add(lefterName);
                    
                    //extrac just the name of the node, without attributes or <>
                    String oldName = currNode.toString();
                    int index1 = oldName.indexOf('<');
                    int index2 = oldName.indexOf('>');
                    String parentName = oldName.substring(index1+1, index2);
                    
                    //for nodes directly under a period node
                    if (parentName.equals("period")) {
                        //remember the last index in values that contains confirmed entry
                        int lastValIndex = values.size();
                        
                        //fill values with appropriate number of empty nodes
                        int numPeriods = modelTime.getNumOfSteps();
                        for (int k = 0; k < numPeriods; k++) {
                            values.addElement(new AdapterNode());
                            
                            attribVal = modelTime.getYear(k);
                            if (header.isEmpty()) header.addElement(attribVal);
                            else if (!header.contains(attribVal)) header.addElement(attribVal);
                        }
                        
                        //get a list of all period nodes in that subtree
                        int offset = 0;
                        AdapterNode parent = currNode.getParent();
                        childNodes = parent.getChildren("period", "");
                        Iterator it = childNodes.iterator();
                        while (it.hasNext()) {
                            //for each period node, get the value of the "final node" under it,
                            //  remember the period node's year attribute
                            parent = (AdapterNode)it.next();
                            currNode = parent.getChild(finalNodeName, "");
                            if (currNode == null) continue;
                            
                            attribVal = parent.getAttributeValue("year");
                            offset = modelTime.getYearIndex(attribVal);
                            if (showNames) {
                                values.setElementAt(currNode.getAttributeValue("name"), lastValIndex+offset);
                            } else {
                                values.setElementAt(currNode, lastValIndex+offset);
                            }
                        }
                        
                    } else if (parentName.equals("grade")) {    
                        //code that allowes grade to be a column header
                        
                        index = currNode.getIndex() - 1;
                        //find which grade is to be displayed
                        currComboBox = (JComboBox)attributeControls.elementAt(index);
                        attribVal = currComboBox.getSelectedItem().toString();
                        
                        if (!attribVal.equals(DEFAULT_PLURAL_STRING)) {
                            //save grade name to the header
                            if (header.isEmpty()) header.addElement(attribVal);
                            else if (!header.contains(attribVal)) header.addElement(attribVal);
                            
                            //add the new value
                            currNode = currNode.getChild(finalNodeName, "");
                            values.addElement(currNode);
                            
                        } else {
                            AdapterNode child, parent = currNode.getParent();
                            childNodes = parent.getChildren("grade", "");
                            Iterator it = childNodes.iterator();
                            while (it.hasNext()) {
                                currNode = (AdapterNode)it.next();
                                attribVal = currNode.getAttributeValue("name");
                                if (header.isEmpty()) header.addElement(attribVal);
                                else if (!header.contains(attribVal)) header.addElement(attribVal);
                                
                                currNode = currNode.getChild(finalNodeName, "");
                                if (currNode != null) values.addElement(currNode);
                                else values.addElement(new AdapterNode());
                            }                           
                        }
                        
                    } else {        //if parent of desired node(s) isn't "period" or "grade"
                       
                        Vector years = modelTime.getTimeIntervals();
                        Iterator yearIt = years.iterator();
                        boolean demographics = false;
                        
                        if (attribVal.equals(DEFAULT_PLURAL_STRING)) attribVal = "";
                        childNodes = currNode.getChildren(nodeName, attribVal);
                        Iterator it = childNodes.iterator();
                        
                        //SPECIAL CASE: nodes under demographics begin with year=1960, not 1975
                        //assume that all values are present
                        if (currNode.getName().equals("demographics")) {
                            demographics = true;
                        }
                        
                        //adds blank values for nodes are not present in a subtree
                        //  but whose parent's name is already added to lefter
                        if (!it.hasNext()) {
                            if (timeInterval) {
                                int numPeriods = modelTime.getNumOfSteps();
                                for (int k = 0; k < numPeriods; k++) {
                                    values.addElement(new AdapterNode());
                                }
                            } else {
                                values.addElement(new AdapterNode());
                            }
                        }
                                                
                        while (it.hasNext()) {
                            currNode = (AdapterNode)it.next();
                            if (showNames) {
                                values.addElement(currNode.getAttributeValue("name"));
                            } else {
                                attribVal = currNode.getAttributeValue("year");
                                if (attribVal != null) {
                                    if (demographics) {
                                        if (header.isEmpty()) header.addElement(attribVal);
                                        else if (!header.contains(attribVal)) header.addElement(attribVal);
                                    } else {
                                        timeInterval = true;
                                        //if node has year, make sure that values are saved under proper year column
                                        Integer attribYear = new Integer(attribVal);
                                        Integer currYear = (Integer)yearIt.next();
                                        
                                        //special case - rows above current only had one column,
                                        //  but this row is time interval, so will add more columns
                                        //  therefore, add blank values to put new values in place
                                        int numVals = values.size();
                                        int targetVals = (lefter.size()-1) * modelTime.getNumOfSteps();  
                                        for (int q = numVals; q < targetVals; q++) {
                                            values.addElement(new AdapterNode());
                                        }
                                        while (!attribYear.equals(currYear)) {
                                            if (header.isEmpty()) header.addElement(currYear.toString());
                                            else if (!header.contains(currYear.toString())) header.addElement(currYear.toString());
                                            values.addElement(new AdapterNode());
                                            if (yearIt.hasNext())currYear = (Integer)yearIt.next();
                                            else break;
                                        }
                                        
                                        if (header.isEmpty()) header.addElement(currYear.toString());
                                        else if (!header.contains(currYear.toString())) header.addElement(currYear.toString());
                                        lastYear = currYear;
                                    }
                                }                                 
                                values.addElement(currNode);
                            }
                        }
                        while (timeInterval && !lastYear.equals(new Integer(modelTime.getEnd()))) {
                            lastYear = (Integer)yearIt.next();
                            if (header.isEmpty()) header.addElement(lastYear.toString());
                            else if (!header.contains(lastYear.toString())) header.addElement(lastYear.toString());
                            values.addElement(new AdapterNode());
                        }
                    }
                } else {    //current node is an intermediate step along the tree
                    if(attribVal.equals(DEFAULT_PLURAL_STRING)) {
                        //get all chidren and add to the queue for examination
                        childNodes = currNode.getChildren(nodeName, "");
                        Iterator it = childNodes.iterator();
                        while (it.hasNext()) {
                            currNode = (AdapterNode)it.next();
                            currNode.setIndex(index+1);
                            queue.addElement(currNode);
                            
                            //don't enqueue all grade nodes, the first one is enough
                            //  the rest will be handled by a while look alsewhere in this function
                            if (currNode.getName().equals("grade")) break;
                        }
                    } else {
                        //get child with target name attribute and add it to the queue for examination
                        currNode = currNode.getChild(nodeName, attribVal);
                        if (currNode != null) {
                           currNode.setIndex(index+1);
                           queue.addElement(currNode);
                        }
                    }
                }
            } //belongs to while (!queue.isEmpty())
        } //belongs to for each region
        
        showTable(values, header, lefter, finalNodeName);
    }
    
    /** Displays the table view on screen. Uses a TableViewModel to interpret 
     * the parameters into a JTable.
     *
     * @param values Vector of String values that will show up in the table cells
     * @param header Vector of Srings that contains column titles
     * @param lefter Vector of Strings that optionally contains row titles
     * @param varName name of the variable whose values are in the table cells
     *      shows up in the top left corner of the table */
    private void showTable(Vector values, Vector header, Vector lefter, String varName) {
        
//System.out.println("values  = " + values.size() + " header = " + header.size() + " lefter = " + lefter.size());
        
        TableViewModel model = new TableViewModel(values, header, lefter, showNames);
        table = new JTable(model);
        table.setCellSelectionEnabled(true);
        table.setTransferHandler(new TableTransferHandler());
	table.setAutoResizeMode( JTable.AUTO_RESIZE_ALL_COLUMNS );
        
        //use panels to place the table appropriately
        JPanel tempPanel = new JPanel();
        tempPanel.setLayout(new BorderLayout());
        tempPanel.add(table.getTableHeader(), BorderLayout.NORTH);
        tempPanel.add(table, BorderLayout.CENTER);
        
        if (!lefter.isEmpty()) {
            lefter = model.getTableLefter();
            JPanel lefterPanel = makeLefter(lefter, varName);
            lefterPanel.setPreferredSize(new Dimension(150, 10));
            lefterPanel.setMinimumSize(new Dimension(10, 10));
            
            JSplitPane splitTableView = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, lefterPanel, tempPanel);
            JPanel tempPanel2 = new JPanel(new BorderLayout());
            tempPanel2.add(splitTableView, BorderLayout.CENTER);
            dataPanel.add(tempPanel2);
        } else {
            dataPanel.add(tempPanel);
        }
        
        table.addMouseListener(new MouseAdapter() {
            public void mousePressed(MouseEvent e) {
                maybeShowPopup(e);
            }
            public void mouseReleased(MouseEvent e) {
                maybeShowPopup(e);
            }
            private void maybeShowPopup(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    tableMenu.show(e.getComponent(), e.getX(), e.getY());
                }
            }
        });
       
        tableExistsFlag = true;
        
        dataPanel.revalidate();
        repaint();
    }
    
    /** Creates the dialog box that will be made visible when the user choses to
     * add a child to an esiting node in the tree. */
    public void makeAddChildDialog() {
        addChildDialog = new JDialog(this, "Add Child Node", true);
        Container content = addChildDialog.getContentPane();
        content.setLayout(new BoxLayout(addChildDialog.getContentPane(), BoxLayout.X_AXIS));

        content.add(makeAddNodePanel());
        content.add(new JSeparator(SwingConstants.VERTICAL));
        content.add(makeAddChildButtonPanel());
    }
    
    /** Currently does nothing, but is meant to create the dialog box for 
     * cloning an existing node in the tree.
     * Future Feature: automatically fill in the node's current parameters but 
     * allow user to modify these. Also, implement dragging of nodes to change 
     * their location in the tree. */
    public void makeCloneNodeDialog() {
        /*cloneNodeDialog = new JDialog(this, "Duplicate Node", true);
        Container content = addChildDialog.getContentPane();
        content.setLayout(new BoxLayout(addChildDialog.getContentPane(), BoxLayout.X_AXIS));

        content.add(makeAddNodePanel());
        content.add(new JSeparator(SwingConstants.VERTICAL));
        //content.add(makeAddChildButtonPanel());*/
    }
    
    /* Creates one of the primary panels for creating the "Add Child" dialog
     * box. 
     * @see makeAddChildButtonPanel()
     * These functions were seporated to accomodate the "clone Node" dialog. */
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
        
        JLabel attribLabel = new JLabel("Node Attribute(s) (optional list in the form name=node name, year=1975");
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
     
    /* Creates one of the primary panels for creating the "Add Child" dialog
     * box. 
     * @see makeAddNodePanel()
     * These functions were seporated to accomodate the "clone Node" dialog. */
    private JPanel makeAddChildButtonPanel() {   
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(0,1,5,5));
        //buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
        
        JButton addNodeButton = new JButton("Add Node");
        addNodeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addChildNode();
                hideAddChildDialog();
            }
        });
        
        JButton addAllButton = new JButton("Add Everywhere");
        addAllButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addChildrenNodes();
                hideAddChildDialog();
            }
        });
        
        JButton cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                hideAddChildDialog();
            }
        });
        
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
        buttonPanel.add(addNodeButton);
        buttonPanel.add(addAllButton);
        buttonPanel.add(Box.createRigidArea(new Dimension(0,5)));
        buttonPanel.add(cancelButton);
        buttonPanel.add(Box.createVerticalGlue());
        
        JPanel tempPanel = new JPanel();
        tempPanel.setLayout(new BoxLayout(tempPanel, BoxLayout.X_AXIS));
        tempPanel.add(new JSeparator(SwingConstants.VERTICAL));
        tempPanel.add(buttonPanel);
        
        return buttonPanel;
    }
    
    /** Parses the information entered into the addChildPanel.
     *
     * @return AdapterNode representation of the parameters */
    private AdapterNode extractNewChild() {
        String nodeName = nameField.getText().trim();
        String attribs = attribField.getText().trim();
        String value = valueField.getText().trim();
        int sIndex, eIndex, index;
        
        //create new node with given name
        AdapterNode newNode = new AdapterNode(nodeName);
        
        //add all attributes to the new node, if the exist
        //  assumes a comma-seporated list of attributes in the form: name=nodeName, year=1975, ...
        if (attribs.length() > 0) {
            eIndex = attribs.indexOf(',');
            sIndex = 0;
            String attrib, val;
            while (eIndex > 0) {
                attrib = attribs.substring(sIndex, eIndex);
                index = attrib.indexOf('=');
                val = attrib.substring(index+1).trim();
                attrib = attrib.substring(0, index).trim();
                newNode.setAttribute(attrib, val);
                
                sIndex = eIndex + 1;
                eIndex = attribs.indexOf(',', sIndex);
            }
            attrib = attribs.substring(sIndex);
            index = attrib.indexOf('=');
            val = attrib.substring(index+1).trim();
            attrib = attrib.substring(0, index).trim();
            newNode.setAttribute(attrib, val);
        }
        
        if (value.length() > 0) {
            newNode.setText(value);
        }
        
        return newNode;
    }
    
    /** Inserts a new node into the JTree at a specified location. Works in 
     * conjunction with the Add Child dialog box. Currently, always inserts
     * the new child at the end of the list of children. 
     * @see addChildrenNodes() */
    private void addChildNode() {
        //build the new child from info entered into Add Child dialog
        AdapterNode newChild = extractNewChild();
        
        JDomToTreeModelAdapter model = (JDomToTreeModelAdapter)tree.getModel();
        model.insertNodeInto(newChild, selectedPath, 0);
    }
    
    /** Inserts a new node into multiple locations in the JTree. Works in 
     * conjunction with the Add Child dialog box.
     *@see addChildNode() */
    private void addChildrenNodes() {
        //build the new child from info entered into Add Child dialog
        AdapterNode newNode = extractNewChild();
        
        JDomToTreeModelAdapter model = (JDomToTreeModelAdapter)tree.getModel();
        
        AdapterNode pathStep;
        Object[] pathNodes = selectedPath.getPath();
        
        //Vector queue = new Vector();
        Vector addedPaths = new Vector();
        
        //get a string representation of the path with just node names, not attributes
        String targetNodeNames = "";
        String targetLastName = "";
        for (int j = 0; j < pathNodes.length; j++) {
            pathStep = (AdapterNode)pathNodes[j];
            targetLastName = pathStep.getName();
            targetNodeNames += targetLastName;
        }
        
        Vector futureParents = new Vector();
        searchTree((AdapterNode)model.getRoot(), targetLastName, futureParents);
        
        //add newChild to every unique parent in futureParents
        Vector parentPathNodes = new Vector();
        TreePath parentPath;
        Iterator it = futureParents.iterator();
        AdapterNode parent;
        while (it.hasNext()) {
            parentPathNodes.clear();
            parent = (AdapterNode)it.next();
            while (parent != null) {
                parentPathNodes.add(0, parent);
                parent = parent.getParent();
            }
            
            parentPath = new TreePath(parentPathNodes.toArray());
            if (!addedPaths.contains(parentPath)) {
                model.insertNodeInto((AdapterNode)newNode.clone(), parentPath, 0);
                addedPaths.addElement(parentPath);
            }
        }
    }
    
    /** Recursively searches the tree for all instances of specified node name.
     * Used by Add Child to insert the new node into all appropriate subtrees.
     *
     * @param currNode the node curently being examined
     * @param targetName name of node that is being searched for
     * @param futureParents Vector of all nodes in the tree that meet the search
     *      criteria (currently, that will become parens of the new node with 
     *      node name <code> targetName </code>) */
    private void searchTree(AdapterNode currNode, String targetName, Vector futureParents) {
        if (currNode.getName().equals(targetName)) {
            futureParents.addElement(currNode);
            return;
        }
        
        java.util.List children = currNode.getChildren();
        Iterator it = children.iterator();
        while (it.hasNext()) {
            searchTree((AdapterNode)it.next(), targetName, futureParents);
        }
    }
    
    /** Displays the appropriate box for adding a child or duplicating a node.
     * This function does not creates the dialogs, simply clears their fields
     * (if necessary) and makes them visible.  */
    private void showAddChildDialog() {
        AdapterNode currParent = (AdapterNode)selectedPath.getLastPathComponent();
        infoLabel.setText("Adding child to " + currParent.toString());
        
        nameField.setText("");
        attribField.setText("");
        valueField.setText("");
        
        //display possible locations where to add node
        
        
        addChildDialog.pack();
        //center above the main window
        addChildDialog.setLocationRelativeTo(addChildDialog.getParent());
        addChildDialog.show();
    }
    
    /** Makes the Add Child dialog invisible */
    private void hideAddChildDialog() {
        addChildDialog.hide();
    }
    
    /** Deletes the currently selected node and all of its children. Checks the
     * selectedPath variable to determine which node to delete. */
    private void deleteNode() {
        AdapterNode currNode = (AdapterNode)selectedPath.getLastPathComponent();
        String message = "Are you sure you want to delete this node";
        
        if (currNode.isLeaf()) {
            message += "?";
        } else {
            message += " and all its children?";
        }
        
        int ans = JOptionPane.showConfirmDialog(this, message, "Delete Node", JOptionPane.YES_NO_OPTION);
        
        if (ans == JOptionPane.NO_OPTION) return;
        
        //delete the node
        JDomToTreeModelAdapter model = (JDomToTreeModelAdapter)tree.getModel();
        model.removeNodeFrom(selectedPath);   
    }
       
    /** Creates a new instance of a Thread that allows dynamic generation of 
     * the query line and table view. Once the user selects a node and 
     * right-clicks on the "Show Table and Query Line" option, the new thread is
     * used begin the query line process usually started by the user. The thread
     * fills a value in the latest combo box and yeilds to the main thread, 
     * which generates the following combo box, as usual. The new thread takes 
     * the role of the user. */
    protected void makeNewThread() {
        Thread newThread = new Thread() {
            public void run() {
                Object[] pathNodes = selectedPath.getPath();
                int j = 0;
                AdapterNode currNode = (AdapterNode)pathNodes[j];
                while (j-1 < pathNodes.length && !currNode.getName().equals("region")) {
                    currNode = (AdapterNode)pathNodes[j++];
                } 

                if (!currNode.getName().equals("region")) {
                    System.out.println("Can't show table, no region node found");
                    return;
                }

                if (regionBox == null) displayRegionBox();
                regionBox.setSelectedValue(currNode.getAttributeValue("name"), true);
                yield();

                int index = 1;
                JComboBox tempBox;
                String attrib;
                for (int k = j; k < pathNodes.length; k++) {
                    currNode = (AdapterNode)pathNodes[k];
                    tempBox = (JComboBox)queryControls.elementAt(index);
                    tempBox.setSelectedItem(currNode.getName());
                    yield();

                    attrib = currNode.getAttributeValue("name");
                    if (attrib != null) {
                        tempBox = (JComboBox)attributeControls.elementAt(index);
                        tempBox.setSelectedItem(attrib);
                        if (!tempBox.getSelectedItem().toString().equals(attrib)) {
                            tempBox.setSelectedItem(DEFAULT_PLURAL_STRING);
                        }
                    }
                    yield();

                    index++;            
                }
                if (currNode.getText() != null) readTableData();
            }
            
        };
        newThread.start();
    }
    
    /** Currently not called. Future Feature: clone an existing node. */
    private void cloneNode() {
        AdapterNode currNode = (AdapterNode)selectedPath.getLastPathComponent();
        
        nameField.setText(currNode.getName());
        attribField.setText(currNode.getAttributes());
        valueField.setText(currNode.getText());
        
        //display possible locations where to add node
        
        
        addChildDialog.pack();
        //center above the main window
        addChildDialog.setLocationRelativeTo(addChildDialog.getParent());
        addChildDialog.show();
    }
    
    /** The saves the current JTree (and its underlying JDOM tree) back to an
     * XML file. 
     * Future Feature: allow the user to save both XML and ZIP files. */
    private void saveFile() {
        XMLOutputter outputter = new XMLOutputter();
                
        int returnVal = fc.showSaveDialog(this.getContentPane());
        if (returnVal == JFileChooser.APPROVE_OPTION) {
            File file = fc.getSelectedFile();
            //jTFfileName.setText(file.getAbsolutePath());
            try {
                outputter.output(document, new FileOutputStream(file));
                nodeValueChangedFlag = false;
            } catch (Exception e) {
                System.err.println(e);
            }
        }            
           /* //determine whether the file is to be an xml or zip
            String fileName = file.toString();
            boolean zipping = false;
            int index = fileName.lastIndexOf('.');
            if (index > 0) {
                if (fileName.substring(index+1).equals("zip")) zipping = true;
            }
            
            try {
                if (!zipping) {
                    outputter.output(document, new FileOutputStream(file));
                    //nodeValueChangedFlag = false;
                } else {

                    byte[] buf = new byte[1024];
                    int len;
                    ZipEntry zipEntry = new ZipEntry(file.toString());
                    FileInputStream fin = new FileInputStream(file);
                    BufferedInputStream in = new BufferedInputStream(fin);
                    ZipOutputStream zos = new ZipOutputStream(new FileOutputStream("c:\\new.zip"));
                    zos.putNextEntry(zipEntry);
                    while ((len = in.read(buf, 0, 1024)) != -1)
                    {
                       zos.write(buf, 0, len);
                    }
                    
                    zipEntry.setSize(file.length());
                    zos.closeEntry();
                    //Close the input stream.
                    in.close();


                    nodeValueChangedFlag = false;
                }
            } catch (Exception e) {
                System.err.println(e);
            }
        }*/
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
    
}