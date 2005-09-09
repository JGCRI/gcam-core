package ModelInterface.ModelGUI2.tables;

import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;

import java.util.*;
import javax.swing.table.AbstractTableModel;
import org.w3c.dom.*;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.TreePath;
import java.awt.event.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

//import java.sql.Statement;
import org.apache.poi.hssf.usermodel.*;

import org.apache.xpath.domapi.*;
import org.jfree.chart.JFreeChart;
import org.w3c.dom.xpath.*;

public abstract class BaseTableModel extends AbstractTableModel {
	protected Vector activeRows;
	protected Document doc;
	protected ArrayList wild;
	protected Map tableFilterMaps;
	protected Frame parentFrame;
	protected String title;

	// stuff for filtering
	// can i move these somewhere
	protected int currFilter;
	protected String[] currKeys;
	
	protected String tableTypeString;

	/** 
	 * Default Constuctor
	 */
	public BaseTableModel() {}

	/**
	 * Constuctor initializes some necessary data members
	 * @param tp Not required in the BaseTableModel, but will be in derived class to create xpath
	 * 	  doc DOM document, necessary so that we can run an Xpath query on it.
	 * 	  parentFrame Reference to the main gui so that we can create a dialog
	 * 	  tableTypeString used to display which type of table the user is looking at
	 */
	public BaseTableModel(TreePath tp, Document doc, JFrame parentFrameIn, String tableTypeString) {
		this.doc = doc;
		this.parentFrame = parentFrameIn;
		this.tableTypeString = tableTypeString;
		this.title = ((DOMmodel.DOMNodeAdapter)tp.getLastPathComponent()).getNode().getNodeName();
		final BaseTableModel thisTableModel = this;
		parentFrame.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent e) {
				if(e.getPropertyName().equals("Control") || (e.getPropertyName().equals("Table") &&
						!thisTableModel.equals(e.getNewValue()))) {
					System.out.println("New Table: "+e.getNewValue());
					System.out.println("This Table: "+thisTableModel);
					System.out.println("Stoped listening for filter");
					parentFrame.removePropertyChangeListener(this);
				} else if(e.getPropertyName().equals("Filter")) {
					try {
						// is there a better way than to do this..
						if(thisTableModel instanceof NewDataTableModel) {
							//((NewDataTableModel)thisTableModel).filterData();
							JOptionPane.showMessageDialog(parentFrame,
								"This table does not support filtering",
								"Table Filter Error", JOptionPane.ERROR_MESSAGE);
						} else {
							thisTableModel.filterData();
						}
					} catch (UnsupportedOperationException uoe) {
						JOptionPane.showMessageDialog(parentFrame,
							"This table does not support filtering",
							"Table Filter Error", JOptionPane.ERROR_MESSAGE);
					}
				}
			}
		});
	}
	
	/** 
	 * Creates a chart from the data in the table.
	 * @author jlurz
	 */
	public abstract JFreeChart createChart(int rowAt, int colAt);
	
	/**
	 * abstract the deriving class needs to implement this for flip functionality, params used 
	 * mostly for MultiTable to know which table to pass the call on to
	 * @param row the row over which flip was called
	 * 	  col the col over which flip was called
	 */
	public abstract void flip(int row, int col);

	/**
	 * The deriving class needs to implement this so that it gets the nodes at given rows/cols and gets their annotation from
	 * the documentation
	 * @param row rows positions of the nodes to be annotated
	 * 	  col cols positions of the nodes to be annotated 
	 * 	  documentation the documentation where to look up annotation information
	 */
	public abstract void annotate(int[] rows, int[] cols, Documentation documentation);

	/**
	 * abstract since deriving tables would most likely have diffrent method for building a table depending on what the purpose of the table is
	 * @param xpe the Xpath expression that it will run to get nodes to build the table
	 */
	protected abstract void buildTable(XPathExpression xpe);
	
	/**
	 * this funtion is used to convert a treepath to and xpath expression, a treepath is an
	 * array of nodes which represent the parent path of the desired leaf node
	 * @param tp the TreePath to be converted
	 *        currNode current node the last node (leaf node that was clicked) !! is this really needed
	 *        flag used to chacnge the strictness, 0 won't specify any attrributes, 1 will specify all attrubutes except any wild nodes
	 */
 	protected XPathExpression treePathtoXPath(TreePath tp, Node currNode, int flag) {
	   // attempts to put the tp in form /nodeName[@attributeName=attrubuteValue]/childNode
           XPathEvaluatorImpl xpeImpl = new XPathEvaluatorImpl(doc);
           //String pathStr = "";
	   StringBuffer pathStr = new StringBuffer("/");
           Object[] path = tp.getPath();
           Node tempNode;
	   currNode = ((DOMmodel.DOMNodeAdapter)path[path.length-1]).getNode();
           for (int i = 0; i < path.length-1; i++) {
	           tempNode= ((DOMmodel.DOMNodeAdapter)path[i]).getNode();
		   if(flag == 0) {
                   	pathStr.append(tempNode.getNodeName()).append("/");
		   } else if(flag == 1) { 
			   pathStr.append(tempNode.getNodeName()); 
			   Vector attrs = getAttrsNoWild(tempNode);
			   if(attrs.size() > 0) {
				   pathStr.append("[");
			   }
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr.append("(@").append(((Node)attrs.get(j)).getNodeName())
					   .append("='").append(((Node)attrs.get(j)).getNodeValue()).append("')");
				   if(j < attrs.size()-1) {
					   pathStr.append(" and ");
				   } else { 
					   pathStr.append("]");
				   }
			   }
			   pathStr.append("/");
		   }
           }
           //pathStr = "/" + pathStr + currNode.getNodeName();
           pathStr.append(currNode.getNodeName());
           if (flag == 1 &&currNode.hasAttributes() && !getTextData(currNode).equals("")) {
		   if(flag == 0) {
                   	pathStr.append("[@").append(getOneAttrVal(currNode)).append("]");
		   } else if(flag == 1) {
			   Vector attrs = getAttrsNoWild(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr.append("[@").append(((Node)attrs.get(j)).getNodeName())
					   .append("='").append(((Node)attrs.get(j)).getNodeValue()).append("']");
			   }
			   pathStr.append("/text()");
		   }
           }
           else if (flag == 1 && currNode.hasAttributes()) {
		   if (flag == 0) {
                   	pathStr.append("[@").append(getOneAttrVal(currNode)).append("]/text()");
		   } else if(flag == 1) {
			   Vector attrs =getAttrsNoWild(currNode);
			   for(int j=0; j < attrs.size(); j++) {
				   pathStr.append("@").append(((Node)attrs.get(j)).getNodeName()).append("='")
					   .append(((Node)attrs.get(j)).getNodeValue()).append("'");
			   }
			   pathStr.append("/text()");
		   }
           }
           else {
                   pathStr.append("/text()");
           }
	   System.out.println(pathStr.toString());
           return xpeImpl.createExpression(pathStr.toString(), xpeImpl.createNSResolver(currNode));
	}

  /**
   * Gets the first attrubute name, value pair, and returns it in a string with the format 
   * name=value
   * @param node the node which you want to get the attrubute from
   * @return a string in the format name=value
   */
  public String getOneAttrVal(Node node) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  if(nodeMap.item(0).getNodeName().equals("fillout")) {
		  return getOneAttrVal(node, 1);
	  }
	  //return nodeMap.item(0).getNodeName() +"="+ nodeMap.item(0).getNodeValue();
	  return /*((Element)node).getAttribute(*/nodeMap.item(0).getNodeValue()/*)*/;
  }

  /**
   * Gets the attrubute name, value pair in the requested position and returns it in a string with the format 
   * name=value
   * @param node the node which you want to get the attrubute from
   *        pos indicates which attrubute is wanted
   * @return a string in the format name=value
   */
  public String getOneAttrVal(Node node, int pos) {
	  NamedNodeMap nodeMap = node.getAttributes();
	  //return nodeMap.item(pos).getNodeName() +"="+ nodeMap.item(pos).getNodeValue();
	  return /*((Element)node).getAttribute(*/nodeMap.item(pos).getNodeValue()/*)*/;
  }

  /**
   * Gets all the attrubutes for the node and returns the ones that are no wild in a vector
   * @param node the node which attributes will come form
   * @return the vector off all the attributes
   */
  public Vector getAttrsNoWild(Node node) {
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && node.getNodeName().equals("subsector") ) {
		  return new Vector();
	  }

	  if(node.getNodeName().equals((String)wild.get(0)) || node.getNodeName().equals((String)wild.get(1)) ) { 
		 return new Vector();
	  }
	  NamedNodeMap nodeMap = node.getAttributes();
	  Node tempNode;
	  Vector ret = new Vector();
	  for(int i = 0; i < nodeMap.getLength(); i++) {
		  tempNode = nodeMap.item(i);
		  if(tempNode.getNodeName().indexOf(':') == -1) {
		  	ret.add(tempNode);
		  }
	  }
	  return ret;
  }

  /**
   * Looks to find a text node child of a node, and returns it's contents
   * @param node the node used to look for a text child
   * @return String with the value of the text node if found, else empty string
   */
  public String getTextData(Node node) {
	  NodeList nl = node.getChildNodes();
	  for (int i = 0; i < nl.getLength(); i++) {
		  if (nl.item(i).getNodeType() == Node.TEXT_NODE) {
			  return nl.item(i).getNodeValue();
		  }
	  }
	  return "";
  }

  /**
   * Creates a dialog box so that the user can select the 2 axis for a table, and returns a list of the 2 nodes, which will be used for the wild
   * @param path the path which was selected in the tree, so we can get a list of canidates for the 2 axes.
   *        parentFrame so we can create a dialog associated with the parentFrame
   * @return an ArrayList of the 2 nodes selected from the dialog
   */
  protected ArrayList chooseTableHeaders( TreePath path/*, JFrame parentFrame*/ ){
	final ArrayList selected = new ArrayList(2);

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
	final JList list = new JList(items);
	list.setSelectionMode(DefaultListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
    
	JScrollPane scrollingList = new JScrollPane(list);
    		
	final JDialog filterDialog = new JDialog(parentFrame, tableTypeString + " for \'" + ((DOMmodel.DOMNodeAdapter)itemsObjs[itemsObjs.length-1]).getNode().getNodeName() + "\'. Please choose two headers:", true);
	filterDialog.setSize(500,400);
	filterDialog.setLocation(100,100);
	filterDialog.setResizable(false);
	
	final JButton nextButton = new JButton(" Finished With Selection ");
	nextButton.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
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
	//filterDialog.show();
	filterDialog.setVisible(true);
   		
  	return selected; //arraylist with the two selected nodes
  }

        /**
	 * Creates a dialog and lists attrubutes of the nodes which can be filtered, while updating
	 * the filterMaps to keep track of what the user has selected
	 * @param parentFrame So that we can create a dialog
	 */
	public void filterData(/*JFrame parentFrame*/) {
		// so i can make oldNumRows final and it won't crash
		if (activeRows == null) {
			activeRows = new Vector();
		}
		// create some copies, so if the use cancels we can reset the old value
		final int oldNumRows = activeRows.size();
		currKeys = new String[0];
		final Map tempFilterMaps = (Map)((LinkedHashMap)tableFilterMaps).clone();

		// a vector of nodeName which can be filtered
		// we go through this list backwards so that nodes apear in the same order they do
		// in the tree
		final Vector possibleKeys = new Vector(tempFilterMaps.keySet());
		currFilter = possibleKeys.size()-1;
		String title = "Filter Table";
		if (possibleKeys.isEmpty()) {
			return;
		}
		final JDialog filterDialog = new JDialog(parentFrame, title, true);
		filterDialog.setSize(500,400);
		filterDialog.setLocation(100,100);
		filterDialog.setResizable(false);

		final JList list = new JList();
		final JLabel listLabel = new JLabel();
		listLabel.setHorizontalAlignment(JLabel.LEFT);
		updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);

		final String cancelTitle = " Cancel ";

		final JButton cancelButton = new JButton(cancelTitle);
		final JButton backButton = new JButton(" < Back ");
		final JButton nextButton = new JButton(" Next > ");

		backButton.setMnemonic(KeyEvent.VK_B);
		nextButton.setMnemonic(KeyEvent.VK_N);
		// adds the listener for the cancel button, and when we have reached the end it will change it's
		// title to finished, which tells use to go ahead and update the filters, and call doFilter
	        cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!cancelButton.getText().equals(cancelTitle)) {
					updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
					tableFilterMaps = tempFilterMaps;
					doFilter(possibleKeys);
					if (oldNumRows < activeRows.size()) {
						fireTableRowsInserted(oldNumRows, activeRows.size());
					} else if (oldNumRows > activeRows.size()) {
						fireTableRowsDeleted(0, activeRows.size());
					} else {
						fireTableRowsUpdated(0,activeRows.size());
					}
				}
				//exit this dialog..
				filterDialog.dispose();
				//filterDialog.hide();
			}
		});

            // when we go back, if we can, get the previous nodeName, and load up all its attributes
	    // make the cancel buttons title cancel again if it was on finished
	    backButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				//currFilter--;
				currFilter++;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!nextButton.isEnabled()) {
					nextButton.setEnabled(true);
					cancelButton.setText(cancelTitle);
				}
				if (currFilter == possibleKeys.size()-1) {
					backButton.setEnabled(false);
				}
			}
		});

            // when we go next, if we can, get the next nodeName, and load up all its attributes
	    // make the cancel buttons title finished if we have reached the end of the list of nodeNames 
	    nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				//currFilter++;
				currFilter--;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!backButton.isEnabled()) {
					backButton.setEnabled(true);
				}
				if (currFilter == 0) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				}
			}
		});

	        // set display properties
		JPanel buttonPane = new JPanel();
	    	buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
	    	buttonPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
	    	buttonPane.add(Box.createHorizontalGlue());
		buttonPane.add(backButton);
		backButton.setEnabled(false);
		if (possibleKeys.size() == 1) {
			nextButton.setEnabled(false);
			cancelButton.setText("Finished");
		}

	    	buttonPane.add(nextButton);
	    	buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
	    	buttonPane.add(cancelButton);

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
		//filterDialog.show();
		filterDialog.setVisible(true);
	}

	/**
	 * Update the filterMaps of the users selections
	 * @param tempFilterMaps the current set of filterMaps being changed by user selections
	 *        list the list of values the user can select
	 *        key the current key in the filterMaps being edited
	 */
	protected void updateFilters(Map tempFilterMaps, JList list, String key) {
		int[] selectedKeys = list.getSelectedIndices();
		int j = 0;
		// since the list only returns the selected ones makes things complicated
		// need to go through and change boolean values of the maps, kind of like an XOR
		for (int i = 0; i < currKeys.length; i++) {
			// clean this up... maybe
			if (((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j >= selectedKeys.length || i != selectedKeys[j])) {
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(false));
			} else if (!((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j < selectedKeys.length && i == selectedKeys[j])) {
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(true));
			}
			if (j < selectedKeys.length && i == selectedKeys[j]) {
				j++;
			}
		}
	}

	/**
	 * Update the JList which is displaced to the user for selection
	 * @param list the list which will change
	 *        listLable the nodeName that these attributes belong to
	 *        key the current nodeName, used as the current key into the filter map
	 *        tempFilterMaps the set of filterMaps being edited by the user
	 */
	protected void updateList(JList list, JLabel listLabel, String key, Map tempFilterMaps) {
		HashMap tempMap = (HashMap)tempFilterMaps.get(key);
		Vector tempVector = new Vector();
		listLabel.setText("Filter "+key);
		currKeys = (String[])tempMap.keySet().toArray(new String[0]);
		list.setListData(currKeys);
		// check the maps to see which ones are true and add it to the list of selected
		for (int i = 0; i < currKeys.length; i++) {
			if (((Boolean)tempMap.get(currKeys[i])).booleanValue()) {
				tempVector.addElement(new Integer(i));
			}
		}
		int[] selected = new int[tempVector.size()];
		for (int i = 0; i < selected.length; i++) {
			selected[i] = ((Integer)tempVector.get(i)).intValue();
		}
		tempMap = null;
		tempVector = null;
		list.setSelectedIndices(selected);
	}

	/**
	 * abstract since most the of the deriving tables will store the data differently
	 * @param possibleFilters the vector nodeNames that had valid attributes for filtering
	 */
	protected abstract void doFilter(Vector possibleFilters);

	public abstract void exportToExcel(HSSFSheet sheet, HSSFWorkbook wb, HSSFPatriarch dp);

}
