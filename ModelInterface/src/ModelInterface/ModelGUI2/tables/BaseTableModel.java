package ModelInterface.ModelGUI2.tables;

import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.InterfaceMain;

import java.util.*;
import javax.swing.table.AbstractTableModel;
import org.w3c.dom.*;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.TreePath;
import java.awt.event.*;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.awt.Component;
import java.awt.Color;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableModel;

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
	protected Documentation documentation;
	protected String units;
	protected boolean remove1975;
	protected TableSorter sortedTable;
	private static java.util.List<String> defaultYearList;

	// stuff for filtering
	// can i move these somewhere
	protected int currFilter;
	protected String[] currKeys;
	
	protected String tableTypeString;

	/** 
	 * Default Constuctor
	 */
	public BaseTableModel() {
		// TODO: Create a prefrence to control removing 
		// the year1975.  Currently this value is just 
		// taken from the global properties file but it 
		// should be able to change while the Interface is 
		// running perhaps even on a per-query basis.
		if(InterfaceMain.getInstance() != null) {
			Properties globalProperties = InterfaceMain.getInstance().getProperties();
			String remove75Str;
			globalProperties.setProperty("remove1975", 
					remove75Str = globalProperties.getProperty("remove1975", "true"));
			remove1975 = Boolean.parseBoolean(remove75Str);
		} else {
			remove1975 = false;
		}
		sortedTable = new TableSorter(this);
	}

	/**
	 * Constuctor initializes some necessary data members
	 * @param tp Not required in the BaseTableModel, but will be in derived class to create xpath
	 * 	  doc DOM document, necessary so that we can run an Xpath query on it.
	 * 	  parentFrame Reference to the main gui so that we can create a dialog
	 * 	  tableTypeString used to display which type of table the user is looking at
	 */
	public BaseTableModel(TreePath tp, Document doc, JFrame parentFrameIn, String tableTypeString, Documentation documentationIn) {
		this.doc = doc;
		this.parentFrame = parentFrameIn;
		this.tableTypeString = tableTypeString;
		this.documentation = documentationIn;
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
	 * Get the cell renderer at specified position
	 * @param row renderer for row
	 * @param col renderer for col
	 * @return the renderer required to view the cell
	 */
	public abstract TableCellRenderer getCellRenderer(int row, int pos);
	
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
	  return nodeMap.item(0).getNodeName() +"="+ nodeMap.item(0).getNodeValue();
	  //return /*((Element)node).getAttribute(*/nodeMap.item(0).getNodeValue()/*)*/;
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
			if (((Boolean)((Map)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j >= selectedKeys.length || i != selectedKeys[j])) {
				((Map)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(false));
			} else if (!((Boolean)((Map)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j < selectedKeys.length && i == selectedKeys[j])) {
				((Map)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(true));
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
		Map tempMap = (Map)tempFilterMaps.get(key);
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

	protected TableCellRenderer getDocumentationRenderer() {
		final TableCellRenderer defaultRenderer = new DefaultTableCellRenderer();
		return new TableCellRenderer() {
			public Component getTableCellRendererComponent(JTable jTable, Object value, boolean isSelected, boolean hasFocus,
					int row, int col) {
				Component ret = defaultRenderer.getTableCellRendererComponent(jTable, value, isSelected, hasFocus, row, col);
				if(documentation != null && documentation.hasDocumentation(getNodeAt(row, col))) {
					ret.setBackground(isSelected? Color.BLUE : Color.CYAN);
				} else {
					ret.setBackground(isSelected? Color.BLUE : Color.WHITE);
				}
				return ret;
			}
		};
	}
	
	/**
	 * Get the backing node in the cell specified
	 * @param row which row
	 * @param col which col
	 * @return the node desired or null if not given a valid position
	 */
	protected abstract Node getNodeAt(int row, int col);

	/**
	 * Determine a prefered dimension size to create an image from a JFreeChart.  The problem is
	 * when there are too many series the legend gets too big an pushes out the chart itself.
	 * @param chart a JFreeChart with data that would be used to create an image from
	 * @return A dimension that will be able to show all of the chart and legend
	 */
	public Dimension getChartDimensions(JFreeChart chart) {
		//System.out.println("Num legend items: "+chart.getPlot().getLegendItems().getItemCount());
		//int numItems = chart.getPlot().getLegendItems().getItemCount();
		int legendSize = 0;
		for(Iterator it = chart.getPlot().getLegendItems().iterator(); it.hasNext(); ) {
			/*
			String temp = ((org.jfree.chart.LegendItem)it.next()).getLabel();
			System.out.println("Lengend item: "+temp);
			*/
			legendSize += ((org.jfree.chart.LegendItem)it.next()).getLabel().length();
		}
		//System.out.println("Total length: "+legendSize);
		if(legendSize <= 500) {
			//System.out.println("Returning: 350, "+350);
			return new Dimension(350, 350);
		} else {
			//System.out.println("Returning: 350, "+(350+((legendSize-500)/2)));
			return new Dimension(350, 350+((legendSize-500)/2));
		}
	}

	/**
	 * Set the active rows of this table.  This method is essentially setting the filter, and should
	 * usually not be called directly.
	 * @param newActive the new Vector of indices which are active
	 */
	public void setActiveRows(Vector newActive) {
		activeRows = newActive;
		// correct event to fire?
		fireTableStructureChanged();
	}

	/**
	 * Used to export a table to text. There is a newline between rows and
	 * a tab between columns.
	 * @return A String which represents this table.
	 */
       public String exportToText() {
	       String lineEnding = System.getProperty("line.separator");
	       StringBuilder ret = new StringBuilder();
	       ret.append(title).append(lineEnding);
	       for(int i = 0; i < getColumnCount(); ++i) {
		       ret.append(getColumnName(i)).append("\t");
	       }
	       ret.append(lineEnding);
	       for(int row = 0; row < getRowCount(); ++row) {
		       for(int col = 0; col < getColumnCount(); ++col) {
			       ret.append(sortedTable.getValueAt(row, col).toString()).append("\t");
		       }
		       ret.append(lineEnding);
	       }
	       return ret.toString();
       }

       public JTable getAsSortedTable() {
	       JTable ret = new JTable(sortedTable);
	       sortedTable.setTableHeader(ret.getTableHeader());
	       return ret;
       }

       public java.util.List<String> getDefaultYearList() {
	       // the default year list could go in a preference dialog as well
	       // WARNING: not thread safe
	       if(defaultYearList == null) {
		       if(InterfaceMain.getInstance() != null) {
			       Properties globalProperties = InterfaceMain.getInstance().getProperties();
			       String defaultYearStr;
			       globalProperties.setProperty("defaultYearList", defaultYearStr = 
					       globalProperties.getProperty("defaultYearList", 
						       "1990;2005;2020;2035;2050;2065;2080;2095"));
			       String[] yearsArr = defaultYearStr.split(";");
			       defaultYearList = new ArrayList<String>(yearsArr.length);
			       if(!(yearsArr.length == 1 && yearsArr[0].equals(""))) {
				       for(String year : yearsArr ) {
					       defaultYearList.add(year);
				       }
			       }
		       } else {
			       defaultYearList = new ArrayList<String>();
		       }
	       }
	       return defaultYearList;
       }
}
