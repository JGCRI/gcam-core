
import java.util.*;
import org.w3c.dom.*;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JScrollPane;
import javax.swing.*;
import javax.swing.tree.TreePath;
import org.w3c.dom.xpath.*;

public class MultiTableModel extends BaseTableModel{
	// used to be able to edit tables in a cell
	// don't really know what to do here
	// it seems to work, but obviously this isn't correct
	private class TableEditor implements TableCellEditor {
		public TableEditor () {}
		public void removeCellEditorListener(javax.swing.event.CellEditorListener cE ) {
		}
		public Object getCellEditorValue() {
			return "I DON'T KNOW";
		}
		public boolean stopCellEditing() {
			return true;
		}
		public void cancelCellEditing() {
		}
		public boolean isCellEditable(EventObject eO) {
			return true;
		}
		public boolean shouldSelectCell(EventObject eO) {
			return true;
		}
		public void addCellEditorListener(javax.swing.event.CellEditorListener cE ) {
		}
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int col) {
			return (JScrollPane)value;
		}
	}
	// to be able to render a table inside a cell
	private class TableRenderer implements TableCellRenderer {
		public TableRenderer () {}
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col ) {
			if(row % 2 == 0) {
				return (new DefaultTableCellRenderer()).getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
			} else {
				return (JScrollPane)value;
			}
		}
	}
	Vector tables;
	TableRenderer tableRenderer;
	TableEditor tableEditor;

	/**
	 * Constructor initializes data members, and calls buildTable to initialize data, and filterMaps
	 * and create the individual tables
	 * @param tp the Tree Path which was selected from the tree, needed to build table
	 *        doc needed to run the XPath query against
	 *        parentFrame needed to create dialogs
	 *        tableTypeString to be able to display the type of table this is
	 */
	public MultiTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString) {
		super(tp, doc, parentFrame, tableTypeString);
		wild = chooseTableHeaders(tp, parentFrame);
	        wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
	        wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 0));
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		activeRows = new Vector(tables.size());
		for(int i = 0; i < tables.size(); i++) {
			activeRows.add(new Integer(i));
		}
	}
	/**
	 * flipps the axis of the individual table
	 * @param row used to figure out which cell needs to be flipped
	 *        col not really important since we only have 1 col
	 */
	public void flip(int row, int col) {
				((NewDataTableModel)((JTable)((JScrollPane)getValueAt(row, col)).getViewport().getView()).getModel()).flip(row, col);
	}

	/**
	 * Runs an XPath expression to get a set of nodes, which then are sorted, based on its path in
	 * the tree.  Uses the sorted data to create a set of tables, also initalizes the filterMaps.
	 * @param xpe the XPath expression which will be used to get nodes.
	 */
  	protected void buildTable(XPathExpression xpe) {
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  Object[] regionAndYear;
	  TreeSet regions = new TreeSet();
	  TreeSet years = new TreeSet();
	  tableFilterMaps = new LinkedHashMap();
	  TreeMap dataTree = new TreeMap();
	  while ((tempNode = res.iterateNext()) != null) {
		regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode(), tableFilterMaps);
		regions.add(regionAndYear[0]);
		years.add(regionAndYear[1]);
		addToDataTree(tempNode, dataTree).put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
	  }
	  recAddTables(dataTree, null, regions, years, "");
  	}

	/**
	 * Gets the 2 attributes of the 2 wilds from going up the parent path of a node, also update the
	 * filter maps
	 * @param n the node whos wild node's attrubutes need to be determined
	 *        filterMaps maps which has the filtering information, which will be updated with the attribute value from this nodes parent path
	 * @return an array of size 2 with the attrubute values of the wild which lead to this node
	 */
  	private Object[] getRegionAndYearFromNode(Node n, Map filterMaps) {
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
			  if(attr.equals("fillout=1")) {
				  attr = getOneAttrVal(n, 1);
			  }
			  if (!tempFilter.containsKey(attr)) {
                          	tempFilter.put(attr, new Boolean(true));
                          	filterMaps.put(n.getNodeName(), tempFilter);
			  }
		  }
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
  	}
  /**
   * Sort data so that we know where each set of data comes from. Recursivly moves to the top of the parentPath
   * and at each level crates/uses the appropriate mapping for this node
   * @param currNode current node we are analyzing in the tree
   *        dataTree the complete set of maps sorting the data
   * @return the current mapping that was just created/used
   */
  private TreeMap addToDataTree(Node currNode, TreeMap dataTree) {
	  if (currNode.getNodeType() == Node.DOCUMENT_NODE) {
		  return dataTree;
	  }
	  TreeMap tempMap = addToDataTree(currNode.getParentNode(), dataTree);
	  // used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && currNode.getNodeName().equals(".*[Ss]ector") ) {
		  return tempMap;
	  }
	  if(currNode.hasAttributes() && !currNode.getNodeName().equals((String)wild.get(0)) && !currNode.getNodeName().equals((String)wild.get(1))) {
		String attr = getOneAttrVal(currNode);
		if(attr.equals("fillout=1")) {
			attr = getOneAttrVal(currNode, 1);
		}
		attr = currNode.getNodeName()+"@"+attr;
		if(!tempMap.containsKey(attr)) {
			tempMap.put(attr, new TreeMap());
		}
		return (TreeMap)tempMap.get(attr);
	  }
	  return tempMap;
  }

  /**
   * Move down the dataTree map until we hit the level of node, as apposed to mappin, then the mapping 
   * one level up is the data map for a table, and it's path is described by title
   * @param dataTree the mappings of attrubutes which will get us to the data
   *        parent so that we can get the data map which is a level up once we hit the bottom
   *        regions column axis attrubutes
   *        years row axis attributes
   *        title a string describing the path in which the data in the table is coming from
   */
  private void recAddTables(TreeMap dataTree, Map.Entry parent, TreeSet regions, TreeSet years, String title) {
	Iterator it = dataTree.entrySet().iterator();
	while(it.hasNext()) {
		Map.Entry me = (Map.Entry)it.next();
		if(me.getValue() instanceof Node) {
	  		NewDataTableModel tM = new NewDataTableModel(regions, (String)wild.get(0), years, (String)wild.get(1), title+'/'+(String)parent.getKey(), (TreeMap)parent.getValue(), doc); 
	  		JTable jTable = new JTable(tM);

	  		jTable.getModel().addTableModelListener((FileChooserDemo)parentFrame);

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
			tables.add(title+"/");
	  		tables.add(tableView);
			return;
		} else {
			recAddTables((TreeMap)me.getValue(), me, regions, years, title+'/'+(String)me.getKey());
		}
	}
  }
        /**
	 * gets the instance of table editor used to be able to edit a table within a table cell
	 * @return tableEditor
	 */
	public TableCellEditor getCellEditor(int row, int col ) {
			return tableEditor;
	}

        /**
	 * gets the instance of table renderer used to be able to view a table within a table cell
	 * @return tableRenderer
	 */
	public TableCellRenderer getCellRenderer(int row, int col ) {
			return tableRenderer;
	}

	/**
	 * get the number of columns in the table
	 * @return always returns 1
	 */
	public int getColumnCount() {
		return 1;
	}

	/**
	 * Get the number of rows in the table. This is really tables * 2, since each table has a label.
	 * Also need to account for the tables which have been filtered out.
	 * @return The number of elements in activeRows
	 */
	public int getRowCount() {
		return activeRows.size();
	}

	/**
	 * returns the table at the requested cell
	 * @param row the row position of the cell
	 *        col the column position of the cell
	 * @return the table at the requested cell
	 */
	public Object getValueAt(int row, int col) {
		return tables.get(((Integer)activeRows.get(row)).intValue());
	}

	/**
	 * return the heading for the column
	 * @param col there is only really 1 column, so not used
	 * @return heading for the column
	 */
	public String getColumnName(int col) {
		return "Stuff"; // should have a more meaningful title
	}

	/**
	 * determines wheter a cell is editable, only tables are editable, which are every other row.
	 * @param row the row position being queryed
	 *        col the column position being queryed
	 * @return true or false depeneding on if the cell is editable
	 */
	public boolean isCellEditable(int row, int col) {
		if(row % 2 == 0) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Updates activeRows to include only the tables which didn't come from any of the attributes filtered
	 * out. Does this by creating a regular expression in the form /nodeName[attrName=attrVal | any more
	 * attrubutes which are still valid]/next child. Then tests each table's label against the regular
	 * expression
	 * @param possibleFilters the vector nodeNames that had valid attributes for filtering
	 */
	protected void doFilter(Vector possibleFilters) {
		String regex = "^/";
		for(int i = possibleFilters.size()-1; i >= 0; i--) {
			Iterator it = ((HashMap)tableFilterMaps.get(possibleFilters.get(i))).entrySet().iterator();
			if(it.hasNext()) {
				regex += (String)possibleFilters.get(i)+":(";
				while(it.hasNext()) {
					Map.Entry me = (Map.Entry)it.next();
					if(((Boolean)me.getValue()).booleanValue()) {
						regex += me.getKey()+"|";
					}
				}
				if(regex.endsWith("|")) {
					regex = regex.substring(0,regex.length()-1)+")/";
				} else {
					regex += ")/";
				}
			} else {
				regex += (String)possibleFilters.get(i)+"/";
			}
		}
		regex += "$";
		Vector tempActive = new Vector();
		for(int i = 0; i < tables.size(); i+=2) {
			if(((String)tables.get(i)).matches(regex)) {
				tempActive.add(new Integer(i));
				tempActive.add(new Integer(i+1));
			}
		}
		activeRows = tempActive;
	}
}
