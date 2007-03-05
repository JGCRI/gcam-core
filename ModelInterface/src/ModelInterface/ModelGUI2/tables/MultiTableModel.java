package ModelInterface.ModelGUI2.tables;

import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.queries.QueryGenerator;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.ModelGUI2.XMLDB;

import java.util.*;

import org.apache.poi.hssf.usermodel.*;

import org.jfree.chart.JFreeChart;
import java.awt.image.BufferedImage;
import org.w3c.dom.*;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.awt.Color;
import java.awt.Dimension;

import javax.swing.*;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.tree.TreePath;
import org.w3c.dom.xpath.*;

import com.sleepycat.dbxml.*;

public class MultiTableModel extends BaseTableModel{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// used to be able to edit tables in a cell
	// don't really know what to do here
	// it seems to work, but obviously this isn't correct
	private class TableEditor implements TableCellEditor {
		private Vector<CellEditorListener> editListeners;
		public TableEditor () {
			editListeners = new Vector<CellEditorListener>();
		}
		public void removeCellEditorListener(CellEditorListener cE ) {
			editListeners.remove(cE);
		}
		public Object getCellEditorValue() {
			System.out.println("Cell Editor Value");
			return "I DON'T KNOW";
		}
		public boolean stopCellEditing() {
			fireEditingStopped();
			return true;
		}
		public void cancelCellEditing() {
			fireEditingCanceled();
		}
		public boolean isCellEditable(EventObject eO) {
			return true;
		}
		public boolean shouldSelectCell(EventObject eO) {
			return true;
		}
		public void addCellEditorListener(CellEditorListener cE ) {
			editListeners.add(cE);
		}
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int col) {
			return (JScrollPane)value;
		}
		private void fireEditingStopped() {
			ChangeEvent ce = new ChangeEvent(this);
			for(int i = 0; i < editListeners.size(); ++i) {
				editListeners.get(i).editingStopped(ce);
			}
		}
		private void fireEditingCanceled() {
			ChangeEvent ce = new ChangeEvent(this);
			for(int i = 0; i < editListeners.size(); ++i) {
				editListeners.get(i).editingCanceled(ce);
			}
		}
	}
	// to be able to render a table inside a cell
	private class TableRenderer implements TableCellRenderer {
		public TableRenderer () {}
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col ) {
			if(row % 2 == 0) {
				Component comp = (new DefaultTableCellRenderer()).getTableCellRendererComponent(table, value, isSelected, hasFocus, row, col);
				if(table.getRowHeight(row) != 16) {
					table.setRowHeight(row, 16);
				}
				comp.setBackground(new Color(240,214,19));
				return comp;
			} else {
				if(table.getRowHeight(row) != (int)((JScrollPane)value).getPreferredSize().getHeight()+10) {
					table.setRowHeight(row, (int)((JScrollPane)value).getPreferredSize().getHeight() +10);
				}
				return (JScrollPane)value;
				//return (JPanel)value;
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
	public MultiTableModel(TreePath tp, Document doc, JFrame parentFrame, String tableTypeString, Documentation documentationIn) {
		super(tp, doc, parentFrame, tableTypeString, documentationIn);
		wild = chooseTableHeaders(tp/*, parentFrame*/);
	        wild.set(0, ((DOMmodel.DOMNodeAdapter)wild.get(0)).getNode().getNodeName());
	        wild.set(1, ((DOMmodel.DOMNodeAdapter)wild.get(1)).getNode().getNodeName());
		wild.add("");
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 0));
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		activeRows = new Vector(tables.size());
		for(int i = 0; i < tables.size(); i++) {
			activeRows.add(new Integer(i));
		}
	}
	/**
	 * flips the axis of the individual table
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
	  Map dataTree = new TreeMap();
	  while ((tempNode = res.iterateNext()) != null) {
		regionAndYear = getRegionAndYearFromNode(tempNode.getParentNode(), tableFilterMaps);
		regions.add(regionAndYear[0]);
		years.add(regionAndYear[1]);
		addToDataTree(tempNode, dataTree).put((String)regionAndYear[0]+";"+(String)regionAndYear[1], tempNode);
		if(units == null) {
			units = ((Element)tempNode.getParentNode()).getAttribute("unit");
		}
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
			  ret.add(getOneAttrVal(n));
			  /*
			  if(!getOneAttrVal(n).equals("fillout=1")) {
			  	ret.add(getOneAttrVal(n));
			  } else {
			        ret.add(getOneAttrVal(n, 1));
			  }
			  */

		  } else if(n.hasAttributes()) {
			  Map tempFilter;
	           	  if (filterMaps.containsKey(n.getNodeName())) {
	                          tempFilter = (Map)filterMaps.get(n.getNodeName());
                          } else {
                                  tempFilter = new HashMap();
                          }
			  String attr = getOneAttrVal(n);
			  /*
			  if(attr.equals("fillout=1")) {
				  attr = getOneAttrVal(n, 1);
			  }
			  */
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
  private Map addToDataTree(Node currNode, Map dataTree) {
	  if (currNode.getNodeType() == Node.DOCUMENT_NODE) {
		  return dataTree;
	  }
	  Map tempMap = addToDataTree(currNode.getParentNode(), dataTree);
	  // used to combine sectors and subsectors when possible to avoid large amounts of sparse tables
	  if( ((((String)wild.get(0)).matches(".*[Ss]ector") || ((String)wild.get(1)).matches(".*[Ss]ector"))) && currNode.getNodeName().equals(".*[Ss]ector") ) {
		  return tempMap;
	  }
	  if(currNode.hasAttributes() && !currNode.getNodeName().equals((String)wild.get(0)) && !currNode.getNodeName().equals((String)wild.get(1))) {
		String attr = getOneAttrVal(currNode);
		/*
		if(attr.equals("fillout=1")) {
			attr = getOneAttrVal(currNode, 1);
		}
		*/
		attr = currNode.getNodeName()+"@"+attr;
		if(!tempMap.containsKey(attr)) {
			tempMap.put(attr, new TreeMap());
		}
		return (Map)tempMap.get(attr);
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
  private void recAddTables(Map dataTree, Map.Entry parent, Set regions, Set years, String titleStr) {
	Iterator it = dataTree.entrySet().iterator();
	while(it.hasNext()) {
		Map.Entry me = (Map.Entry)it.next();
		if(me.getValue() instanceof Node || me.getValue() instanceof Double) {
			NewDataTableModel tM;
			if(me.getValue() instanceof Double) {
				tM = new NewDataTableModel(regions, qg.getAxis1Name()/*(String)wild.get(0)*/, years, 
						qg.getVariable(), /*titleStr+'/'+(String)parent.getKey()*/title, (Map)parent.getValue(), doc,
						null); 
				tM.setColNameIndex(qg.getChartLabelColumnName());
			} else {
				tM = new NewDataTableModel(regions, (String)wild.get(0), years, 
						(String)wild.get(1), /*titleStr+'/'+(String)parent.getKey()*/title, (Map)parent.getValue(), doc,
						documentation /*, (String)wild.get(2)*/); 
			}
			tM.units = units;
	  		JTable jTable = new JTable(tM);

	  		jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
	 
			jTable.setCellSelectionEnabled(true);

	  		javax.swing.table.TableColumn col;
	  		Iterator i = regions.iterator();
	  		int j = 1;
	  		while(i.hasNext()) {
		  		col = jTable.getColumnModel().getColumn(j);
				col.setPreferredWidth(((String)i.next()).length()*5+30);
				if(qg == null) { // only want to do this when values might have documentation
					col.setCellRenderer(tM.getCellRenderer(0, j));
				}
		  		j++;
	  		}
			CopyPaste copyPaste = new CopyPaste( jTable );
	  		JScrollPane tV = new JScrollPane(jTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER,
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			JScrollPane tableView = tV;
			if(me.getValue() instanceof Double) {
				JPanel tpanel = new JPanel();
				JSplitPane sp = new JSplitPane();

				JLabel labelChart = new JLabel();
				try {
					JFreeChart chart = tM.createChart(0,0);
					Dimension chartDim = tM.getChartDimensions(chart);
					BufferedImage chartImage = chart.createBufferedImage( (int)chartDim.getWidth(), 
							(int)chartDim.getHeight());
					labelChart.setIcon(new ImageIcon(chartImage));
				} catch(Exception e) {
					//e.printStackTrace();
					labelChart.setText("Cannot Create Chart");
				}
				//labelChart.setIcon(tM.getChartImage());

				sp.setLeftComponent(tV);
				sp.setRightComponent(labelChart);
				tableView = new JScrollPane(sp);
				sp.setDividerLocation(parentFrame.getWidth()-350);
			}

	  		if(tables == null) {
		  		tables = new Vector();
	  		}
			String labelStr = titleStr.replace("/", ",   ").replace("@", ": ");
			labelStr = labelStr.substring(2, labelStr.length());
			//tables.add(titleStr+"/");
			tables.add(labelStr);
	  		tables.add(tableView);
			return;
		} else {
			recAddTables((Map)me.getValue(), me, regions, years, titleStr+'/'+(String)me.getKey());
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
		return title; 
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
		StringBuffer regex = new StringBuffer("^/");
		for(int i = possibleFilters.size()-1; i >= 0; i--) {
			Iterator it = ((Map)tableFilterMaps.get(possibleFilters.get(i))).entrySet().iterator();
			if(it.hasNext()) {
				regex.append((String)possibleFilters.get(i)).append("@(");
				while(it.hasNext()) {
					Map.Entry me = (Map.Entry)it.next();
					if(((Boolean)me.getValue()).booleanValue()) {
						regex.append(me.getKey()).append("|");
					}
				}
				/*
				if(regex.endsWith("|")) {
					regex = regex.substring(0,regex.length()-1)+")/";
					*/
				if(regex.charAt(regex.length()-1) == '|') {
					regex.deleteCharAt(regex.length()-1).append(")/");
				} else {
					regex.append(")/");
				}
			} else {
				regex.append((String)possibleFilters.get(i)).append("/");
			}
		}
		regex.append("$");
		String regexStr = regex.toString();
		System.out.println("Regex is: "+regexStr);
		Vector tempActive = new Vector();
		for(int i = 0; i < tables.size(); i+=2) {
			if(((String)tables.get(i)).matches(regexStr)) {
				tempActive.add(new Integer(i));
				tempActive.add(new Integer(i+1));
			}
		}
		activeRows = tempActive;
	}
	public JFreeChart createChart(int rowAt, int colAt) {
		return ((NewDataTableModel)((JTable)((JScrollPane)getValueAt(rowAt, colAt)).getViewport().getView()).getModel()).createChart(rowAt, colAt);
	}

	QueryGenerator qg;
	public MultiTableModel(QueryGenerator qgIn, String queryFilter, Object[] regions, JFrame parentFrameIn) {
		qg = qgIn;
		parentFrame = parentFrameIn;
		//title = qgIn.getVariable();
		title = qgIn.toString();
		wild = new ArrayList();
		wild.add(qgIn.getNodeLevel());
		wild.add(qgIn.getYearLevel());
		System.out.println("Query is "+qgIn.getCompleteXPath(regions));
		System.out.println("Before Function: "+System.currentTimeMillis());
		buildTable(DbViewer.xmlDB.createQuery(qgIn.getCompleteXPath(regions), queryFilter, null), qgIn.isSumAll(), qgIn.getLevelValues());
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		activeRows = new Vector(tables.size());
		for(int i = 0; i < tables.size(); i++) {
			activeRows.add(new Integer(i));
		}
		int numTables = getRowCount() / 2;
		if(numTables > 1) {
			title += " ("+numTables+" Tables)";
		} else {
			title += " (1 Table)";
		}
	}
	private void buildTable(XmlResults res, boolean sumAll, Object[] levelValues) {
		System.out.println("In Function: "+System.currentTimeMillis());
	  try {
		  if(!res.hasNext()) {
			  System.out.println("Query didn't get any results");
			  // display an error on the screen
			  JOptionPane.showMessageDialog(parentFrame, "Query didn't get any results", "Build Table Error",
					  JOptionPane.ERROR_MESSAGE);
			  return;
		  }
	  } catch(XmlException e) {
		  e.printStackTrace();

	  }
	  XmlValue tempNode;
	  Object[] regionAndYear;
	  Set regions = new TreeSet();
	  Set years = new LinkedHashSet();
	  tableFilterMaps = new LinkedHashMap();
	  Map dataTree = new TreeMap();
	  try {
		  while(res.hasNext()) {
			  tempNode = res.next();
			  regionAndYear = qg.extractAxisInfo(tempNode.getParentNode(), tableFilterMaps);
			  String parentNodeName = tempNode.getParentNode().getNodeName();
			  if(units == null) {
				  units = XMLDB.getAttr(tempNode.getParentNode(), "unit");
			  }
			  if(sumAll) {
				  //regionAndYear[1] = "All "+(String)wild.get(0);
				  regionAndYear[1] = levelValues[0];
			  }
			  regions.add(regionAndYear[0]);
			  years.add(regionAndYear[1]);
			  Map retMap = qg.addToDataTree(new XmlValue(tempNode), dataTree); 
			  DbViewer.xmlDB.printLockStats("addToDataTree");
			  Double ret = (Double)retMap.get((String)regionAndYear[0]+";"+(String)regionAndYear[1]);
			  if(ret == null) {
				  retMap.put((String)regionAndYear[0]+";"+(String)regionAndYear[1], new Double(tempNode.asNumber()));
			  } else {
				  retMap.put((String)regionAndYear[0]+";"+(String)regionAndYear[1], 
						  new Double(ret.doubleValue() + tempNode.asNumber()));
			  }
			  ret = (Double)retMap.get((String)regionAndYear[0]+";Total");
			  if(ret == null) {
				  retMap.put((String)regionAndYear[0]+";Total", new Double(tempNode.asNumber()));
			  } else {
				  retMap.put((String)regionAndYear[0]+";Total", 
						  new Double(ret.doubleValue() + tempNode.asNumber()));
			  }
			  tempNode.delete();
		  }
		  res.delete();
		  DbViewer.xmlDB.printLockStats("buildTable");
	  } catch(Exception e) {
		  e.printStackTrace();
	  }
	  years.add("Total");
	  
	  if(remove1975) {
		  regions.remove("1975");
	  }
	  System.out.println("After build Tree: "+System.currentTimeMillis());
	  recAddTables(dataTree, null, regions, years, "");
	  System.out.println("After Add table: "+System.currentTimeMillis());
	  /* Figure out what to do for level selected
	  System.out.println("Level Selected: "+levelValues);
	  if(!sumAll && levelValues != null && years.size() != levelValues.length) {
		  //indRow = new Vector(levelValues);
		  indRow = new Vector(levelValues.length, 0);
		  for(int i =0; i < levelValues.length; ++i) {
			  System.out.println(levelValues[i]);
			  indRow.add(levelValues[i]);
		  }
	  } else {
		  indRow = new Vector( years );
	  //}
	  indCol = new Vector( regions );
	  ind1Name = (String)wild.get(0);
	  //ind2Name = (String)wild.get(1);
	  */
	}

  public void exportToExcel(HSSFSheet sheet, HSSFWorkbook wb, HSSFPatriarch dp) {
	  HSSFRow row = sheet.createRow(sheet.getLastRowNum()+1);
	  row.createCell((short)0).setCellValue(getColumnName(0));
	  for(int rowN = 0; rowN < getRowCount(); rowN +=2) {
		  row = sheet.createRow(sheet.getLastRowNum()+1);
		  row.createCell((short)0).setCellValue(getValueAt(rowN,0).toString());
		  //System.out.println("Table? "+getValueAt(rowN+1,0));
		  //((BaseTableModel)getValueAt(rowN+1,0)).exportToExcel(sheet, wb);
		  ((NewDataTableModel)((JTable)((JScrollPane)((JSplitPane)((JScrollPane)getValueAt(rowN+1, 0)).getViewport().getView())
		   	.getLeftComponent()).getViewport().getView()).getModel()).exportToExcel(sheet, wb, dp);
	  }
  }
	public boolean equals(Object other) {
		if(other == this) {
			return true;
		} else if(!(other instanceof BaseTableModel) || other == null) {
			return false;
		} else if(other instanceof NewDataTableModel) {
			for(int i = 1; i < getRowCount(); i += 2) {
				//System.out.println("checking other against: "+((JScrollPane)getValueAt(i, 0)).getViewport().getView());
				//if(((JTable)((JScrollPane)getValueAt(i, 0)).getViewport().getView()).getModel() == other) {
				if(getModelAt(i) == other) {
					return true;
				}
			}
		}
		return false;
	}

	private BaseTableModel getModelAt(int row) {
		Object ret = ((JScrollPane)getValueAt(row, 0)).getViewport().getView();
		if(ret instanceof JSplitPane) {
			ret = ((JScrollPane)((JSplitPane)ret).getLeftComponent()).getViewport().getView();
		}
		return (BaseTableModel)((JTable)ret).getModel();
	}


	protected Node getNodeAt(int row, int col) {
		// this table model doesn't acctually hold any nodes
		return null;
	}

  public void annotate(int[] rows, int[] cols, Documentation documentation) {
	  JTable jTable = (JTable)((JScrollPane)getValueAt(rows[0], cols[0])).getViewport().getView();
	  ((BaseTableModel)jTable.getModel()).annotate(jTable.getSelectedRows(), jTable.getSelectedColumns(), documentation);
  }
}
