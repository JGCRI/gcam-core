//import javax.swing.table.AbstractTableModel;
//package ModelGUI2;
package ModelInterface.ModelGUI2;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.util.*;
import javax.swing.*;
import javax.swing.tree.TreePath;

import org.jfree.chart.JFreeChart;
import org.w3c.dom.*;
import org.w3c.dom.xpath.*;

/**
 * Not used anymore, was the first table, but was decided that it wasn't useful. The Code hasn't been
 * updated in a while so it will need some work to get it working again with the changes to BaseTableModel
 */
public class DataTableModel extends BaseTableModel{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private Vector cols;
	private Vector rows;
	private short tableType;
	
	public static final short NORMAL_TABLE = 0;
	public static final short DEMAND_COMPONENTS_TABLE = 1;
	public static final short SAM = 2;

	public DataTableModel(TreePath tp, Document doc, JFrame parentFrame) {
		super(tp, doc, parentFrame, "Data Table");
		cols = new Vector();
		rows = new Vector();
		activeRows = new Vector();
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 0));
		tableType = NORMAL_TABLE;
		for (int i = 0; i < rows.size(); i++) {
			activeRows.addElement(new Integer(i));
		}
	}
	public void flip(int row, int col) {
		throw new UnsupportedOperationException();
	}
  	protected void buildTable(XPathExpression xpe){
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  Vector tempVector;
	  HashMap tempMap;
	  Vector path;
	  if(tableFilterMaps == null) {
		  tableFilterMaps = new HashMap();
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
	           	  if (tableFilterMaps.containsKey(cols.get(i))) {
	                          tempMap = (HashMap)tableFilterMaps.get(cols.get(i));
                          } else {
                                  tempMap = new HashMap();
                          }
			  if (!tempMap.containsKey(tempVector.get(i))) {
                          	tempMap.put(tempVector.get(i), new Boolean(true));
                          	tableFilterMaps.put(cols.get(i), tempMap);
			  }
          	  }
		  rows.addElement(tempVector);
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
		  path.addElement(currNode);
		  if (currNode.hasAttributes()) {
			  currList.addElement(getOneAttrVal(currNode).substring(getOneAttrVal(currNode).indexOf('=')+1));
		  }
		  if (!getTextData(currNode).equals("")) {
		  	  currList.addElement(getTextData(currNode));
		  }
		  else {
		  	
		  }
	  }
	  return currList;
  }

	public int getRowCount() {
		return activeRows.size();
	}

	public int getColumnCount() {
		return cols.size();
	}

	public boolean isCellEditable(int row, int col) {
		if(col == cols.size()-1 && tableType == NORMAL_TABLE){
			return true;
		}else{
			return false;
		}
	}

	public void setValueAt(Object value, int row, int col) {
		if (value instanceof Double) {
			value = ((Double)value).toString();
		}
		if(value.toString().equals(getValueAt(row, col).toString())) {
			System.out.println("Didn't Change");
			return;
		}
		((Vector)rows.get(((Integer)activeRows.get(row)).intValue())).set(col, value);
		System.out.println("Trying to set a value");
		fireTableCellUpdated(row, col);
	}

	public void setValueAt(Object value, TreePath tp) {
		if( tableType == NORMAL_TABLE ) {
			int col = getColumnCount();
			for(int i = 0; i < rows.size(); i++) {
				if(((Vector)rows.get(i)).get(col).toString().equals(tp.toString())) {
					if(value.toString().equals(getValueAt(i, col-1).toString())) {
						System.out.println("Didn't Change tp");
						return;
					}
					((Vector)rows.get(i)).set(col-1, value);
					System.out.println("Trying to set a value tp");
					return;
				}
			}
		}
	}

	public Object getValueAt(int row, int column) {
		Object ret = ((Vector)rows.get(((Integer)activeRows.get(row)).intValue())).get(column);
		if(column == getColumnCount()) {
			return ret;
		}
		if(getColumnClass(column) == Double.class) {
			return new Double((String)ret);
		}
		return ret;
	}

	public String getColumnName(int column) {
		return (String)cols.get(column);
	}

	public static Class checkClass(Object obj) {
		try {
			new Double(obj.toString());
		} catch (NumberFormatException e) {
			return String.class;
		}
		return Double.class;
	}

	public Class getColumnClass(int column) {
		if (((String)cols.get(column)).matches("^.*year$")) {
			return String.class;
		}
		return checkClass((((Vector)rows.get(0)).get(column)));
	}

	protected void doFilter(Vector possibleFilters) {
		boolean isCondensed = false;
		activeRows = new Vector();
		for (int i = 0; i < rows.size(); i++) {
			activeRows.addElement(new Integer(i));
		}
		Integer rowPos = new Integer(-1);
		if (isCondensed) {
			Vector tempVector = new Vector();
			for (int i = 0; i < cols.size(); i++) {
				if (((String)cols.get(i)).matches("^region.*$")) {
					tempVector.add(cols.get(i));
				} else if (((String)cols.get(i)).matches("^.*year$")) {
					tempVector.add(cols.get(i));
				} else {
					tempVector.add("");
				}
			}
			possibleFilters = tempVector;
		}
		for (int i = 0; i < possibleFilters.size(); i++) {
			if (((String)possibleFilters.get(i)).equals("")) {
				continue;
			}
			currKeys = (String[])((HashMap)tableFilterMaps.get((String)possibleFilters.get(i))).keySet().toArray(new String[0]);
			//for (Iterator it = activeRows.iterator(); it.hasNext(); rowPos = (Integer)it.next()) {
			Iterator it = activeRows.iterator();
			while (it.hasNext()) {
				rowPos = (Integer)it.next();
				for (int j = 0; j < currKeys.length; j++) {
					//System.out.println("At row: "+rowPos.intValue()+" with key: "+currKeys[j]);
					if (!((Boolean)((HashMap)tableFilterMaps.get((String)possibleFilters.get(i))).get(currKeys[j])).booleanValue() && 
					    ((String)((Vector)rows.get(rowPos.intValue())).get(i)).equals(currKeys[j])) {
						//System.out.println("Going to Remove "+rowPos.intValue());
						it.remove();
						break;
					}
				}
			}
		}
	}

	public Vector getCols() {
		return cols;
	}

	public Vector getRows() {
		return rows;
	}

	public HashMap getFilterMaps() {
		return (HashMap)tableFilterMaps;
	}

	public void updateDataFilter(JFrame parentFrame) {
		filterData(parentFrame);
	}
	public JFreeChart createChart(){
		throw new UnsupportedOperationException();
	}
}

