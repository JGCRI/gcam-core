import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.*;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JScrollPane;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.TreePath;
import java.awt.event.*;
import org.apache.xpath.domapi.*;
import org.w3c.dom.xpath.*;

public class MultiTableModel extends BaseTableModel{
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

	public MultiTableModel(TreePath tp, Document doc, JFrame parentFrame) {
		super(tp, doc, parentFrame);
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
			  if (!tempFilter.containsKey(attr)) {
                          	tempFilter.put(attr, new Boolean(true));
                          	filterMaps.put(n.getNodeName(), tempFilter);
			  }
		  }
		  n = n.getParentNode();
	  } while(n.getNodeType() != Node.DOCUMENT_NODE /*&& (region == null || year == null)*/);
	  return ret.toArray();
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
		return (TreeMap)tempMap.get(attr);
	  }
	  return tempMap;
  }

  private void recAddTables(TreeMap dataTree, Map.Entry parent, TreeSet regions, TreeSet years, String title) {
	Iterator it = dataTree.entrySet().iterator();
	while(it.hasNext()) {
		Map.Entry me = (Map.Entry)it.next();
		if(me.getValue() instanceof Node) {
	  		NewDataTableModel tM = new NewDataTableModel(regions, (String)wild.get(0), years, (String)wild.get(1), title+'/'+(String)parent.getKey(), (TreeMap)parent.getValue(), doc); 
	  		JTable jTable = new JTable(tM);
			//HERE
	  		//jTable.getModel().addTableModelListener(this);

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
	public TableCellEditor getCellEditor(int row, int col ) {
			return tableEditor;
	}
	public TableCellRenderer getCellRenderer(int row, int col ) {
			return tableRenderer;
	}
	public int getColumnCount() {
		return 1;
	}
	public int getRowCount() {
		return activeRows.size();
	}
	public Object getValueAt(int row, int col) {
		return tables.get(((Integer)activeRows.get(row)).intValue());
	}
	public String getColumnName(int col) {
		return "Stuff";
	}
	public boolean isCellEditable(int row, int col) {
		if(row % 2 == 0) {
			return false;
		} else {
			return true;
		}
	}

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
