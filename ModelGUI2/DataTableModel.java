import javax.swing.table.AbstractTableModel;
import java.util.*;
import javax.swing.*;
import java.awt.*;
import javax.swing.tree.TreePath;
import org.w3c.dom.*;
import org.apache.xpath.domapi.*;
import org.w3c.dom.xpath.*;
/*
import javax.swing.JDialog;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.BoxLayout;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import java.awt.Container;
*/
import java.awt.event.*;

public class DataTableModel extends BaseTableModel{
	private Vector cols;
	private Vector rows;
	//private Vector activeRows;
	//private HashMap tableFilterMaps;
	private short tableType;
	
	public static final short NORMAL_TABLE = 0;
	public static final short DEMAND_COMPONENTS_TABLE = 1;
	public static final short SAM = 2;

	public DataTableModel(TreePath tp, Document doc, JFrame parentFrame) {
		super(tp, doc, parentFrame);
		cols = new Vector();
		rows = new Vector();
		activeRows = new Vector();
		buildTable(treePathtoXPath(tp, doc.getDocumentElement(), 0));
		tableType = NORMAL_TABLE;
		for (int i = 0; i < rows.size(); i++) {
			activeRows.addElement(new Integer(i));
		}
	}
  	protected void buildTable(XPathExpression xpe){
	  XPathResult res = (XPathResult)xpe.evaluate(doc.getDocumentElement(), XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
	  xpe = null;
	  Node tempNode;
	  //Vector cols = new Vector();
	  Vector tempVector;
	  //Vector rows = new Vector();
	  //HashMap filterMaps; 
	  HashMap tempMap;
	  Vector path;
	  if(tableFilterMaps == null) {
		  tableFilterMaps = new HashMap();
	  }
	  /*
	  if (tableModel == null) {
		  filterMaps = new HashMap();
	  } else {
		  filterMaps = tableModel.getFilterMaps();
	  }
	  */
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
	  /*
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
	  */
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
		  //path.addElement(((DOMmodel)jtree.getModel()).getAdapterNode(currNode));
		  // HERE for now
		  path.addElement(currNode);
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
	/*
	public DataTableModel(Vector colHeaders, Vector rowData, HashMap filterMaps, short type, JFrame parentFrame) {
		cols = colHeaders;
		rows = rowData;
		tableFilterMaps = filterMaps;
		/*
		activeRows = new Vector();
		for (int i = 0; i < rows.size(); i++) {
			activeRows.addElement(new Integer(i));
		}
		// used to be end comment
		filterData(parentFrame, true);
		tableType = type;
	}
	*/

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
				//System.out.println("in is "+tp.toString());
				//System.out.println("row is "+((Vector)rows.get(i)).get(col).toString());
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

	/*
	private int currFilter;
	private String[] currKeys;
	public void filterData(JFrame parentFrame, boolean isCondensed) {
		// so i can make oldNumRows final and it won't crash
		if (activeRows == null) {
			activeRows = new Vector();
		}
		final int oldNumRows = activeRows.size();
		/*
		final Vector oldActiveRows = activeRows;
		activeRows = new Vector();
		for (int i = 0; i < rows.size(); i++) {
			activeRows.addElement(new Integer(i));
		}
		// used to be close comment
		currKeys = new String[0];
		final HashMap tempFilterMaps = (HashMap)tableFilterMaps.clone();
		final Vector possibleKeys = new Vector();
		final boolean tempIsCondensed = isCondensed;
		currFilter = 0;
		String title;
		if (isCondensed) {
			for (int i = 0; i < cols.size(); i++) {
				if (((String)cols.get(i)).matches("^region.*$")) {
					possibleKeys.add(cols.get(i));
				} else if (((String)cols.get(i)).matches("^.*year$")) {
					possibleKeys.add(cols.get(i));
				}
			}
			title = "Filter Results";
		} else {
			title = "Filter Table";
			if(tableType == NORMAL_TABLE) {
				possibleKeys.addAll(cols.subList(0,cols.size()-1));
			} else if (tableType == DEMAND_COMPONENTS_TABLE ) {
				possibleKeys.addAll(cols.subList(0,cols.size()-6));
			}
		}
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
	        cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!cancelButton.getText().equals(cancelTitle)) {
					updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
					tableFilterMaps = tempFilterMaps;
					doFilter(possibleKeys, tempIsCondensed);
					if (oldNumRows < activeRows.size()) {
						System.out.println("%% 1 %%");
						fireTableRowsInserted(oldNumRows, activeRows.size());
					} else if (oldNumRows > activeRows.size()) {
						System.out.println("%% 2 %%");
						fireTableRowsDeleted(0, activeRows.size());
					} else {
						System.out.println("%% 3 %%");
						fireTableRowsUpdated(0,activeRows.size());
					}
				}
				//exit this dialog..
				filterDialog.dispose();
				//filterDialog.hide();
			}
		});

	        backButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				currFilter--;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!nextButton.isEnabled()) {
					nextButton.setEnabled(true);
					cancelButton.setText(cancelTitle);
				}
				if (currFilter == 0) {
					backButton.setEnabled(false);
				}
			}
		});

	        nextButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
				currFilter++;
				updateList(list, listLabel, (String)possibleKeys.get(currFilter), tempFilterMaps);
				if (!backButton.isEnabled()) {
					backButton.setEnabled(true);
				}
				if (currFilter == possibleKeys.size()-1) {
					nextButton.setEnabled(false);
					cancelButton.setText("Finished");
				}
			}
		});

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
		filterDialog.show();
	}

	private void updateFilters(HashMap tempFilterMaps, JList list, String key) {
		int[] selectedKeys = list.getSelectedIndices();
		int j = 0;
		for (int i = 0; i < currKeys.length; i++) {
			// clean this up... maybe
			if (((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j >= selectedKeys.length || i != selectedKeys[j])) {
				//System.out.println("Changing Key: "+currKeys[i]+"'s value to false pos is "+i+" and selected key pos "+selectedKeys[j]);
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(false));
			} else if (!((Boolean)((HashMap)tempFilterMaps.get(key)).get(currKeys[i])).booleanValue() && (j < selectedKeys.length && i == selectedKeys[j])) {
				//System.out.println("Changing Key: "+currKeys[i]+"'s value to true");
				((HashMap)tempFilterMaps.get(key)).put(currKeys[i], new Boolean(true));
			}
			if (j < selectedKeys.length && i == selectedKeys[j]) {
				j++;
				//System.out.println("j is now "+j);
			}
		}
	}

	private void updateList(JList list, JLabel listLabel, String key, HashMap tempFilterMaps) {
		HashMap tempMap = (HashMap)tempFilterMaps.get(key);
		Vector tempVector = new Vector();
		listLabel.setText("Filter "+key);
		currKeys = (String[])tempMap.keySet().toArray(new String[0]);
		list.setListData(currKeys);
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
	*/

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
}

