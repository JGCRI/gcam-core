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
	//Vector activeRows;
	TableRenderer tableRenderer;
	TableEditor tableEditor;
	//Map tableFilterMaps;
	/*
	public MultiTableModel (Vector tablesIn, Map filters) {
		tableFilterMaps = filters;
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		tables = tablesIn;
		activeRows= new Vector(tables.size());
		for(int i = 0; i < tables.size(); i++) {
			activeRows.add(new Integer(i));
		}

	}
	*/
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
			//tables.add(title+"/"+(String)me.getKey());
			tables.add(title+"/");
	  		tables.add(tableView);
			return;
		} else {
			recAddTables((TreeMap)me.getValue(), me, regions, years, title+'/'+(String)me.getKey());
		}
	}
  }
	public TableCellEditor getCellEditor(int row, int col ) {
		/*
		if(row % 2 == 0) {
			return new javax.swing.DefaultCellEditor();
		} else {
		*/
			return tableEditor;
		//}
	}
	public TableCellRenderer getCellRenderer(int row, int col ) {
		/*
		if(row % 2 == 0) {
			return new DefaultTableCellRenderer();
		} else {
		*/
			return tableRenderer;
		//}
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
	/*
	private int currFilter;
	private String[] currKeys;
	public void filterData(JFrame parentFrame) {
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
		//was close comment
		currKeys = new String[0];
		final Map tempFilterMaps = (Map)((LinkedHashMap)tableFilterMaps).clone();
		/*
		final Vector possibleKeys = new Vector();
		final boolean tempIsCondensed = isCondensed;
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
// was close comment
		//possibleKeys = new Vector(filters.keySet());
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
	        cancelButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (!cancelButton.getText().equals(cancelTitle)) {
					updateFilters(tempFilterMaps, list, (String)possibleKeys.get(currFilter));
					tableFilterMaps = tempFilterMaps;
					doFilter(possibleKeys);
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
	private void updateFilters(Map tempFilterMaps, JList list, String key) {
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
	private void updateList(JList list, JLabel listLabel, String key, Map tempFilterMaps) {
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
				regex = regex.substring(0,regex.length()-1)+")/";
			} else {
				regex += (String)possibleFilters.get(i)+"/";
			}
		}
		regex += "$";
		//System.out.println("Regex: "+regex);
		Vector tempActive = new Vector();
		for(int i = 0; i < tables.size(); i+=2) {
			if(((String)tables.get(i)).matches(regex)) {
				//System.out.println("match: "+tables.get(i));
				tempActive.add(new Integer(i));
				tempActive.add(new Integer(i+1));
			} /*else {
				System.out.println("didn't match: "+tables.get(i));
			}*/
		}
		activeRows = tempActive;
	}
}
