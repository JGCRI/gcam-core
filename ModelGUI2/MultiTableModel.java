import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.Node;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JScrollPane;
import javax.swing.*;
import java.awt.*;
//import javax.swing.tree.TreePath;
import java.awt.event.*;

public class MultiTableModel extends AbstractTableModel {
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
	Vector activeRows;
	TableRenderer tableRenderer;
	TableEditor tableEditor;
	HashMap tableFilterMaps;
	public MultiTableModel (Vector tablesIn, HashMap filters) {
		tableFilterMaps = filters;
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		tables = tablesIn;
		activeRows= new Vector(tables.size());
		for(int i = 0; i < tables.size(); i++) {
			activeRows.add(new Integer(i));
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
			*/
			currKeys = new String[0];
			final HashMap tempFilterMaps = (HashMap)tableFilterMaps.clone();
			/*
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
			}*/
			//possibleKeys = new Vector(filters.keySet());
			final Vector possibleKeys = new Vector(tempFilterMaps.keySet());
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
	private void doFilter(Vector possibleFilters) {
	}
}
