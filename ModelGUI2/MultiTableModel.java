import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.Node;
import javax.swing.table.*;
import javax.swing.JTable;
import java.awt.Component;
import javax.swing.JScrollPane;
//import javax.swing.*;
//import java.awt.*;
//import javax.swing.tree.TreePath;
//import java.awt.event.*;

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
	TableRenderer tableRenderer;
	TableEditor tableEditor;
	public MultiTableModel (Vector tablesIn) {
		tableEditor = new TableEditor();
		tableRenderer = new TableRenderer();
		tables = tablesIn;
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
		return tables.size();
	}
	public Object getValueAt(int row, int col) {
		return tables.get(row);
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
}
