import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.Node;
//import javax.swing.*;
//import java.awt.*;
//import javax.swing.tree.TreePath;
//import java.awt.event.*;

public class NewDataTableModel extends AbstractTableModel {
	Vector indCol;
	Vector indRow;
	String ind1Name;
	String ind2Name;
	TreeMap data;
	boolean flipped;
	String w3;

	public NewDataTableModel(Collection set1, String set1Name, Collection set2, String set2Name, String w3In, TreeMap dataIn) {
		w3 = w3In;
		indCol = new Vector(set1);
		indCol.add(0,w3 /*set2Name*/);
		indRow = new Vector(set2);
		data = dataIn;
		flipped = false;
	}

	public void flip() {
		Vector tempArr = indCol;
		indCol = indRow;
		indRow = tempArr;
		indRow.remove(0);
		String tempStr = ind1Name;
		ind1Name = ind2Name;
		ind2Name= tempStr;
		indCol.add(0, w3 /*ind2Name*/);
		flipped = !flipped;
		fireTableStructureChanged();
	}

	public int getColumnCount() {
		return indCol.size();
	}

	public int getRowCount() {
		return indRow.size();
	}

	public Object getValueAt(int row, int col) {
		if(col ==0) {
			return indRow.get(row);
		}
		Node ret = ((Node)data.get(getKey(row,col)));
		if(ret == null) {
			return "";
		}
		return ret.getNodeValue();
	}

	public String getColumnName(int column) {
		return (String)indCol.get(column);
	}

	public boolean isCellEditable(int row, int col) {
		return col > 1;
	}

	private String getKey (int row, int col) {
		if(flipped) {
			return (String)indRow.get(row)+(String)indCol.get(col);
		}
		return (String)indCol.get(col)+(String)indRow.get(row);
	}

	public void setValueAt(Object val, int row, int col) {
		Node n = (Node)data.get(getKey(row,col));
		n.setNodeValue(val.toString());
		// fireOffSomeListeners?
	}
}
