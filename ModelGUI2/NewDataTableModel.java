import javax.swing.table.AbstractTableModel;
import java.util.*;
import org.w3c.dom.Node;
import org.w3c.dom.*;
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
	Document doc;

	public NewDataTableModel(Collection set1, String set1Name, Collection set2, String set2Name, String w3In, TreeMap dataIn, Document docIn) {
		w3 = w3In;
		indCol = new Vector(set1);
		indCol.add(0,w3 /*set2Name*/);
		indRow = new Vector(set2);
		data = dataIn;
		flipped = false;
		doc = docIn;
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
		//if( n != null ){
			n.setNodeValue(val.toString());
		/*}else{
			n = doc.createElement( val.toString() );
			//n.setNodeValue(val.toString());
			Node updown = null;
			Node side = null;
			if( row > 0 ){
				updown = (Node)data.get(getKey(row-1, col));
				if ( col > 0 ){
					side = (Node)data.get(getKey(row, col-1));
				}else{ // col == 0
					side = (Node)data.get(getKey(row, col+1));
				}
			}else{ // row == 0
				updown = (Node)data.get(getKey(row+1, col));
				if ( col > 0 ){
					side = (Node)data.get(getKey(row, col-1));
				}else{ // col == 0
					side = (Node)data.get(getKey(row, col+1));
				}
			}
			ArrayList nodepath = new ArrayList();
			Node parent = updown.getParentNode();
			int index = 0;
			boolean splitonside = false;
			boolean stoplooking = false;
			while( parent != null ){
				nodepath.add( parent );
				if ( !stoplooking && (parent.getNodeName() == side.getNodeName() || parent.getNodeName() == updown.getNodeName())){
					index = nodepath.indexOf( parent );
					if ( parent.getNodeName() == side.getNodeName() ){
						splitonside = true;
					}else{
						splitonside = false;
					}
					System.out.println("inedex is .. " + index);
					stoplooking = true;
				}
				parent = parent.getParentNode();
			}
			if( splitonside ){
				Node curr = (Node)nodepath.get( index );
				if ( index == nodepath.size()-1 ){
						
				}else{
					Nodelist children = curr.getChildNodes();
					
				}
				
			}else{ // split on updown
				
			}
			
		}
		// fireOffSomeListeners?
*/
		}
}
