/*
 * TableViewModel.java
 *
 * Created on June 13, 2003, 11:08 AM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import java.util.Vector;
//import org.jdom.Element;

public class TableViewModel extends javax.swing.table.AbstractTableModel {
    private AdapterNode[][] tableValues;
    //private Vector tableValues;
    private Vector tableHeader;
    private Vector tableLefter;
    private int numRows;
    private int numCols;
    
    /** Creates a new instance of TableViewModel */
    public TableViewModel(Vector values, Vector lefter, boolean names) {
        tableHeader = new Vector();
        tableLefter = (Vector)lefter.clone(); 
        
        numRows = tableLefter.size();
        numCols = (int)Math.ceil(values.size() / numRows);
        
        tableValues = new AdapterNode[numRows][numCols];
        int ct = 0;
        if (names) {
            for (int row = 0; row < numRows; row++) {
                for (int col = 0; col < numCols; col++) {
                    AdapterNode newNode = new AdapterNode();
                    newNode.setText((String)values.elementAt(ct++));
                    tableValues[row][col] = newNode;
                }
            }
        } else {
            for (int row = 0; row < numRows; row++) {
                for (int col = 0; col < numCols; col++) {
                    tableValues[row][col] = (AdapterNode)values.elementAt(ct++);
//System.out.println("added " + tableValues[row][col] + ", " + tableValues[row][col].getText());
                }
            }
        }
    }
    
    public TableViewModel(Vector values, Vector header, Vector lefter, boolean names) {
        tableLefter = (Vector)lefter.clone();
        
        numRows = tableLefter.size();
        numCols = (int)Math.ceil(values.size() / numRows);
        
//System.out.println("haeder size = " + header.size());
        
        if (header != null && !header.isEmpty()) {
//System.out.println("adding things to header");
            tableHeader = (Vector)header.clone();
        } else {
            tableHeader = new Vector();
            for (int j = 0; j < numCols; j++) {
                tableHeader.addElement(" ");
            }
        }
       
        tableValues = new AdapterNode[numRows][numCols];
        int ct = 0;
        if (names) {
            for (int row = 0; row < numRows; row++) {
                for (int col = 0; col < numCols; col++) {
                    AdapterNode newNode = new AdapterNode();
                    newNode.setText((String)values.elementAt(ct++));
                    tableValues[row][col] = newNode;
                }
            }
        } else {
            for (int row = 0; row < numRows; row++) {
                for (int col = 0; col < numCols; col++) {
                    tableValues[row][col] = (AdapterNode)values.elementAt(ct++);
//System.out.println("added " + tableValues[row][col] + ", " + tableValues[row][col].getText());
                }
            }
        }
    }
    
    public boolean isCellEditable(int row, int col) {
        return true;
    }
    
    public int getColumnCount() {
        return numCols;
    }
    
    public AdapterNode getNodeAt(int row, int col) {
        return tableValues[row][col];
    }
    
    public int getRowCount() {
        return numRows;
    }
    
    public Vector getTableHeader() {
        return tableHeader;
    }
    
    public Vector getTableLefter() {
        return tableLefter;
    }
    
    public Object getValueAt(int row, int col) {
        return tableValues[row][col].getText();
    }
    
    public String getColumnName(int col) {
        return tableHeader.elementAt(col).toString();
    }
    
    public void addBlankRow(int newRowIndex) {
        AdapterNode[][] newValues = new AdapterNode[numRows+1][numCols];
        
        //copy the begining of the array
        for (int j = 0; j < newRowIndex; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j][k] = tableValues[j][k];
            }
        }
        
        //insert the blank row
        for (int k = 0; k < numCols; k++) {
            newValues[newRowIndex][k] = new AdapterNode();
        }
        
        //copy the rest of the array
        for (int j = newRowIndex; j < numRows; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j+1][k] = tableValues[j][k];
            }
        }
        tableValues = newValues;
    }
    
    public void setValueAt(Object newValue, int row, int col) {
        tableValues[row][col].setText(newValue.toString());
        fireTableCellUpdated(row, col);
    }
    
}
