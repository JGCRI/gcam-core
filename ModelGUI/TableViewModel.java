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
                }
            }
        }
        
        insertRegionBlanks();
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
//System.out.println("newRowIndex = " + newRowIndex + ", numRows = " + numRows + ", actual num rows = " + newValues.length);
        
        //copy the begining of the array
        for (int j = 0; j < newRowIndex; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j][k] = tableValues[j][k];
//System.out.println("added " + newValues[j][k].getText() + " at " + j + ", " + k);
            }
        }
        
        //insert the blank row
        for (int k = 0; k < numCols; k++) {
            newValues[newRowIndex][k] = new AdapterNode();
//System.out.println("added " + newValues[newRowIndex][k].getText() + " at " + newRowIndex + ",     " + k);
        }
        
        //copy the rest of the array
        for (int j = newRowIndex; j < numRows; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j+1][k] = tableValues[j][k];
//System.out.println("added " + newValues[j+1][k].getText() + " at " + j + ", " + k);
            }
        }
        tableValues = newValues;
        numRows = numRows+1;
    }
    
    public void insertRegionBlanks() {
        if (tableLefter == null || tableLefter.isEmpty()) return;
        
        Vector newLefter = new Vector();
        String currRegion, prevRegion = "";
        int index;
        int numBlanks = 0;
        for (int j = 0; j < tableLefter.size(); j++) {
            //get the name of current region
            currRegion = tableLefter.elementAt(j).toString();
            index = currRegion.indexOf('<');
            currRegion = currRegion.substring(0, index);
            
            if (!currRegion.equals(prevRegion)) {
                newLefter.addElement(currRegion);
                addBlankRow(j + numBlanks++);
            }
            
            //save the lefter without region name
            newLefter.addElement(tableLefter.elementAt(j).toString().substring(index));
            
            prevRegion = currRegion;
        }
        //save the new lefter
        tableLefter = (Vector)newLefter.clone();
    }
    
    public void setValueAt(Object newValue, int row, int col) {
        tableValues[row][col].setText(newValue.toString());
        fireTableCellUpdated(row, col);
    }
    
}
