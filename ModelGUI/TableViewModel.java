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
import org.jdom.Element;

public class TableViewModel extends javax.swing.table.AbstractTableModel {
    private Element[][] tableValues;
    //private Vector tableValues;
    private Vector tableHeader;
    private Vector tableLefter;
    private int numRows;
    private int numCols;
    
    /** Creates a new instance of TableViewModel */
    public TableViewModel(Vector values, Vector header) {
        tableHeader = (Vector)header.clone();
        tableLefter = new Vector();
        
        numCols = tableHeader.size();
        numRows = (int)Math.ceil(values.size() / numCols);

       /* Vector tableValues = new Vector();
        Vector row = new Vector();
        int ct = 0;
        for (int j = 0; j < numRows; j++) {
           // row = new Element[numCols];
            row.clear();
            for (int k = 0; k < numCols; k++) {
                row.addElement(values.elementAt(ct++));
                //row[k] = (Element)values.elementAt(ct++);
            }
            tableValues.add(row);
        }*/
        
        tableValues = new Element[numRows][numCols];
        int ct = 0;
        for (int row = 0; row < numRows; row++) {
            for (int col = 0; col < numCols; col++) {
                tableValues[row][col] = (Element)values.elementAt(ct++);
            }
        }
    }
    
    public TableViewModel(Vector values, Vector header, Vector lefter) {
        tableHeader = (Vector)header.clone();
        tableLefter = (Vector)lefter.clone();
        
        numCols = tableHeader.size();
        numRows = (int)Math.ceil(tableHeader.size() / numCols);
        
        /*Vector tableValues = new Vector();
        Vector row = new Vector();
        int ct = 0;
        for (int j = 0; j < numRows; j++) {
            //row = new Element[numCols];
            row.clear();
            for (int k = 0; k < numCols; k++) {
                row.addElement(values.elementAt(ct++));
                //row[k] = (Element)values.elementAt(ct++);
            }
            tableValues.add(row);
        }*/
        tableValues = new Element[numRows][numCols];
        int ct = 0;
        for (int row = 0; row < numRows; row++) {
            for (int col = 0; col < numCols; col++) {
                tableValues[row][col] = (Element)values.elementAt(ct++);
            }
        }
    }
    
    public int getColumnCount() {
        return numCols;
    }
    
    public int getRowCount() {
        return numRows;
    }
    
    /*public Vector getTableValues() {
        return tableValues;
    }*/
    
    public Vector getTableHeader() {
        return tableHeader;
    }
    
    public Vector getTableLefter() {
        return tableLefter;
    }
    
    public Object getValueAt(int row, int col) {
        return tableValues[row][col].getText();
        //return ((Vector)tableValues.elementAt(row)).elementAt(col);
    }
    
    public String getColumnName(int col) { 
        return tableHeader.elementAt(col).toString(); 
    }
    
    public void addBlankRow(int newRowIndex) {
        Element[][] newValues = new Element[numRows+1][numCols];
        
        //copy the begining of the array
        for (int j = 0; j < newRowIndex; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j][k] = tableValues[j][k];
            }
        }
        
        //insert the blank row
        for (int k = 0; k < numCols; k++) {
            newValues[newRowIndex][k] = new Element("blank");
        }
        
        //copy the rest of the array
        for (int j = newRowIndex; j < numRows; j++) {
            for (int k = 0; k < numCols; k++) {
                newValues[j+1][k] = tableValues[j][k];
            }
        }
        tableValues = newValues; 
    }
    

    
}
