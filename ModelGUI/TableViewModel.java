/*
 * TableViewModel.java
 *
 * Created on June 13, 2003, 11:08 AM
 */

package ModelGUI;

import java.util.Vector;

/** This class is necessary to display contents of AdapterNodes in a table format.
 * Along with {@link TableTransferHandler}, handles user interaction with JTable.
 *
 * In the ModelGUI package, TableViewModel is created inside ControlPanel.
 *
 * @author Yulia Eyman (yulia@umd.edu)
 */
public class TableViewModel extends javax.swing.table.AbstractTableModel {
    private AdapterNode[][] tableValues;
    private Vector tableHeader;
    private Vector tableLefter;
    private int numRows;
    private int numCols;
    
    /** Creates a new instance of TableViewModel.
     *
     * @param values Vector of either Strings or AdapterNodes (determined by 
     *      <code> names</code>) that contain information to be displayed
     *      in table cells
     * @param lefter Vector of Strings that contain row headings for the 
     *      table. Currently, these Strings are created using 
     *       <code> AdapterNode.toLefterString() </code>
     * @param names boolean specifying whether <code> values </code> contains
     *      AdapterNode objects (if <code> names </code> = false) or text
     */
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
    
     /** Creates a new instance of TableViewModel.
     *
     * @param values Vector of either Strings or AdapterNodes (determined by 
     *      <code> names</code>) that contain information to be displayed
     *      in table cells
     * @param header Vector of Strings that contains column headings for the
     *      table. Currently, not every table will have a header.
     * @param lefter Vector of Strings that contain row headings for the 
     *      table. Currently, these Strings are created using 
     *       <code> AdapterNode.toLefterString() </code>
     * @param names boolean specifying whether <code> values </code> contains
     *      AdapterNode objects (if <code> names </code> = false) or text
     */     
    public TableViewModel(Vector values, Vector header, Vector lefter, boolean names) {
        tableLefter = (Vector)lefter.clone();
        
        numRows = tableLefter.size();
        numCols = (int)Math.ceil(values.size() / numRows);
                
        if (header != null && !header.isEmpty()) {
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
    
    /** Returns whether the contents of a cell can be manipulated by the user.
     *
     * @param row index of target row, starting at 0
     * @param col index of target column, starting at 0
     * @return true if cell content can be modified */    
    public boolean isCellEditable(int row, int col) {
        return true;
    }
    
    /** Returns the total number of columns in table.
     *
     * @return total colunms */    
    public int getColumnCount() {
        return numCols;
    }
    
    /** Retrieves the value in a specified location in the table.
     *
     * @param row target node's row
     * @param col target node's column 
     * @return AdapterNode in desired position */    
    public AdapterNode getNodeAt(int row, int col) {
        return tableValues[row][col];
    }
    
    /** Returns the total number of rows in table.
     *
     * @return total rows */    
    public int getRowCount() {
        return numRows;
    }
    
    /** Retrieves the column header of table.
     *
     * @return Vector containing String names of columns or empty Vector */    
    public Vector getTableHeader() {
        return tableHeader;
    }
    
    /** Retrieves the row header of table.
     *
     * @return Vector containing String names of rows or empty Vector */    
    public Vector getTableLefter() {
        return tableLefter;
    }
    
    /** Retrieves the value stored in the specified table cell.
     *
     * @param row target cell's row
     * @param col target cell's column
     * @return String reperesentation of cell value */    
    public Object getValueAt(int row, int col) {
        return tableValues[row][col].getText();
    }
    
    /** Retreives the header of a specific column.
     *
     * @param col target column
     * @return column name */    
    public String getColumnName(int col) {
        return tableHeader.elementAt(col).toString();
    }
    
    /** Inserts a row of blank cells into the table.
     *
     * @param newRowIndex index at which new row is to be added */    
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
        numRows = numRows+1;
    }
    
    /** Inserts blank rows between different regions of the table and row header.
     */
    public void insertRegionBlanks() {
        if (tableLefter == null || tableLefter.isEmpty()) return;
        
        Vector newLefter = new Vector();
        String currRegion, prevRegion = "", lefterName, prevName;
        int index1, index2, size = tableLefter.size();
        int numBlanks = 0;
        boolean modified = false;
        
        if (tableLefter.size() == 1) {
            lefterName = (String)tableLefter.firstElement();
            index1 = lefterName.indexOf('<');
            tableLefter.setElementAt(lefterName.substring(0, index1), 0);
            return;
        }
        
        //check that there are more than one entry per region
        lefterName = tableLefter.elementAt(0).toString();
        //currRegion = tableLefter.elementAt(1).toString();
        index1 = lefterName.indexOf('<');
        
        //get the name of current region
        prevName = tableLefter.elementAt(0).toString();
        index1 = lefterName.indexOf('<');
        for (int j = 1; j < size; j++) {
            lefterName = tableLefter.elementAt(j).toString();
            index2 = lefterName.indexOf('<');

            //if each region only has one entry in the table (e.g USA <period>, 
            //  Canada <period>, ...) than add only the region name to the lefter
            if (index1 > 0 && index2 > 0) {
                if (lefterName.substring(index2).equals(prevName.substring(index1))) {
                    if (!modified) newLefter.addElement(prevName.substring(0, index1));
                    newLefter.addElement(lefterName.substring(0, index2));
                    modified = true;
                }
            }
            
            prevName = lefterName;
            index1 = index2;
        }
        if (modified) {
            //save the new lefter
            tableLefter = (Vector)newLefter.clone();
            return;
        }
        
        for (int j = 0; j < size; j++) {
            //get the name of current region
            lefterName = tableLefter.elementAt(j).toString();
            index1 = lefterName.indexOf('<');
            currRegion = lefterName.substring(0, index1);
            
            //otherwise, add the name of the region and insert a blank line
            //  then add the names of the actual field (without the region name) to the lefter
            if (!currRegion.equals(prevRegion)) {
                newLefter.addElement(currRegion);
                addBlankRow(j + numBlanks++);
            }
            
            //save the lefter without region name
            newLefter.addElement(lefterName.substring(index1));
            
            prevRegion = currRegion;
        }
        //save the new lefter
        tableLefter = (Vector)newLefter.clone();
    }
    
    /** Sets the value of a table cell.
     *
     * @param newValue String representation of cell's new value
     * @param row row index of target cell
     * @param col column index of target cell */    
    public void setValueAt(Object newValue, int row, int col) {
        ControlPanel.nodeValueChangedFlag = true;
        tableValues[row][col].setText(newValue.toString());
        fireTableCellUpdated(row, col);
    }
    
}
