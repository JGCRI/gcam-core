/*
 * TableTransferHandler.java
 *
 * Created on June 24, 2003, 10:34 PM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import java.io.*;
import javax.swing.*;
import javax.swing.table.*;
import java.awt.datatransfer.*;

public class TableTransferHandler extends TransferHandler {
    private int[] rows = null;
    private int addIndex = -1; //Location where items were added
    private int addCount = 0;  //Number of items added.
    
    /** Creates a new instance of TableTransferHandler */
    //public TableTransferHandler() {
    //}

    
    protected Transferable createTransferable(JComponent c) {
//System.out.println("createTransferable");
        return new StringSelection(exportString(c));
    }
    
    public int getSourceActions(JComponent c) {
//System.out.println("getSourceActions");
        return COPY;
    }
    
    public boolean importData(JComponent c, Transferable t) {
//System.out.println("importData");
        if (canImport(c, t.getTransferDataFlavors())) {
            try {
                String str = (String)t.getTransferData(DataFlavor.stringFlavor);
                importString(c, str);
                return true;
            } catch (UnsupportedFlavorException ufe) {
            } catch (IOException ioe) {
            }
        }

        return false;
    }
    
    protected void exportDone(JComponent c, Transferable data, int action) {
//System.out.println("exportDone");
        cleanup(c, action == MOVE);
    }
    
    public boolean canImport(JComponent c, DataFlavor[] flavors) {
//System.out.println("canImport");
        for (int i = 0; i < flavors.length; i++) {
            if (DataFlavor.stringFlavor.equals(flavors[i])) {
                return true;
            }
        }
        return false;
    }

    

    protected String exportString(JComponent c) {
//System.out.println("exportString");
        JTable table = (JTable)c;
        rows = table.getSelectedRows();
        int[] cols = table.getSelectedColumns();
        
        StringBuffer buff = new StringBuffer();
        
        for (int i = 0; i < rows.length; i++) {
            for (int j = 0; j < cols.length; j++) {
                Object val = table.getValueAt(rows[i], cols[j]);
                buff.append(val == null ? "" : val.toString());
                if (j != cols.length - 1) {
                    buff.append("\t");
                }
            }
            if (i != rows.length - 1) {
                buff.append("\n");
            }
        }
        
        return buff.toString();
    }

    /*
     * Function that handles pasting into the table - specifically coded to paste from Excel
     *      Accepts string where row breaks are \n characters and cell breaks are \t characters
     */
    protected void importString(JComponent c, String str) {
//System.out.println("importString");
        JTable table = (JTable)c;
        TableViewModel model = (TableViewModel)table.getModel();
        int[] rowIndexes = table.getSelectedRows();
        int[] columnIndexes = table.getSelectedColumns();
        int currRow;
        int currCol;
        
        String[] newRowVals = str.split("\n");
        for (int j = 0; j < rowIndexes.length && j < newRowVals.length; j++) {
            currRow = rowIndexes[j];
            String[] newCellVals = newRowVals[j].split("\t");
            
            for (int col = 0; col < columnIndexes.length && col < newCellVals.length; col++) {
            	currCol = columnIndexes[ col ];
                model.setValueAt(newCellVals[col], currRow, currCol );
            }
        }
    }
    
    protected void cleanup(JComponent c, boolean remove) {
//System.out.println("cleanup");
        JTable source = (JTable)c;
        if (remove && rows != null) {
            DefaultTableModel model =
                 (DefaultTableModel)source.getModel();

            //If we are moving items around in the same table, we
            //need to adjust the rows accordingly, since those
            //after the insertion point have moved.
            if (addCount > 0) {
                for (int i = 0; i < rows.length; i++) {
                    if (rows[i] > addIndex) {
                        rows[i] += addCount;
                    }
                }
            }
            for (int i = rows.length - 1; i >= 0; i--) {
                model.removeRow(rows[i]);
            }
        }
        rows = null;
        addCount = 0;
        addIndex = -1;
    }

    
}
