/*
 * TableTransferHandler.java
 *
 * Created on June 24, 2003, 10:34 PM
 */

package ModelGUI;

import java.io.*;
import javax.swing.*;
import javax.swing.table.*;
import java.awt.datatransfer.*;

/** This class handles copying and pasting into a <code> JTable </code> using 
 * the Ctrl-c and Ctrl-v shortcuts. Most of the code is copied from the Java tutorial. 
 *
 * WARNING: currently, this implementation does not function correctly on Mac OS. 
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

public class TableTransferHandler extends TransferHandler{
    private int[] rows = null;
    private int addIndex = -1; //Location where items were added
    private int addCount = 0;  //Number of items added.
    
    /* Creates a new instance of TableTransferHandler */
    /*public TableTransferHandler() {
        super();
        clipboard = ControlPanel.toolkit.getSystemClipboard ();
    }*/
    
    /** Creates the object that will handle String copy-pasting.
     *
     * @param c JComponent to which the transfers will be liked
     * @return Transferable object */    
    protected Transferable createTransferable(JComponent c) {
        return new StringSelection(exportString(c));
    }
    
    /** 
     * @param c JComponent that 
     * @return COPY */    
    public int getSourceActions(JComponent c) {
        //JOptionPane.showMessageDialog(null, "Copy request registered");
        return COPY;
    }
       
    public boolean importData(JComponent c, Transferable t) {
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
        cleanup(c, action == MOVE);
    }
    
    /** Tests if the type of information that will be copied can be copied.
     * Currently, TableTransferHandler only supports String copy operation.
     *
     * @return true if type can be transfered */    
    public boolean canImport(JComponent c, DataFlavor[] flavors) {
        for (int i = 0; i < flavors.length; i++) {
            if (DataFlavor.stringFlavor.equals(flavors[i])) {
                return true;
            }
        }
        return false;
    }

    /** Called to handle copying of a String.
     * @see importString
     * 
     * @param c JComponent from wich String is being copied
     * @return the String content of the copy operation */    
    protected String exportString(JComponent c) {
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

    /**
     * Function that handles pasting into the table - specifically coded to 
     * paste from Excel. Interprets String's '\n' characters as row breaks
     * and '\t' characters as column breaks.
     *
     * @param c JTable that is being modified
     * @param str content to paste into the table */    
    protected void importString(JComponent c, String str) {
        ControlPanel.nodeValueChangedFlag = true;
        JTable table = (JTable)c;
        TableViewModel model = (TableViewModel)table.getModel();
        int[] rowIndexes = table.getSelectedRows();
        int[] colIndexes = table.getSelectedColumns();
        
        int numCols = table.getColumnCount();
        int currRow, currCol;
        
        String[] newRowVals = str.split("\n");
        for (int j = 0; j < rowIndexes.length; j++) {
            currRow = rowIndexes[j];
            String[] newCellVals = newRowVals[j].split("\t");
            
            for (int k = 0; k < colIndexes.length; k++) {
                currCol = colIndexes[k];
                model.setValueAt(newCellVals[k], currRow, currCol);
            }
        }
    }
    
    /** Called at the end of a transfer operation */
    protected void cleanup(JComponent c, boolean remove) {
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
