/*
 * JTableLefter.java
 *
 * Created on June 11, 2003, 5:27 PM
 */

package ModelGUI;

/**
 *
 * @author  Yulia Eyman (yulia@wam.umd.edu)
 */

import javax.swing.*;

public class JTableLefter extends javax.swing.JComponent {
    JButton[] lefters;
    
    /** Creates a new instance of JTableLefter */
    public JTableLefter(String[] names) {
        int height = names.length;
        lefters = new JButton[height];
        
        for (int j = 0; j < height; j++) {
            //create an array of non-editable buttons
            lefters[j] = new JButton(names[j]);
        }
    }
    
}
