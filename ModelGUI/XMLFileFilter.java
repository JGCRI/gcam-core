/*
 * XMLFileFilter.java
 *
 * Created on September 22, 2003, 5:12 PM
 */

package ModelGUI;

import javax.swing.filechooser.*;
import javax.swing.*;
import java.io.File;

/** This class will allow only .xml files to be displayed by default when the  
 * user tries to open a new file to view.  
 *
 * @author  Yulia Eyman (yulia@umd.edu)
 */
public class XMLFileFilter extends FileFilter {

    public boolean accept(File f) {
        if (f.isDirectory()) {
            return true;
        }

        String fname = f.getName();
        int index = fname.lastIndexOf('.') + 1;
        if (index >= 0) {
            String extension = fname.substring(index, fname.length());
            if (extension.equals("xml")) {
                    return true;
            } else {
                return false;
            }
        }
        return false;

    }
    
    public String getDescription() {
        return ".xml";
    }
    
}
