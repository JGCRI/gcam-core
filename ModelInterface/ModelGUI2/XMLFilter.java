//package ModelGUI2;
package ModelInterface.ModelGUI2;
import java.io.*;

public class XMLFilter extends javax.swing.filechooser.FileFilter
{

  /**
   * Used to determine which files to allow to be selected in the
   * JFileChooser.
   * @param f the file in question
   * @return true if it is an XML file or directory, false otherwise
   */ 
  public boolean accept(File f)
  {
    return f.getName().toLowerCase().endsWith(".xml")
          || f.isDirectory();
  }

  /**
   * Returns a discriptiion of type of files that would be accepted
   * @return a string which specifies XML files, files that in with .xml
   */
  public String getDescription()
  {
    return "XML files (*.xml)";
  }
} 
