package ModelGUI2;
import java.io.*;

public class CSVFilter extends javax.swing.filechooser.FileFilter
{

  /**
   * Used to determine which files to allow to be selected in the
   * JFileChooser.
   * @param f the file in question
   * @return true if it is an CSV file or directory, false otherwise
   */ 
  public boolean accept(File f)
  {
    return f.getName().toLowerCase().endsWith(".csv")
          || f.isDirectory();
  }

  /**
   * Returns a discriptiion of type of files that would be accepted
   * @return a string which specifies CSV files, files that in with .csv
   */
  public String getDescription()
  {
    return "Comma Separated Values files (*.csv)";
  }
} 
