import javax.swing.*;
import java.io.*;

public class CSVFilter extends javax.swing.filechooser.FileFilter
{

  public boolean accept(File f)
  {
    return f.getName().toLowerCase().endsWith(".csv")
          || f.isDirectory();
  }

  public String getDescription()
  {
    return "Comma Separated Values files (*.csv)";
  }
} 