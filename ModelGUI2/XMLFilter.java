import javax.swing.*;
import java.io.*;

public class XMLFilter extends javax.swing.filechooser.FileFilter
{

  public boolean accept(File f)
  {
    return f.getName().toLowerCase().endsWith(".xml")
          || f.isDirectory();
  }

  public String getDescription()
  {
    return "XML files (*.xml)";
  }
} 
