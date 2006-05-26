/*
 * This software, which is provided in confidence, was prepared by employees
        of Pacific Northwest National Laboratory operated by Battelle Memorial
        Institute. Battelle has certain unperfected rights in the software 
        which should not be copied or otherwise disseminated outside your 
        organization without the express written authorization from Battelle. All rights in
        the software are reserved by Battelle.  Battelle makes no warranty,
        express or implied, and assumes no liability or responsibility for the
        use of this software.
 */
/*!
 * \file PPViewer.java
 * \ingroup PPsource
 * \brief Viewer class which handles interface for PreProcessor
 *
 *
 * \author Vincent Nibali
 * \date $Date: 2006-04-07 14:25:43 -0400 (Fri, 07 Apr 2006) $
 * \version $Revision: 1 $
 */
/*
 * Portions of this code were based on similar code in InputViewer.java,
 * by Pralit Patel
 */

package ModelInterface.PPsource;


import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.event.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import ModelInterface.InterfaceMain;
import ModelInterface.MenuAdder;
import ModelInterface.InterfaceMain.MenuManager;
import ModelInterface.ModelGUI2.DOMPasteboard;
import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.ModelGUI2.XMLFilter;

public class PPViewer implements ActionListener, MenuAdder
{
  
//*****************************************************************************
//*                            VARIABLES                                      *
//*****************************************************************************
  
  public static String controlStr = "PPViewer";
  
  private JFrame parentFrame;
  
  private File currFile;
  
  private JPanel pane;
  
  private JTextArea textArea;
  
  private XMLFilter xmlFilter = new XMLFilter();

  
//*****************************************************************************
//*                            FUNCTIONS                                      *
//*****************************************************************************
  
//*************************Interface Setup Functions***************************
  
  public PPViewer(JFrame parentFrameIn)
  {
    parentFrame = parentFrameIn;
    
    
    //Add listener to add or remove menu items based on gaining and losing control
    parentFrame.addPropertyChangeListener(new PropertyChangeListener()
    {
      public void propertyChange(PropertyChangeEvent evt)
      {
        if(evt.getPropertyName().equals("Control"))
        {
          if(evt.getOldValue().equals(controlStr)||evt.getOldValue().equals(controlStr+"Same"))
          {
            //TODO relinquish control of the interface Panel
            /* - pralit's stuff
            ((InterfaceMain)parentFrame).getSaveMenu().removeActionListener(thisViewer);
            ((InterfaceMain)parentFrame).getSaveAsMenu().removeActionListener(thisViewer);
            ((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(false);
            ((InterfaceMain)parentFrame).removePropertyChangeListener(savePropListener);
            // ((InterfaceMain)parentFrame).getQuitMenu().removeActionListener(thisViewer);
            ((InterfaceMain)parentFrame).getSaveMenu().setEnabled(false);
            doc = null;
            documentation = null;
            parentFrame.getContentPane().removeAll();
            parentFrame.setTitle("ModelInterface");
            if(splitPane!=null)
            {
              ((InterfaceMain)parentFrame).getProperties().setProperty("dividerLocation",
                  String.valueOf(splitPane.getDividerLocation()));
            }
            */
          }
          if(evt.getNewValue().equals(controlStr))
          {
            //TODO gaining control of the interface Panel
            /*
            ((InterfaceMain)parentFrame).getSaveMenu().addActionListener(thisViewer);
            ((InterfaceMain)parentFrame).getSaveAsMenu().addActionListener(thisViewer);
            ((InterfaceMain)parentFrame).getSaveAsMenu().setEnabled(true);
            ((InterfaceMain)parentFrame).addPropertyChangeListener(savePropListener);
            leftWidth = Integer.parseInt(((InterfaceMain)parentFrame).getProperties().getProperty(
                "dividerLocation", "200"));
            */
            setupPane();
          }
        }
      }
    });
  }
  
  
  public void addMenuItems(MenuManager menuMan)
  {
    //Adds an Open Preprocessor file menu item
    JMenuItem menuItem = new JMenuItem("Preprocessor file");
    menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P,
            ActionEvent.ALT_MASK));
    menuItem.addActionListener(this);
    menuMan.getSubMenuManager(InterfaceMain.FILE_MENU_POS).
        getSubMenuManager(InterfaceMain.FILE_OPEN_SUBMENU_POS).addMenuItem(menuItem, 10);
  }
  
  /**
   * Process events from the menu items.
   * 
   * @param e the event, only care about a click on a menu item
   */
  public void actionPerformed(ActionEvent e)
  {//handle all button clicks
    boolean status = false;
    String command = e.getActionCommand();
    
    //TODO handle create datafi,le button clicks
    
    if(command.equals("Preprocessor file"))
    {
      //sets up the panel for a preprocessor file
      status = openPPFile();
      
      if(status)
      {
        //file was opened, display it in the pane, set new title
        displayFile();
        parentFrame.setTitle("["+currFile+"] - ModelInterface");
      }
      
    } else if(command.equals("stub"))
    {
      //does nothing
    }
  }
  
//*********************My Private Functions************************************
  /**
     * Creates a JFileChooser to figure out which file to open, then displays
     * the file and sets current doc to it
     * 
     * @return true if we opened a file, false otherwise
     */
  private boolean openPPFile()
  {
    JFileChooser fc = new JFileChooser();
    fc.setDialogTitle("Open XML File");

    // Choose only files, not directories
    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    // Start in current directory
    fc.setCurrentDirectory(new File(((InterfaceMain)parentFrame).getProperties().getProperty("lastDirectory", ".")));

    // Set filter for Java source files.
    fc.setFileFilter(xmlFilter);

    // Now open chooser
    int result = fc.showOpenDialog(parentFrame);
    
    if(result==JFileChooser.CANCEL_OPTION)
    { //user canceled file open, return with no changes
      return false;
    } else if(result==JFileChooser.APPROVE_OPTION)
    { 
      //user selected an XML file, open it and fire a control change
      ((InterfaceMain)parentFrame).fireControlChange(controlStr);
      currFile = fc.getSelectedFile();
      
      //set last directory for subsequent file opens
      ((InterfaceMain)parentFrame).getProperties().setProperty("lastDirectory",
          fc.getCurrentDirectory().toString());

      //TODO get the file? display it?
      //doc = readXMLFile(currFile);
      
    } else
    { //not sure what would get us here but, to be safe, return false
      return false;
    }
    
    //if we reach the end a file was successfully opened
    return true;
  }
  
  private void displayFile()
  {
    
  }
  
  private void setupPane()
  {
    pane = new JPanel();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
    pane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
    
    pane.add(createFileComponents());
    pane.add(createDefinitionComponents());
    pane.add(createTextArea());
    
    parentFrame.setContentPane(pane);
    parentFrame.pack();
  }
  
  private Component createFileComponents()
  {
    JPanel myPane = new JPanel();
    myPane.setLayout(new BoxLayout(myPane, BoxLayout.LINE_AXIS));
    
    JLabel fLabel = new JLabel("File Name:");
    myPane.add(fLabel);
    
    JTextField fField = new JTextField(30);
    fField.addActionListener(this);
    myPane.add(fField);
    
    return myPane;
  }
  
  private Component createDefinitionComponents()
  {
    //TODO
    JPanel myPane = new JPanel();
    
    JLabel dLabel = new JLabel("Definition:");
    myPane.add(dLabel);
    
    JButton cButton = new JButton("Create");
    cButton.addActionListener(this);
    myPane.add(cButton);
    
    return myPane;
  }
  
  private Component createTextArea()
  {
    textArea = new JTextArea(100, 80);
    JScrollPane scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    textArea.setEditable(true);
    
    return scrollPane;
  }
}
