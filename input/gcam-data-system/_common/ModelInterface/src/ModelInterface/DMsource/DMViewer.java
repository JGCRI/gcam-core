/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
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

package ModelInterface.DMsource;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.event.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Vector;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.w3c.dom.NodeList;

import ModelInterface.InterfaceMain;
import ModelInterface.MenuAdder;
import ModelInterface.BatchRunner;
import ModelInterface.InterfaceMain.MenuManager;
import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.ModelGUI2.XMLFilter;

public class DMViewer implements ActionListener, MenuAdder, BatchRunner
{
  
//*****************************************************************************
//*                            VARIABLES                                      *
//*****************************************************************************
  
  public static String controlStr = "DMViewer";
  
  private JFrame parentFrame;
  
  private File currFile;
  
  private JPanel pane;
  
  private JTextArea textArea;
  
  private JTextField dataSetField;
  
  private XMLFilter xmlFilter = new XMLFilter();

  
//*****************************************************************************
//*                            FUNCTIONS                                      *
//*****************************************************************************
  
//*************************Interface Setup Functions***************************
  
  public DMViewer()
  {
    parentFrame = InterfaceMain.getInstance().getFrame();
    
    
    //Add listener to add or remove menu items based on gaining and losing control
    parentFrame.addPropertyChangeListener(new PropertyChangeListener()
    {
      public void propertyChange(PropertyChangeEvent evt)
      {
        if(evt.getPropertyName().equals("Control"))
        {
          if(evt.getOldValue().equals(controlStr)||evt.getOldValue().equals(controlStr+"Same"))
          {
            //relinquishes control of the interface pane
            parentFrame.getContentPane().removeAll();
          }
          if(evt.getNewValue().equals(controlStr))
          {
            //sets up the interface pane for use with Data Manipulator
            setupPane();
          }
        }
      }
    });
  }
  
  
  public void addMenuItems(MenuManager menuMan)
  {
    //Adds an Open Preprocessor file menu item
    JMenuItem menuItem = new JMenuItem("DM Command file");
    menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D,
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
    
    if(command.equals("DM Command file"))
    {
      //sets up the panel for a data manipulator file
      status = openDMFile();
      if(status)
      {
        //file was opened, display it in the pane, set new title
        displayFile();
        parentFrame.setTitle("["+currFile+"] - ModelInterface");
      }
      
    } else if(command.equals("Run"))
    {
      //creates the actual file from pressing enter on file name or clicking the create button
        //dialog box to make sure user wants to create now
      int resp = JOptionPane.showConfirmDialog(parentFrame,"Are you sure you want to run the DataManipulator now?", 
          "Run DataManipulator?", JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
      if(resp == JOptionPane.NO_OPTION)
      {
        return;
      }
        //writes the command file to the disk
      writeFile();
        //using written file, runs the preprocessor
      runDataManipulator();
    } else if(command.equals("FindDataSet"))
    {
      //lets the user choose a dataset from a popup window
      findDataSet();
    }
  }
  
//*********************My Private Functions************************************
  /**
     * Creates a JFileChooser to figure out which file to open, then displays
     * the file and sets current doc to it
     * 
     * @return true if we opened a file, false otherwise
     */
  private boolean openDMFile()
  {
    JFileChooser fc = new JFileChooser();
    fc.setDialogTitle("Open XML Command File");

    // Choose only files, not directories
    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    // Start in current directory
    fc.setCurrentDirectory(new File(InterfaceMain.getInstance().getProperties().getProperty("lastDirectory", ".")));

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
      InterfaceMain.getInstance().fireControlChange(controlStr);
      currFile = fc.getSelectedFile();
      
      //set last directory for subsequent file opens
      InterfaceMain.getInstance().getProperties().setProperty("lastDirectory",
          fc.getCurrentDirectory().toString());
      
    } else
    { //not sure what would get us here but, to be safe, return false
      return false;
    }
    
    //if we reach the end a file was successfully opened
    return true;
  }
  
  private void findDataSet()
  {
    JFileChooser fc = new JFileChooser();
    fc.setDialogTitle("Open XML Data File");

    // Choose only files, not directories
    fc.setFileSelectionMode(JFileChooser.FILES_ONLY);

    // Start in current directory
    fc.setCurrentDirectory(new File(InterfaceMain.getInstance().getProperties().getProperty("lastDirectory", ".")));

    // Set filter for XML files.
    fc.setFileFilter(xmlFilter);

    // Now open chooser
    int result = fc.showOpenDialog(parentFrame);
    
    if(result==JFileChooser.CANCEL_OPTION)
    { //user canceled file open, do nothing
      return;
    } else if(result==JFileChooser.APPROVE_OPTION)
    { 
      //user selected an XML file, store its path
      File holdFile = fc.getSelectedFile();
      dataSetField.setText(holdFile.getPath());
      
      //set last directory for subsequent file opens
      InterfaceMain.getInstance().getProperties().setProperty("lastDirectory",
          fc.getCurrentDirectory().toString());
      
    } else
    { //not sure what would get us here but, to be safe, return with no change
      return;
    }
    
    //if we reach the end a file was successfully found
  }
  
  private void displayFile()
  {
    //at this point we have a file, just need to get the text from it, and 
    //display that text in the textarea
    if(currFile == null)
    { //safety, incase opening a file failed
      return;
    }
    
    try
    {
      int holdFile = 0;
      char fromFile;
      String toText = "";
      FileReader fReader = new FileReader(currFile);
      
      //reading the file character by character
      while((holdFile = fReader.read()) != -1)
      {
        fromFile = (char)holdFile;
        toText += fromFile;
      }
      fReader.close();
      
      //setting the text area to contain the read text
      textArea.setText(toText);
      
    } catch(FileNotFoundException e)
    {
      System.out.println("The file "+currFile.getName()+" does not exist, how I dont know.");
    } catch(IOException e)
    {
      System.out.println("Encountered an error while attempting to read from "+currFile.getName()+".");
    }
  }
  
  private void setupPane()
  {
    pane = new JPanel();
    SpringLayout layout = new SpringLayout();
    pane.setLayout(layout);
    
    //setting up the dataset and locate buttons
    JLabel dLabel = new JLabel("Data Set:");
    layout.putConstraint(SpringLayout.WEST, dLabel, 10, SpringLayout.WEST, pane);
    layout.putConstraint(SpringLayout.NORTH, dLabel, 12, SpringLayout.NORTH, pane);
    
    dataSetField = new JTextField(40);
    dataSetField.setMaximumSize(dataSetField.getPreferredSize());
    layout.putConstraint(SpringLayout.WEST, dataSetField, 5, SpringLayout.EAST, dLabel);
    layout.putConstraint(SpringLayout.NORTH, dataSetField, 10, SpringLayout.NORTH, pane);
    
    JButton fButton = new JButton("Find Data Set");
    fButton.setActionCommand("FindDataSet");
    fButton.addActionListener(this);
    layout.putConstraint(SpringLayout.WEST, fButton, 10, SpringLayout.EAST, dataSetField);
    layout.putConstraint(SpringLayout.NORTH, fButton, 8, SpringLayout.NORTH, pane);
    
    //setting up label and run button
    JLabel cLabel = new JLabel("Command File:");
    layout.putConstraint(SpringLayout.NORTH, cLabel, 10, SpringLayout.SOUTH, fButton);
    layout.putConstraint(SpringLayout.WEST, cLabel, 10, SpringLayout.WEST, pane);
    
    JButton rButton = new JButton("Run");
    rButton.setActionCommand("Run");
    rButton.addActionListener(this);
    layout.putConstraint(SpringLayout.NORTH, rButton, 5, SpringLayout.SOUTH, fButton);
    layout.putConstraint(SpringLayout.EAST, rButton, -10, SpringLayout.EAST, pane);
    
    //setting up text area
    textArea = new JTextArea(25, 80);
    JScrollPane scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    textArea.setEditable(true);
    textArea.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
    layout.putConstraint(SpringLayout.WEST, scrollPane, 10, SpringLayout.WEST, pane);
    layout.putConstraint(SpringLayout.NORTH, scrollPane, 5, SpringLayout.SOUTH, rButton);
    
    //making the window create at the correct size
    layout.putConstraint(SpringLayout.EAST, pane, 10, SpringLayout.EAST, scrollPane);
    layout.putConstraint(SpringLayout.SOUTH, pane, 10, SpringLayout.SOUTH, scrollPane);
    
    //adding the components to the pane
    pane.add(dLabel);
    pane.add(dataSetField);
    pane.add(fButton);
    pane.add(cLabel);
    pane.add(rButton);
    pane.add(scrollPane);
    
    parentFrame.setContentPane(pane);
    parentFrame.pack();
  }
  
  private void writeFile()
  {
    //have a text file in the textarea, need to write that text to a file
    //use as the file name the name of the file that was opened
    
    if(currFile == null)
    { //safety, incase opening a file failed
      return;
    }
    
    try
    {
      FileWriter fWriter = new FileWriter(currFile);
      
      //writing the contents of the textArea to the file which was opened
      fWriter.write(textArea.getText());
      fWriter.flush();
      fWriter.close();
    } catch(IOException e)
    {
      System.out.println("Encountered an error while attempting to write to "+currFile.getName()+".");
    }
    
  }
  
  private void runDataManipulator()
  {
    Handler fHand, cHand;
    String dSource, rSource, cSource;
    try
    {
      dSource = dataSetField.getText();
      
      //***init logger classes for DM
      Logger log = Logger.getLogger("DataManipulation");
      log.setLevel(Level.parse("WARNING"));
      log.setUseParentHandlers(false);
      
      cHand = new ConsoleHandler();
      cHand.setLevel(Level.WARNING);//use ALL for diagnostics, WARNING usually
      log.addHandler(cHand);
      try
      {
        fHand = new FileHandler("DMLog.log");
        fHand.setLevel(Level.ALL);
        fHand.setFormatter(new SimpleFormatter());
        log.addHandler(fHand);
      } catch(SecurityException e)
      {
        e.printStackTrace();
      } catch(IOException e)
      {
        e.printStackTrace();
      }
      //***done initing DM logger
      
      rSource = "regionDef.xml";
      cSource = currFile.getName();
      
      log.log(Level.INFO, "creating ManipulationDriver to run datamanipulation");
      throw new UnsupportedOperationException("This feature is not available in this version.");
      /*
      ManipulationDriver mainRun = new ManipulationDriver(dSource, rSource, cSource);
      log.log(Level.INFO, "calling runAll in DataBuilder");
      mainRun.runAll();
      JOptionPane.showMessageDialog(parentFrame, "The DataManipulator has completed running.", "DataManipulator Completed",
          JOptionPane.PLAIN_MESSAGE);
      */
    } catch(NullPointerException e)
    {
      e.printStackTrace();
      JOptionPane.showMessageDialog(parentFrame, "Data Manipulator requires a dataset to run.", "DataManipulator Error",
          JOptionPane.PLAIN_MESSAGE);
      return;
    }
  }
  public void runBatch(Node command) {
	  // set up logging
	  Handler fHand, cHand;
	  Logger log = Logger.getLogger("DataManipulation");
	  log.setLevel(Level.parse("WARNING"));
	  log.setUseParentHandlers(false);

	  cHand = new ConsoleHandler();
	  cHand.setLevel(Level.WARNING);//use ALL for diagnostics, WARNING usually
	  log.addHandler(cHand);
	  try
	  {
		  fHand = new FileHandler("DMLog.log");
		  fHand.setLevel(Level.ALL);
		  fHand.setFormatter(new SimpleFormatter());
		  log.addHandler(fHand);
	  } catch(SecurityException e)
	  {
		  e.printStackTrace();
	  } catch(IOException e)
	  {
		  e.printStackTrace();
	  }

	  NodeList children = command.getChildNodes();
	  String dataFilename = null;
	  String regionDefFilename = null;
	  String commandFilename = null;
	  for(int i = 0; i < children.getLength(); ++i ) {
		  Node child = children.item(i);
		  // TODO: put in a parse filter for this
		  if(child.getNodeType() != Node.ELEMENT_NODE) {
			  continue;
		  }
		  String actionCommand = ((Element)child).getNodeName();
		  if(actionCommand.equals("data")) {
			  dataFilename = ((Element)child).getAttribute("file");
		  } else if(actionCommand.equals("region")) {
			  regionDefFilename = ((Element)child).getAttribute("file");
		  } else if(actionCommand.equals("command")) {
			  commandFilename = ((Element)child).getAttribute("file");
		  } else if(actionCommand.equals("log")) {
			  log.setLevel(Level.parse(((Element)child).getAttribute("level")));
		  } else {
			  log.warning("Unreckognized command: "+actionCommand);
		  }
	  }
	  if(dataFilename == null || regionDefFilename == null || commandFilename == null) {
		  log.severe("Not enough information to run data manipulator");
	  } else {
          throw new UnsupportedOperationException("This feature is not available in this version.");
          /*
		  ManipulationDriver mainRun = new ManipulationDriver(dataFilename, regionDefFilename, commandFilename);
		  log.log(Level.INFO, "calling runAll in DataBuilder");
		  mainRun.runAll();
          */
	  }

  }
}
