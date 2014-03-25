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

package ModelInterface.PPsource;


import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.event.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

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

import ModelInterface.InterfaceMain;
import ModelInterface.MenuAdder;
import ModelInterface.InterfaceMain.MenuManager;
import ModelInterface.ModelGUI2.DOMmodel;
import ModelInterface.ModelGUI2.Documentation;
import ModelInterface.ModelGUI2.XMLFilter;
import ModelInterface.BatchRunner;

public class PPViewer implements ActionListener, MenuAdder, BatchRunner
{
  
//*****************************************************************************
//*                            VARIABLES                                      *
//*****************************************************************************
  
  public static String controlStr = "PPViewer";
  
  private JFrame parentFrame;
  
  private File currFile;
  
  private JPanel pane;
  
  private JTextArea textArea;
  
  private JTextField fileNameField;
  
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
            parentFrame.getContentPane().removeAll();
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
      
    } else if(command.equals("Create"))
    {
      //creates the actual file from pressing enter on file name or clicking the create button
        //dialog box to make sure user wants to create now
      int resp = JOptionPane.showConfirmDialog(parentFrame,"Are you sure you want to run the Preprocessor now?", 
          "Run Preprocessor?", JOptionPane.YES_NO_OPTION, JOptionPane.PLAIN_MESSAGE);
      if(resp == JOptionPane.NO_OPTION)
      {
        return;
      }
        //writes the data definition file to the disk
      writeFile();
        //using written file, runs the preprocessor
      runPreprocess();
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
      
    } else
    { //not sure what would get us here but, to be safe, return false
      return false;
    }
    
    //if we reach the end a file was successfully opened
    return true;
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
    
    //setting up the file name stuff
    JLabel fLabel = new JLabel("File Name:");
    layout.putConstraint(SpringLayout.WEST, fLabel, 10, SpringLayout.WEST, pane);
    layout.putConstraint(SpringLayout.NORTH, fLabel, 10, SpringLayout.NORTH, pane);
    
    fileNameField = new JTextField(30);
    fileNameField.setActionCommand("Create");
    fileNameField.addActionListener(this);
    fileNameField.setMaximumSize(fileNameField.getPreferredSize());
    layout.putConstraint(SpringLayout.WEST, fileNameField, 5, SpringLayout.EAST, fLabel);
    layout.putConstraint(SpringLayout.NORTH, fileNameField, 10, SpringLayout.NORTH, pane);
    
    //setting up label and create button
    JLabel dLabel = new JLabel("Definition:");
    layout.putConstraint(SpringLayout.NORTH, dLabel, 5, SpringLayout.SOUTH, fileNameField);
    layout.putConstraint(SpringLayout.WEST, dLabel, 10, SpringLayout.WEST, pane);
    
    JButton cButton = new JButton("Create");
    cButton.setActionCommand("Create");
    cButton.addActionListener(this);
    layout.putConstraint(SpringLayout.NORTH, cButton, 10, SpringLayout.NORTH, pane);
    layout.putConstraint(SpringLayout.EAST, cButton, -10, SpringLayout.EAST, pane);
    
    //setting up text area
    textArea = new JTextArea(25, 80);
    JScrollPane scrollPane = new JScrollPane(textArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    textArea.setEditable(true);
    textArea.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
    layout.putConstraint(SpringLayout.WEST, scrollPane, 10, SpringLayout.WEST, pane);
    layout.putConstraint(SpringLayout.NORTH, scrollPane, 5, SpringLayout.SOUTH, dLabel);
    
    //making the window create at the correct size
    layout.putConstraint(SpringLayout.EAST, pane, 10, SpringLayout.EAST, scrollPane);
    layout.putConstraint(SpringLayout.SOUTH, pane, 10, SpringLayout.SOUTH, scrollPane);
    
    //adding the components to the pane
    pane.add(fLabel);
    pane.add(fileNameField);
    pane.add(dLabel);
    pane.add(cButton);
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
  
  private void runPreprocess()
  {
    Handler fHand, cHand;
    String dSource, rSource, dOutput;
    
    //***init logger classes for PP
    Logger log = Logger.getLogger("Preprocess");
    log.setLevel(Level.parse("WARNING"));
    log.setUseParentHandlers(false);
    
    cHand = new ConsoleHandler();
    cHand.setLevel(Level.WARNING);//use ALL for diagnostics, WARNING usually
    log.addHandler(cHand);
    try
    {
      fHand = new FileHandler("PPLog.log");
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
    //***done initing PP logger
    
    dSource = currFile.getName();
    rSource = "inputR.xml";
    dOutput = fileNameField.getText();
    if(dOutput == null)
    {
      dOutput = "modelInterfacePPoutput";
    } else if(!dOutput.endsWith(".xml"))
    {
      dOutput = dOutput.concat(".xml");
    }
    
    log.log(Level.INFO, "creating DataBuilder to run preprocessing");
    throw new UnsupportedOperationException("This feature is not available in this version.");
    /*
    DataBuilder mainRun = new DataBuilder(dSource, rSource, dOutput);
    log.log(Level.INFO, "calling runAll in DataBuilder");
    mainRun.runAll();
    JOptionPane.showMessageDialog(parentFrame, "The Preprocessor has completed running.", "Preprocessor Completed",
        JOptionPane.PLAIN_MESSAGE);
    */
  }

  public void runBatch(Node command) {
	  // set up logging
	  Handler fHand, cHand;
	  Logger log = Logger.getLogger("Preprocess");
	  log.setLevel(Level.parse("WARNING"));
	  log.setUseParentHandlers(false);

	  cHand = new ConsoleHandler();
	  cHand.setLevel(Level.WARNING);//use ALL for diagnostics, WARNING usually
	  log.addHandler(cHand);
	  try
	  {
		  fHand = new FileHandler("PPLog.log");
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
	  String outputFilename = null;
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
		  } else if(actionCommand.equals("output")) {
			  outputFilename = ((Element)child).getAttribute("file");
		  } else if(actionCommand.equals("log")) {
			  log.setLevel(Level.parse(((Element)child).getAttribute("level")));
		  } else {
			  log.warning("Unreckognized command: "+actionCommand);
		  }
	  }
	  if(dataFilename == null || regionDefFilename == null || outputFilename == null) {
		  log.severe("Not enough information to run data manipulator");
	  } else {
          throw new UnsupportedOperationException("This feature is not available in this version.");
          /*
		  log.log(Level.INFO, "creating DataBuilder to run preprocessing");
		  DataBuilder mainRun = new DataBuilder(dataFilename, regionDefFilename, outputFilename);
		  log.log(Level.INFO, "calling runAll in DataBuilder");
		  mainRun.runAll();
		  // TODO: maybe we should let the user know this batch finished however we
		  // don't want to stop and wait for user input such as with a pop-up dialog
          */
	  }

  }
}
