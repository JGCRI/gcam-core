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
 * \file Console.java
 * \ingroup DataManipulation
 * \brief Class for outputting simple text to the screen.
 *
 *
 * \author Vincent Nibali
 * \date $Date: 2005-08-25 08:58:17 -0400 (Thu, 25 Aug 2005) $
 * \version $Revision: 2277 $
 */
package ModelInterface.DMsource;

import java.awt.Dimension;

import javax.swing.*;

import java.awt.*;


public class Console
{
  final static String LOOKANDFEEL = "System";
  static int openWindows = 0;
  
  private String name;
  private JTextArea textBox;
  private JScrollPane scrollBox;
  
  
//*****************************************************************************
//****************************Constructor**************************************
//*****************************************************************************
  
  protected Console(String n)
  {
    openWindows++;
    name = n;
    
    //Set the look and feel.
    initLookAndFeel();

    //Make sure we have nice window decorations.
    JFrame.setDefaultLookAndFeelDecorated(true);
    
    JFrame frame = new JFrame(name);
    frame.addWindowListener(new java.awt.event.WindowAdapter()
    {
      public void windowClosing(java.awt.event.WindowEvent e)
      {
        ConsoleManager cm;
        cm = ConsoleManager.getConsoleManager();
        cm.removeConsole(name);
        
        if(openWindows==1)
        {
          System.exit(0); // Terminate when the last window is closed.
        }
        openWindows--;
      }
    });
    
    JPanel pane = new JPanel();
    pane.setLayout(new BoxLayout(pane, BoxLayout.Y_AXIS));
    pane.setBorder(BorderFactory.createEmptyBorder(10,10,10,10));
    pane.add(addTextArea());
    
    
    //Display the window.
    frame.setContentPane(pane);
    frame.pack();
    frame.setSize(new Dimension(640, 480));

    frame.setVisible(true);
  }
  
//*****************************************************************************
//**************************Main Functions*************************************
//*****************************************************************************
  
  public String getName()
  {
    return name;
  }
  
  public static Console getConsole(String name)
  {
    ConsoleManager cm;
    Console result;
    
    cm = ConsoleManager.getConsoleManager();
    result = cm.getConsole(name);
    
    if(result == null)
    { //this Console must be added
      result = new Console(name);
      cm.addConsole(result);
      
      return result;
    } else
    { //console already exists, return it
      return result;
    }
  }
  
  public void addText(String text)
  {
    textBox.append("\n");
    textBox.append(text);
    textBox.revalidate();
    scrollBox.revalidate();
  }
  
  public void write(String text)
  {
    textBox.append(text);
    textBox.revalidate();
    scrollBox.revalidate();
  }
  
  
//*****************************************************************************
//****************************My Functions*************************************
//*****************************************************************************  
  
  private Component addTextArea()
  {
    textBox = new JTextArea();
    textBox.setEditable(false);
    scrollBox = new JScrollPane(textBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
        JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

    return scrollBox;
  }
  
  private static void initLookAndFeel()
  {
    String lookAndFeel = null;

    if(LOOKANDFEEL!=null)
    {
      if(LOOKANDFEEL.equals("Metal"))
      {
        lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
      } else if(LOOKANDFEEL.equals("System"))
      {
        lookAndFeel = UIManager.getSystemLookAndFeelClassName();
      } else if(LOOKANDFEEL.equals("Motif"))
      {
        lookAndFeel = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
      } else if(LOOKANDFEEL.equals("GTK+"))
      { // new in 1.4.2
        lookAndFeel = "com.sun.java.swing.plaf.gtk.GTKLookAndFeel";
      } else
      {
        System.err.println("Unexpected value of LOOKANDFEEL specified: "
            +LOOKANDFEEL);
        lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
      }

      try
      {
        UIManager.setLookAndFeel(lookAndFeel);
      } catch(ClassNotFoundException e)
      {
        System.err
            .println("Couldn't find class for specified look and feel:"+lookAndFeel);
        System.err.println("Did you include the L&F library in the class path?");
        System.err.println("Using the default look and feel.");
      } catch(UnsupportedLookAndFeelException e)
      {
        System.err.println("Can't use the specified look and feel ("+lookAndFeel
            +") on this platform.");
        System.err.println("Using the default look and feel.");
      } catch(Exception e)
      {
        System.err.println("Couldn't get specified look and feel ("+lookAndFeel
            +"), for some reason.");
        System.err.println("Using the default look and feel.");
        e.printStackTrace();
      }
    }
  }
  
}
