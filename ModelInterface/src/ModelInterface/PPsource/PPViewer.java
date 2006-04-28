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

package ModelInterface.PPsource;


import java.awt.*;
import java.awt.event.ActionEvent;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.event.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;
import java.util.Vector;

import ModelInterface.InterfaceMain;
import ModelInterface.MenuAdder;
import ModelInterface.InterfaceMain.MenuManager;
import ModelInterface.ModelGUI2.DOMPasteboard;
import ModelInterface.ModelGUI2.DOMmodel;

public class PPViewer implements MenuAdder
{
  public static String controlStr = "PPViewer";
  
  private JFrame parentFrame;

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
            //TODO recinding control of the interface Panel
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
          }
        }
      }
    });
  }
  
  
  public void addMenuItems(MenuManager menuMan)
  {
    // TODO Auto-generated method stub
    
  }
  
  /**
   * Process events from the menu items.
   * 
   * @param e the event, only care about a click on a menu item
   */
  public void actionPerformed(ActionEvent e)
  {//handle all button clicks
    
  }
  
//*********************My Private Functions************************************
  

}
