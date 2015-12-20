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
 * \file Main.java
 * \ingroup DataManipulation
 * \brief Main class for data manipulation.
 *
 *  A wrapper type class which just reads in the files to be used in the ManipulationDriver
 * and then calls the running functions.
 *
 * \author Vincent Nibali
 * \date $Date: 2006-06-06 10:08:40 -0400 (Tue, 06 Jun 2006) $
 * \version $Revision: 2726 $
 */
/**
 * MASTER TODO LIST: *currently RUNNING and CORRECT but INCOMPLETE*
 * TODO: more error checking!
 * TODO: expand general output options
 * TODO: comment private functions
 */
package ModelInterface.DMsource;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.*;

import org.jdom.*;
import org.jdom.input.*;


/**
 * Main class for data manipulation. A wrapper type class which just reads in the files
 *  to be used in the ManipulationDriver and then calls the running functions.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class Main
{

  public static void main(String[] args)
  {
    Document in;
    Element root, currFile;
    String dSource, rSource, cSource;
    Logger log = Logger.getLogger("DataManipulation");
    Handler fHand, cHand;
    String logLevel;
    try
    {
      SAXBuilder builder = new SAXBuilder();
      in = builder.build("DMfiles.xml");
      root = in.getRootElement();
      currFile = root.getChild("data");
      dSource = currFile.getAttributeValue("file");
      currFile = root.getChild("region");
      rSource = currFile.getAttributeValue("file");
      currFile = root.getChild("command");
      cSource = currFile.getAttributeValue("file");
      
      //***setting up a logger for DataManipulator
      currFile = root.getChild("log");
      if(currFile == null)
      { //no given logging level, set to lowest output amount (regular use)
        logLevel = "WARNING";
      } else
      { //given a logging level, use that
        logLevel = currFile.getAttributeValue("level");
      }
      log.setLevel(Level.ALL);
      log.setUseParentHandlers(false);
      
      cHand = new ConsoleHandler();
      cHand.setLevel(Level.parse(logLevel));
      log.addHandler(cHand);
      fHand = new FileHandler("DMLog.log");
      fHand.setLevel(Level.ALL);
      fHand.setFormatter(new SimpleFormatter());
      log.addHandler(fHand);
      //***done initing DM logger
     
      //***********************Making Program Work*****************************
      log.log(Level.FINE, "Creating the ManipulationDriver to run program");
      ManipulationDriver mainRun = new ManipulationDriver(dSource, rSource, cSource);
      log.log(Level.FINE, "Calling main MD's runall() function");
      mainRun.runAll();
      //***********************************************************************
    } catch(FileNotFoundException e)
    {
      log.log(Level.SEVERE, "FileNotFound! DMfiles.xml does not exist");
    } catch(JDOMException e)
    {
      log.log(Level.SEVERE, "JDOM Exception! in main function");
    }catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException! in main function");
    }
    
    log.log(Level.INFO, "data manipulator completed running");
  }
}
