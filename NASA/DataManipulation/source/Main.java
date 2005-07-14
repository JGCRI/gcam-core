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
 * \file Main.java
 * \ingroup DataManipulation
 * \brief Main class for data manipulation.
 *
 *  A wrapper type class which just reads in the files to be used in the ManipulationDriver
 * and then calls the running functions.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
/**
 * MASTER TODO LIST: *currently RUNNING and CORRECT but INCOMPLETE*
 * TODO: add in variableInfo reading fucntions, way to account for units? references? ...other stuff
 * TODO: comment private functions
 */
package source;

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
      log.setLevel(Level.parse(logLevel));
      log.setUseParentHandlers(false);
      
      cHand = new ConsoleHandler();
      cHand.setLevel(Level.WARNING);
      log.addHandler(cHand);
      fHand = new FileHandler("DataManip.log");
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
      log.log(Level.SEVERE, "IOException! for shame! in main function");
    }
  }
}
