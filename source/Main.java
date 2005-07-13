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
      
      ManipulationDriver mainRun = new ManipulationDriver(dSource, rSource, cSource);
      mainRun.runAll();
      
    } catch(FileNotFoundException e)
    {
      System.out.println("FileNotFound! DMfiles.xml does not exist");
    } catch(JDOMException e)
    {
      System.out.println("JDOM Exception! in main function");
    }
    catch(IOException e)
    {
      System.out.println("IOException! for shame! in main function");
    }
    
    //((Region)bigTest.regionList.get("Alaska")).printToBits();
    //((Region)bigTest.regionList.get("USAcontiguous")).printToBits();
    //((Region)bigTest.regionList.get("USA")).printToBits();
  }
}
