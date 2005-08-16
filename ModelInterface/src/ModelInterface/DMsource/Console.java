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
 * \file Console.java
 * \ingroup DataManipulation
 * \brief Class for outputting simple text to the screen.
 *
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package ModelInterface.DMsource;


public class Console
{
  private String name;
//*****************************************************************************
//****************************Constructor**************************************
//*****************************************************************************
  
  protected Console(String n)
  {
    name = n;
  }
  
//*****************************************************************************
//****************************Functions****************************************
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
  
}
