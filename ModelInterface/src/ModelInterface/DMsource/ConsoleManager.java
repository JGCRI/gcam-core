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
 * \file ConsoleManager.java
 * \ingroup DataManipulation
 * \brief Class for constructing and accessing of Console objects.
 *
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

/**
 * TODO: everything... get and add specifically
 */
package ModelInterface.DMsource;

import java.lang.ref.*;
import java.util.*;

public class ConsoleManager
{
  /*
   * Singleton ConsoleManager instance.
   */
  private static ConsoleManager consoleManager;
  
  private Map consoles;
  
  
  
//*****************************************************************************
//****************************Constructor**************************************
//*****************************************************************************
  
  protected ConsoleManager()
  {
    if(consoleManager != null)
      throw new IllegalStateException("there can be only one ConsoleManager; use get method");
    
    
    consoleManager = this;
    consoles = new HashMap();
  }
  
//*****************************************************************************
//****************************Functions****************************************
//*****************************************************************************
  
  public static ConsoleManager getConsoleManager()
  {
    if(consoleManager == null)
      consoleManager = new ConsoleManager();
    
    return consoleManager;
  }
  
/**
   * Returns a Console given its name.
   *
   * @param name the name of the Console.
   *
   * @return a named Console, or <code>null</code> if there is no
   *     console with that name.
   *
   * @throw java.lang.NullPointerException if <code>name</code>
   *     is <code>null</code>.
   */
  public Console getConsole(String name)
  {
    WeakReference ref;

    /* Throw a NullPointerException if name is null. */
    name.getClass();

    ref = (WeakReference)consoles.get(name);
    if(ref!=null)
    {
      Console hold = (Console)ref.get();
      if(hold != null)
      {
        return (Console)ref.get();
      } else
      {
        return null;
      }
    }
    else
      return null;
  }
  
  public boolean addConsole(Console toAdd)
  {
    String name;
    WeakReference ref;
    
    name = toAdd.getName();
    ref = (WeakReference)consoles.get(name);
    
    if(ref == null)
    {
      consoles.put(name, new WeakReference(toAdd));

      return true;
    } else
    { //oh noes this console already exists!!!!!!!
      Console hold = (Console)ref.get();
      if(hold == null)
      {
        consoles.put(name, new WeakReference(toAdd));
        return true;
      } else
      {
        return false;
      }
    }
  }
  
  public void removeConsole(String toRemove)
  {
    consoles.remove(toRemove);
  }
}
