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
 * \file ConsoleManager.java
 * \ingroup DataManipulation
 * \brief Class for constructing and accessing of Console objects.
 *
 *
 * \author Vincent Nibali
 * \date $Date: 2005-08-25 08:58:17 -0400 (Thu, 25 Aug 2005) $
 * \version $Revision: 2277 $
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
