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
 * \file Variable.java
 * \ingroup DataManipulation
 * \brief User defined piece of data which can be manipulated.
 *
 *  The base of all variables. Stores data as an array of DataWrappers for efficency of
 * matrix operations. If all region info was aggregated into one matrix there would be
 * large black areas which do not need to be operated on. By storing regions data seperately
 * while working on it runtime is improved.
 *
 * \author Vincent Nibali
 * \date $Date: 2007-11-29 18:20:17 -0500 (Thu, 29 Nov 2007) $
 * \version $Revision: 3276 $
 */

package ModelInterface.DMsource;

import java.io.*;


/**
 * User defined piece of data which can be manipulated. The base of all variables. 
 * Stores data as an array of DataWrappers for efficency of
 * matrix operations. If all region info was aggregated into one matrix there would be
 * large black areas which do not need to be operated on. By storing regions data seperately
 * while working on it runtime is improved.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public abstract class Variable
{
  String name;	//the name of this variable, how it is accessed
  String comment; //if the user would like additional information printed with the variable
  
  //***************************************************************************
  //************************Constructors***************************************
  //***************************************************************************
  
  //***************************************************************************
  //******************Variable Functions***************************************
  //***************************************************************************
  public Variable getCopy()
  {
    return getCopy(name);
  }
  public boolean sameShape(Variable var)
  {
    if((isGroup() != var.isGroup())||(isReference() != var.isReference()))
    {
      return false;
    }
    Wrapper[] myData = getData();
    Wrapper[] testData = var.getData();
    
    if(myData.length != testData.length)
    {
      return false;
    }
    
    for(int i = 0; i < myData.length; i++)
    {
      if(myData[i].data.length != testData[i].data.length)
      {
        return false;
      }
      if(myData[i].data[0].length != testData[i].data[0].length)
      {
        return false;
      }
    }
    
    return true;
  }
  
  //***************************************************************************
  //******************Abstract Functions***************************************
  //***************************************************************************
  /**
   * Prints only the variable's data component to the screen. This does not print any
   * qualifying information, even the variables name. This function can be used
   * to print in the midst of other set output or user defined output. If a value does
   * not lie in the variables data even though it is in the rectangular bounds, NaN
   * will be printed.
   *
   */
  public abstract void printStandard(BufferedWriter out) throws IOException;
  /**
   * Prints the variables data with qualifying information. This will include at least
   * the name of the variable, but may also include bounding information and so forth.
   * If a value does not lie in the variables data even though it is in the rectangular
   * bounds, NaN will be printed.
   */
  public abstract void printVerbose(BufferedWriter out) throws IOException;
  public abstract Wrapper[] getData();
  public abstract Wrapper[] getWeight();
  public abstract Wrapper[] getLandFract();
  public abstract void setData(Wrapper[] d);
  public abstract boolean isReference();
  public abstract boolean isGroup();
  public abstract Variable getCopy(String n);
  public abstract Variable getShape(String n);
}

