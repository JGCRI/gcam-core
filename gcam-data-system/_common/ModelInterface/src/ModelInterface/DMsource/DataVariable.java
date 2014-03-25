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
 * \file DataVariable.java
 * \ingroup DataManipulation
 * \brief Extension of Variable which stores some matrix of numbers from 0-2 dimensions.
 *
 *  This is a differentiation from the usual ReferenceVariable so that simple number
 * data could be stored as such. Print functions are different as there is no name or
 * location information to print. This information is also not included in the class
 * to save space, especially on the weighting mask.
 *
 * \author Vincent Nibali
 * \date $Date: 2007-11-29 18:20:17 -0500 (Thu, 29 Nov 2007) $
 * \version $Revision: 3276 $
 */

package ModelInterface.DMsource;

import java.io.*;


/**
 * Extension of Variable which stores some matrix of numbers from 0-2 dimensions.
 * This is a differentiation from the usual ReferenceVariable so that simple number
 * data could be stored as such. Print functions are different as there is no name or
 * location information to print. This information is also not included in the class
 * to save space, especially on the weighting mask.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class DataVariable extends Variable
{
  int dims;	//number of dimensions of the data (i might nto need this, but we'll see)
  Wrapper[] data;	//the data this variable holds
  
  //***************************************************************************
  //************************Constructors***************************************
  //***************************************************************************
  
  /**
   * Default Constructor for a data variable.
   *
   */
  public DataVariable()
  {
    name = "null";
    comment = null;
  }
  
  public DataVariable(String n)
  {
    name = n;
    comment = null;
  }
  /**
   * Point data variable constuctor. Takes a supplied name and a scalar
   * data value.
   * @param n Name of this variable.
   * @param d Data this variable will hold.
   */
  public DataVariable(String n, double d)
  {
    dims = 0;
    name = n;
    comment = null;
    data = new DataWrapper[1];
    data[0] = new DataWrapper();
    data[0].data = new double[1][1];
    data[0].data[0][0] = d;
  }
  /**
   * One dimensional array data variable constuctor. Takes supplied name and an
   * array of values.
   * @param n Name of this variable.
   * @param d Data this variable will hold.
   */
  public DataVariable(String n, double d[])
  {
    dims = 1;
    name = n;
    comment = null;
    data = new DataWrapper[1];
    data[0] = new DataWrapper();
    data[0].data = new double[1][];
    data[0].data[0] = d;
  }
  /**
   * Two dimensional matrix data variable constuctor. Takes supplied name and a
   * matrix of values.
   * @param n Name of this variable.
   * @param d Data this variable will hold.
   */
  public DataVariable(String n, double d[][])
  {
    dims = 2;
    name = n;
    comment = null;
    data = new DataWrapper[1];
    data[0] = new DataWrapper();
    data[0].data = d;
  }
  /**
   * Data copy constuctor for data variable. Takes data already in working format
   * and copies it.
   * @param n Name of this variable.
   * @param d Data this variable will hold.
   */
  public DataVariable(String n, Wrapper[] d)
  {
    name = n;
    comment = null;
    data = d;
    if(d[0].data.length <= 1)
      dims = 0;
    else if(d[0].data[0].length <= 1)
      dims = 1;
    else
      dims = 2;
  }
  
  //***************************************************************************
  //******************Variable Functions***************************************
  //***************************************************************************
  
  public void printStandard(BufferedWriter out) throws IOException
  {
    for(int i = 0; i < data.length; i++)
    {
      data[i].printStandard(out);
    }
    out.flush();
  }
  public void printVerbose(BufferedWriter out) throws IOException
  {
    out.write(name+":\n");
    if(comment != null)
    {
      out.write("\tDescription: "+comment);
      out.newLine();
    }
    for(int i = 0; i < data.length; i++)
    {
      data[i].printStandard(out);
    }
    out.flush();
  }
  public Wrapper[] getData()
  {
    return data;
  }
  public Wrapper[] getWeight() {
	  // TODO: maybe return a 1 wrapper with data 1x1 matrix with 1?
	  throw new UnsupportedOperationException();
  }
  public Wrapper[] getLandFract() {
	  // TODO: maybe return a 1 wrapper with data 1x1 matrix with 1?
	  throw new UnsupportedOperationException();
  }
  public void setData(Wrapper[] d)
  {
    data = d;
  }
  public boolean isReference()
  {
    return false;
  }
  public boolean isGroup()
  {
    return false;
  }
  public Variable getCopy(String n)
  {
    return new DataVariable(n, data);
  }
  public Variable getShape(String n)
  {
    return new DataVariable(n);
  }
  
  //***************************************************************************
  //******************Personal Functions***************************************
  //***************************************************************************
  
  //***************************************************************************
}

