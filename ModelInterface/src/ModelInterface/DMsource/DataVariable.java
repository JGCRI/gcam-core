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
 * \date $Date$
 * \version $Revision$
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

