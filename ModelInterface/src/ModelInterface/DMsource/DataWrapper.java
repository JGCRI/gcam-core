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
 * \file DataWrapper.java
 * \ingroup DataManipulation
 * \brief The basic unit of information to be manipulated, can be just numbers or a region.
 *
 * This class was necessitated by the desire to make all actual manipulation functions as
 * standardized as possible. All data is put in an array of these classes and operated on
 * as such.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package ModelInterface.DMsource;

import java.io.*;


/**
 * The basic unit of information to be manipulated, can be just numbers or a region.
 * This class was necessitated by the desire to make all actual manipulation functions as
 * standardized as possible. All data is put in an array of these classes and operated on
 * as such.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class DataWrapper extends Wrapper
{

  //***************************************************************************
  
  /**
   * Default constuctor for a DataWrapper. Assumes this is strict data, not
   * a region reference.
   *
   */
  public  DataWrapper()
  {
    
  }
  /**
   * Copy Constructor which copies data.
   * @param copy DataWrapper whos information will be copied.
   */
  public DataWrapper(DataWrapper copy)
  {
    data = copy.data;
  }
  
  //***************************************************************************
  
  public Wrapper makeCopy()
  {
    return new DataWrapper(this);
  }
  
  public double[][] getData()
  {
    return data;
  }
  
  public void setData(double[][] newData)
  {
    data = newData;
  }
  
  public double getX()
  {
    return -1;
  }
  
  public void setX(double X)
  {}
  
  public double getY()
  {
    return -1;
  }
  
  public void setY(double Y)
  {}
  
  public double getH()
  {
    return -1;
  }
  
  public void setH(double H)
  {}
  
  public double getW()
  {
    return -1;
  }
  
  public void setW(double W)
  {}
  
  public double getRes()
  {
    return -1;
  }
  
  public void setRes(double R)
  {}
  
  public boolean isData()
  {
    return true;
  }
  
  public void printStandard(BufferedWriter out) throws IOException
  { 
    for(int i = 0; i < data.length; i++)
    {
      for(int k = 0; k < (data[0].length-1); k++)
      {
        out.write(data[i][k]+",");
      }
      out.write(data[i][data[0].length-1]+"");
      out.newLine();
    }
  }

  public double getInternalRegionID(int level) {
	  // does not make sense for a data wrapper
	  return -1;
  }
}
