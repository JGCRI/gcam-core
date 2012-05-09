/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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
