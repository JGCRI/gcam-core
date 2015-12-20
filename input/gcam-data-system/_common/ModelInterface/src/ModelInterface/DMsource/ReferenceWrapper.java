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
 * \file *file*
 * \ingroup *project*
 * \brief *brief description*
 *
 *  Detailed description.
 *
 * \author Vincent Nibali
 * \date $Date: 2009-12-08 16:48:48 -0500 (Tue, 08 Dec 2009) $
 * \version $Revision: 3638 $
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
public class ReferenceWrapper extends Wrapper
{
  String name;
  double res;
  byte[][] mask;
  double x;
  double y;
  double height;
  double width;
  Region wrappedRegion;
  
  //***************************************************************************
  
  /**
   * Default constuctor for a ReferenceWrapper. Assumes this is strict data, not
   * a region reference.
   *
   */
  public  ReferenceWrapper()
  {
    name = null;
    res = 1;
    x = 0;
    y = 0;
    height = 0;
    width = 0;
    wrappedRegion = null;
  }
  /**
   * Copy Constructor which does NOT take data only size information.
   * @param copy ReferenceWrapper whos information will be copied.
   */
  public  ReferenceWrapper(ReferenceWrapper copy)
  {
    name = copy.name;
    res = copy.res;
    x = copy.x;
    y = copy.y;
    height = copy.height;
    width = copy.width;
    mask = copy.mask;
    wrappedRegion = copy.wrappedRegion;
    //doesnt copy data, initializes it?
  }
  /**
   * Region-based DataWrapper constructor. Gets bounding information from the 
   * passed region including bitmask. Data is not filled but allocated to the
   * correct size.
   * @param r Region whos values will construct this DataWrapper.
   */
  public  ReferenceWrapper(Region r)
  { //need to use this one so we can get a bitmask
    name = r.name;
    res = r.resolution;
    x = r.x;
    y= r.y;
    height = r.height;
    width = r.width;
    data = new double[(int)Math.round(height/res)][(int)Math.round(width/res)];
    mask = r.getBitMask();
    wrappedRegion = r;
  }
  /**
   * Simulated region-based DataWrapper constuctor. Passed values give the
   * approximation of a region for the allocation of data space but the
   * actual region and its bitmask are not passed.
   * @param n Name of the region.
   * @param r Resolution of the region.
   * @param X Lower-Left X coordinate.
   * @param Y Lower-Left Y coordinate.
   * @param W Width of the region.
   * @param H Height of the region.
   */
  public  ReferenceWrapper(String n, double r, double X, double Y, double W, double H)
  {
    name = n;
    res = r;
    x = X;
    y = Y;
    height = H;
    width = W;
    data = new double[(int)(H/res)][(int)(W/res)];
    mask = new byte[(int)(H/res)][(int)(W/res)];
    // don't really have a region
    wrappedRegion = null;
  }
  
  //***************************************************************************
  
  public Wrapper makeCopy()
  {
    return new ReferenceWrapper(this);
  }
  
  public boolean isData()
  {
    return false;
  }
  
  public double getX()
  {
    return x;
  }
  
  public void setX(double X)
  {
    x = X;
  }
  
  public double getY()
  {
    return y;
  }
  
  public void setY(double Y)
  {
    y = Y;
  }
  
  public double getH()
  {
    return height;
  }
  
  public void setH(double H)
  {
    height = H;
  }
  
  public double getW()
  {
    return width;
  }
  
  public void setW(double W)
  {
    width = W;
  }
  
  public double getRes()
  {
    return res;
  }
  
  public void setRes(double R)
  {
    res = R;
  }
  
  public void printStandard(BufferedWriter out) throws IOException
  { 
    for(int i = 0; i < data.length; i++)
    {
      for(int k = 0; k < (data[0].length-1); k++)
      {
        if(Double.isNaN(data[i][k]))
          out.write("NaN,");
        else
          out.write(data[i][k]+",");
      }
      if(Double.isNaN(data[i][data[0].length-1]))
        out.write("NaN");
      else
        out.write(data[i][data[0].length-1]+"");
      out.newLine();
    }
  }

  public double getInternalRegionID(int level) {
	  return wrappedRegion != null ? wrappedRegion.getInternalID(level) : -1;
  }
}
