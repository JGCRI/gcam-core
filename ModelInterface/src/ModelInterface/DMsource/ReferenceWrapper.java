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
 * \file *file*
 * \ingroup *project*
 * \brief *brief description*
 *
 *  Detailed description.
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
public class ReferenceWrapper extends Wrapper
{
  String name;
  double res;
  byte[][] mask;
  double x;
  double y;
  double height;
  double width;
  
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
    data = new double[(int)(height/res)][(int)(width/res)];
    mask = r.getBitMask();
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
      System.out.print("\t");
      for(int k = 0; k < data[0].length; k++)
      {
        if(Double.isNaN(data[i][k]))
          out.write("NaN ");
        else
          out.write(data[i][k]+" ");
      }
      out.newLine();
    }
  }
}
