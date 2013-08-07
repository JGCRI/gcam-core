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
 * \date $Date: 2009-12-08 16:48:48 -0500 (Tue, 08 Dec 2009) $
 * \version $Revision: 3638 $
 */
package ModelInterface.DMsource;

import java.io.*;

public abstract class Wrapper
{
  double[][] data;
  
  //***************************************************************************
  
  
  //***************************************************************************
  
  public abstract Wrapper makeCopy();
  
  public double[][] getData()
  {
    return data;
  }
  
  public void setData(double[][] newData)
  {
    data = newData;
  }
  
  public abstract double getX();
  
  public abstract void setX(double X);
  
  public abstract double getY();
  
  public abstract void setY(double Y);
  
  public abstract double getH();
  
  public abstract void setH(double H);
  
  public abstract double getW();
  
  public abstract void setW(double W);
  
  public abstract double getRes();
  
  public abstract void setRes(double R);
  
  public abstract boolean isData();
  
  public abstract void printStandard(BufferedWriter out) throws IOException;

  /**
   * Get the internal region ID for the wrapped data at the given level.  If
   * this wrapper does not have an associated reigon then -1 will be returned.
   * @param level The level at which to get the region ID.
   * @return The internal reigon ID or -1 if not applicable.
   * @see Region.getInternalID
   */
  public abstract double getInternalRegionID(int level);
}
