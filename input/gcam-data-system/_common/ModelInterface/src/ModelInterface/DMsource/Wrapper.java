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
