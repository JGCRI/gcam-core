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
/*
 * this class just contains the overlap functions all blocks are going to want
 * oh
 * and a constructor, just for fun
 */


package ModelInterface.PPsource;

import java.awt.geom.*;

public class Block extends Rectangle2D.Double
{
  public Block()
  {}
  public Block(double X, double Y, double H, double W)
  {
    x = X;
    y = Y;
    height = H;
    width = W;
  }
  
//*****************************************************************************
//*************************Functions Proper************************************ 
//***************************************************************************** 

  /**
   * The proportion of THIS block which overlaps the supplied block.
   * 
   * @param target Block to test against.
   * @return Decimal portion (0-1) of overlap.
   */
  public double getOverlap(Rectangle2D.Double target)
  {
    /*
     * this function returns the decimal of THIS object which intersects with TARGET object
     * used in aggregating data between resolutions
     */
    Rectangle2D.Double hold = new Rectangle2D.Double();
    Rectangle2D.Double.intersect(this, target, hold);
    //returning the ratio of areas
    return (hold.height*hold.width)/(height*width);
  }
  /**
   * Creates a block with the supplied parameters and tests for the proportion of overlap
   * between the two.
   * @param x Lower-Left X coordinate to test against.
   * @param y Lower-Left Y coordinate to test against.
   * @param h Height of the region to test against.
   * @param w Width of the region to test against.
   * @return Decimal portion (0-1) of overlap.
   */
  public double getOverlap(double x, double y, double h, double w)
  {
     /*
      * this function returns the decimal of THIS object which intersects with created object
      * based on passed in parameters
      * used in aggregating data between resolutions
      */
    Rectangle2D.Double target = new Rectangle2D.Double(x, y, w, h);
    Rectangle2D.Double hold = new Rectangle2D.Double();
    Rectangle2D.Double.intersect(this, target, hold);
    //returning the ratio of areas
    return (hold.height*hold.width)/(height*width);
  }
}
