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
