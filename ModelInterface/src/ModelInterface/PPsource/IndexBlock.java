package ModelInterface.PPsource;

import java.awt.geom.Rectangle2D;
import java.util.TreeMap;

public class IndexBlock extends Rectangle2D.Double
{
  /*
   * seriously? i cant believe i am making this a different class than rect...
   */
  
  private int xIndex;
  private int yIndex;
  
  /**
   * Default Constuctor for a resolution 1 block at 0,0 with no data.
   */
  public IndexBlock()
  {
    x = 0;
    y = 0;
    height = 1;
    width = 1;
    
    xIndex = 0;
    yIndex = 0;
  }
  /**
   * Copy Constructor which will mimic the passed block in shape and location but NOT take its data.
   * @param copy DataBlock which supplies the information
   */
  public IndexBlock(Rectangle2D.Double copy)
  {
    x = copy.x;
    y = copy.y;
    height = copy.height;
    width = copy.width;
    
    xIndex = 0;
    yIndex = 0;
  }
  /**
   * Standard Constructor which defines the bounds of the new block.
   * 
   * @param X Lower-Left X coordinate.
   * @param Y Lower-Left Y coordinate.
   * @param H Height of the block.
   * @param W Width of the block.
   */
  public IndexBlock(double X, double Y, double H, double W)
  {
    x = X;
    y = Y;
    height = H;
    width = W;
    
    xIndex = (int)Math.floor(X/W);
    yIndex = (int)Math.floor(Y/H);
  }
  
//*****************************************************************************
//*************************Functions Proper************************************ 
//***************************************************************************** 

  public int getXIndex()
  {
    return xIndex;
  }
  public int getYIndex()
  {
    return yIndex;
  }
  
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
