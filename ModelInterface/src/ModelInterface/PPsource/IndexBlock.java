package ModelInterface.PPsource;

import java.awt.geom.Rectangle2D;

public class IndexBlock extends Block
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
    
    xIndex = (int)Math.floor(((X/W)+(180/W)));
    yIndex = (int)Math.floor(((Y/H)+(90/H)));
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
}
