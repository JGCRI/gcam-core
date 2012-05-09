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
    
   // Add small number so won't round down due to numerical error. Results should be integer, so won't change result otherwise.
    xIndex = (int)Math.floor(((X/W)+(180/W) + W/10 ));
    yIndex = (int)Math.floor(((Y/H)+(90/H) + H/10 ));

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
