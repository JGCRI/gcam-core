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
 * \file QBNode.java
 * \ingroup Preprocess
 * \brief Node class for the QuadBucketTree as a 2D area.
 *
 *  A node for the QuadBucketTree. This is essentially a rectangle2D object
 * though it does nto extend that class. It has its own intersects and
 * contains functions as these are not closed on any side.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
package ModelInterface.PPsource;


import java.util.*;

/*
 * stole intersects function from java.awt.geom.Rectangle2D
 */

/**
 * Node class for the QuadBucketTree as a 2D area. This is essentially a rectangle2D object
 * though it does nto extend that class. It has its own intersects and
 * contains functions as these are not closed on any side.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class QBNode
{
  public boolean leaf;
  public ArrayList data;
  public double minX;
  public double centerX;
  public double maxX;
  public double minY;
  public double centerY;
  public double maxY;
  public QBNode parent;
  
  /**
   * Default Constructor. If used the bounds MUST be set manually.
   * @deprecated does not ensure the node is ready to be used.
   */
  public QBNode()
  {
    leaf = true;
    parent = null;
    data = new ArrayList();
  }
  /**
   * Creates a new node which defaults to leaf and has the passed bounds.
   * 
   * @param x1 Smallest X bound for the node.
   * @param x2 Largest X bound for the node.
   * @param y1 Smallest Y bound for the node.
   * @param y2 Largest Y bound for the node.
   */
  public QBNode(double x1, double x2, double y1, double y2)
  {
    minX = x1;
    maxX = x2;
    centerX = (minX+maxX)/2;
    minY = y1;
    maxY = y2;
    centerY = (minY+maxY)/2;
    leaf = true;
    parent = null;
    data = new ArrayList();
  }
  /**
   * Tests whether or not the passed rectangle intersects this node, ALL sides are closed.
   * This can be used in a recursive search algorithm to determine which nodes to search in.
   * 
   * @param x Lower-Left X coordinate.
   * @param y Lower-Left Y coordinate.
   * @param w Width of the rectangle.
   * @param h Height of the rectangle.
   * @return Whether of not the intersection occured.
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    //open bounded, does this rectangle intersect this node, used for recursive searching
    double mx = minX;
    double my = minY;
    double mw = maxX-minX;
    double mh = maxY-minY;
    return w>0&&h>0&&mw>0&&mh>0&&x<mx+mw&&x+w>mx&&y<my+mh&&y+h>my;
  }
  /**
   * Much like {@link intersects} but dealing with point data. Once again this
   * is closed on all sides.
   * @param x X-coordinate to check.
   * @param y Y-coordinate to check.
   * @return Whether of not this node contained the point.
   */
  public boolean contains(double x, double y)
  {
    //this is not closed on any side, the point must be IN the bounds, not just on them
    return x>minX&&y>minY&&x<maxX&&y<maxY;
  }

}
