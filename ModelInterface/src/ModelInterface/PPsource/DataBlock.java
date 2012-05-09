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
 * \file DataBlock.java
 * \ingroup Preprocess
 * \brief Storage class for all data in the QuadBucketTree as a Rectangle2D with data elements.
 *
 *  All data in the quadbuckettree is stored in these blocks. Blocks represent a smallest resolution
 * area of earth. Data is contained as a list of variables, and for each variable a list of times
 * at which values for it exist.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

/* DEVELOPER COMMENTS
 * This is the basic building block of information in this program
 * a matrix of datablocks will make up a region,
 * they will store all possible variables on as small a resolution as possible
 */
package ModelInterface.PPsource;

import java.util.*;

/**
 * Storage class for all data in the QuadBucketTree as a Rectangle2D with data elements.
 * All data in the quadbuckettree is stored in these blocks. Blocks represent a smallest resolution
 * area of earth. Data is contained as a list of variables, and for each variable a list of times
 * at which values for it exist.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class DataBlock extends Block
{
  //string, object pairs describing what the data is, and then the corresponding value - ("CO2", 1452)
  public TreeMap<String, TreeMap<java.lang.Double, java.lang.Double>> data;
  //x - the longitude of the block in degrees (degrees W are negative)
  //y - the latitude of the block in degrees (degrees S are negative)
  //height - from y going N
  //width - from x going E
  
  /**
   * Default Constuctor for a resolution 1 block at 0,0 with no data.
   */
  public DataBlock()
  {
    x = 0;
    y = 0;
    height = 1;
    width = 1;
    data = new TreeMap<String, TreeMap<java.lang.Double, java.lang.Double>>();
  }
  /**
   * Copy Constructor which will mimic the passed block in shape and location but NOT take its data.
   * @param copy DataBlock which supplies the information
   */
  public DataBlock(DataBlock copy)
  {
    x = copy.x;
    y = copy.y;
    height = copy.height;
    width = copy.width;
    data = new TreeMap<String, TreeMap<java.lang.Double, java.lang.Double>>(); //dont copy the data cuz i dont need it
  }
  /**
   * Standard Constructor which defines the bounds of the new block.
   * 
   * @param X Lower-Left X coordinate.
   * @param Y Lower-Left Y coordinate.
   * @param H Height of the block.
   * @param W Width of the block.
   */
  public DataBlock(double X, double Y, double H, double W)
  {
    x = X;
    y = Y;
    height = H;
    width = W;
    data = new TreeMap<String, TreeMap<java.lang.Double, java.lang.Double>>();
  }
}
