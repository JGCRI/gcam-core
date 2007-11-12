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
