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
 * \file Region.java
 * \ingroup DataManipulation
 * \brief Abstract definition of a region, be it containing other regions or blocks of data.
 *
 *  Basic region definition. This lays out the data access functions which all regions have.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package ModelInterface.DMsource;

/*
 * A region is composed of a matrix of datablocks or a group of regions
 * it contains information on what values are present in its children
 * a number of regions may be the children of another region (called a superRegion)
 */

/*
 * stores data in a ridiculous way: 
 * TreeMap - on variables (CO2, coal, forests, whatever) of TreeMaps
 * TreeMap - on year of matricies	- years are stored as String.valueOf(double)'s so as to allow multiple layers per year
 * Matrix - of values
 * exp. data.get("CO2").get(2000)[0][25]
 */


import java.awt.geom.*;
import java.util.ArrayList;



/**
 * Abstract definition of a region, be it containing other regions or blocks of data.
 * Basic region definition. This lays out the data access functions which all regions have.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public abstract class Region extends Rectangle2D.Double
{
  public String name; //the name of the region ("maryland" etc.) if one exists
  public double resolution; //how large each block of the matricies are
  public int numSub; //number of sub regions this region contains will normally be 0
  public int level; //the level of this region as defined in the early XML file
  
  public abstract boolean isSuper();
  /**
   * Gets some Matrix for this region. The actual matrix returned is the
   * weight matrix, which can be used as a bit mask, or when aggragating up.
   * 
   * @return Weight Matrix for this region.
   */
  public abstract double[][] getM();
  /**
   * Gets the specified matrix of data values from this region.
   * @param var Variable you would like to extract.
   * @param year Time you would like to extract for.
   * @return Matrix of double values corresponding to this regions data.
   */
  public abstract double[][] getM(String var, String year);
  /**
   * Gets the specified data values as a working set of {@link DataWrappers} which
   * are the basic manipulation block. Subregions are not aggregated together so
   * that all operations can be performed on the lowest possible level.
   * @param var Variable you would like to extract.
   * @param year Time you would like to extract for.
   * @return Array of DataWrappers each one of which correspond to a lowest level region of data.
   */
  public abstract ReferenceWrapper[] getWorkingM(String var, String year);
  /**
   * Gets a bit mask which defines for each point in the regions bounds whether
   * or not it is in the actual region. This mask is aggregated up from any
   * possible component regions to form one total matrix.
   * @return Bitmask of this region.
   */
  public abstract byte[][] getBitMask();
  /**
   * Gets a list of all the times contained in this region for a specific field.
   * @param var Field for which to get all time entries.
   * @return Array of strings each of which correspond to a time which has an entry.
   */
  public abstract ArrayList<String> getTimeList(String var);
  /**
   * Gets a region's datalist from the supplied variable. This is used to
   * get a sub region of data from a variable which spans a containing region.
   * @param ref Variable whos data will be parsed out.
   * @return Data in this regions shape.
   */
  public abstract Wrapper[] extractRegion(ReferenceVariable ref); 
  /**
   * Prints this regions bit mask to the screen as a matrix of 0's (not in region)
   * and 1's (in region). Additionally gives some bounds for the region.
   *
   */
  public void printToBits()
  {
    System.out.println("Printing BitMask for: "+name);
    System.out.println("bounds: (x) "+x+" to "+(x+width)+" by (y) "+(y+height)+" to "+y);
    double mask[][];
    mask = getM();
    for(int i = 0; i < mask.length; i++)
    {
      for(int k = 0; k < mask[i].length; k++)
      {
        if(mask[i][k] != java.lang.Double.NaN)
          System.out.print("1");
        else
          System.out.print("0");
      }
      System.out.print("\n");
    }
  }
}
