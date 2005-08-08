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
 * \file RegionMask.java
 * \ingroup Preprocess
 * \brief Definition of a region as bounding rectangle and bitmask.
 *
 *  This class defines a region. A region consists of its x and y coordinates (in
 * latitude and longitude) its height and width, the resolution of data it contains
 * and a bit mask to define for each resolution sized block wether or not it is
 * contained in the region.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

/* DEVELOPER COMMENTS
 * contain a bounding box, resolution, bitmask for a region
 * this will be used to create an actual region object, to determine which data it should contain
 */
package PPsource;

import java.awt.geom.*;

/**
 * Definition of a region as bounding rectangle and bitmask. 
 * A region consists of its x and y coordinates (in latitude and longitude)
 * its height and width, the resolution of data it contains
 * and a bit mask to define for each resolution sized block wether or not it is
 * contained in the region.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class RegionMask extends Rectangle2D.Double
{
  public String name; //the name of the region this is a mask for
  public byte  bMask[][]; //a bitmask which defines whether or not each point is in the region
  public double resolution; //how much space each bit represents
  //Rec2D already contains x, y, w, h so i wont store the min's and max's here...
  /**
   * Default Constructor. Creates an impossible rectangle with negative bounds.
   */
  public RegionMask()
  {
    name = "noName";
    resolution = 1;
    x = 0;
    y = 0;
    width = -1;
    height = -1;
  }
  /**
   * Basic Constructor for name and resolution. The bounds of the rectangle are set to
   * impossible values to force later setting to correct values.
   * 
   * @param n The name of the region this is a mask for.
   * @param r The resolution of the bitMask this class holds.
   */
  public RegionMask(String n, double r)
  {
    name = n;
    resolution = r;
    x = 0;
    y = 0;
    width = -1;
    height = -1;
  }
  /**
   * Gets the portion of the passed rectangle which overlaps this region.
   * Checks bounds of rectangle vs bounds of region, then checks agains the
   * bitmask of the region itself.
   * 
   * @param X X-value of the bottom left-most point in a rectangle.
   * @param Y Y-value of the bottom left-most point in a rectangle.
   * @param W Width of the rectangle.
   * @param H Height of the rectangle.
   * @return Portion of the passed rectangle which overlaps this region.
   */
  public double inRegion(double X, double Y, double W, double H)
  {
    //pretty self explanitory, checks if any part of the sent rectangle is in this specific region
    //know it intersects in some way, now find if it additionally lays over a
    //region of the mask which contains some 1's, return portion which is in region
    double toReturn = 0;
    double portion;
    Rectangle2D.Double thisRect, passRect;
    int lowX, highX, lowY, highY; //the range of indicies to check in the mask
    double smallX, bigX, smallY, bigY; //used for proportion finding
    double thisX, thisY; //x and y coordinates for the current place in bit mask
    passRect = new Rectangle2D.Double(X, Y, W, H);
    lowX = (int)(Math.floor(((X-x))/resolution));
    highX = (int)(Math.ceil((((X+(W-resolution))-x))/resolution));
    lowY = (int)(Math.floor((((y+height)-(Y+(H))))/resolution));
    highY = (int)(Math.ceil((((y+height)-(Y+resolution)))/resolution));
    for(int i = (lowY); i<=(highY); i++)
    {
      if((i<bMask.length)&&(i>=0))
      {//assuring no out of bounds checking
        for(int k = (lowX); k<=(highX); k++)
        {
          if((k<bMask[i].length)&&(k>=0))
          {//assuring no out of bounds checking
            if(bMask[i][k]==1)
            { //this portion of checked area lies in block, add it to the blocks weight
              thisX = (x+(k*resolution));
              thisY = (y+height-((i+1)*resolution));
              
              if(thisX > X)
                smallX = thisX;
              else
                smallX = X;
              if(thisY > Y)
                smallY = thisY;
              else
                smallY = Y;
              
              if((thisX+resolution) < (X+W))
                bigX = (thisX+resolution);
              else
                bigX = (X+W);
              if((thisY+resolution) < (Y+H))
                bigY = (thisY+resolution);
              else
                bigY = (Y+H);
              
              toReturn += (((bigX-smallX)*(bigY-smallY))/(W*H));
            }
          }
        }
      }
    } //return the amount of overlap (DB of mask)
    return toReturn;
  }
  /**
   * Creates an empty bitmask matrix using the regions height, width, and resolution.
   *
   */
  public void makeMatrix()
  {
    if((height != -1)&&(width != -1))
      bMask = new byte[(int)(height/resolution)][(int)(width/resolution)];
  }
  /**
   * Sets the given coordinates to true in the bit mask.
   * 
   * @param findX X-coordinate (longitude) of the point to set true.
   * @param findY Y-coordinate (latitude) of the point to set true.
   */
  public void setPointTrue(double findX, double findY)
  {
    int X = (int)(Math.floor((findX-x)/resolution));
    int Y = (int)(Math.floor(((y+height)-(findY+resolution))/resolution));
    bMask[Y][X] = 1;
  }
  /**
   * Prints to standard out the bitmask for this region as well as its bounds.
   * The bitmask is printed as a matrix of 1's and 0's.
   *
   */
  public void printMask()
  {
    System.out.println("Printing BitMask for: "+name);
    System.out.println("bounds: (x) "+x+" to "+(x+width)+" by (y) "+(y+height)+" to "+y);
    for(int i = 0; i < bMask.length; i++)
    {
      for(int k = 0; k < bMask[i].length; k++)
      {
        System.out.print(bMask[i][k]);
      }
      System.out.println();
    }
  }
}

