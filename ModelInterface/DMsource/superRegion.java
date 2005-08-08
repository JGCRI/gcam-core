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
 * \file superRegion.java
 * \ingroup DataManipulation
 * \brief Extension of Region which stores higher level regions, those defined by other regions.
 *
 *  A higher level Region. This can store a list of subregions, or even other superRegions,
 * which it aggregates on command. Functions generally call the corresponding function
 * in all contained regions down to the lowest level then all return.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package DMsource;

import java.util.*;



/**
 * Extension of Region which stores higher level regions, those defined by other regions.
 * A higher level Region. This can store a list of subregions, or even other superRegions,
 * which it aggregates on command. Functions generally call the corresponding function
 * in all contained regions down to the lowest level then all return.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class superRegion extends Region
{
  
  ArrayList data; //a list of other stored regions, be they sub or super
  /**
   * Constucts a blank super region with imposible bounds all fields must
   * be set later by the user. Setting of bounds and filling of data should
   * happen as new subregions are added to this regions list.
   *
   */
  public superRegion()
  {
    name = "blank";
    resolution = 1;
    x = 180;
    y = 90;
    height = -1;
    width = -1;
    numSub = 0;
    data = new ArrayList();
  }
  
  public boolean isSuper()
  {
    return true;
  }
  
  public double[][] getM() //getM returns the weight matrix
  { //does not weight return as it is returning weight (not weight^2)
    Region holdR;
    int offsetY, offsetX;
    double[][] holdM;
    double[][] holdW;
    double[][] toReturn = new double[(int)(height/resolution)][(int)(width/resolution)];
    for(int i = 0; i < toReturn.length; i++)
    {
      for(int k = 0; k < toReturn[i].length; k++)
      {
        toReturn[i][k] = 0;
      }
    }
    
    for(int i = 0; i < data.size(); i++)
    {
      holdR = (Region)data.get(i);
      holdM = holdR.getM();
      offsetY = (int)(((y+height)-(holdR.y+holdR.height))/resolution);
      offsetX = (int)((holdR.x-x)/resolution);
      for(int iY = 0; iY < holdM.length; iY++)
      {
        for(int iX = 0; iX < holdM[iY].length; iX++)
        {
          if(!java.lang.Double.isNaN(holdM[iY][iX]))
          {
            if(java.lang.Double.isNaN(toReturn[(offsetY+iY)][(offsetX+iX)]))
              toReturn[(offsetY+iY)][(offsetX+iX)] = (holdM[iY][iX]);
            else
              toReturn[(offsetY+iY)][(offsetX+iX)] += (holdM[iY][iX]);
          }
        }
      }
    }
    return toReturn;
  }
  
  public double[][] getM(String var, String year)
  {
    Region holdR;
    int offsetY, offsetX;
    double[][] holdM, holdW;
    double[][] toReturn = new double[(int)(height/resolution)][(int)(width/resolution)];
    for(int i = 0; i < toReturn.length; i++)
    {
      for(int k = 0; k < toReturn[i].length; k++)
      {
        toReturn[i][k] = java.lang.Double.NaN;
      }
    }
    
    for(int i = 0; i < data.size(); i++)
    {
      holdR = (Region)data.get(i);
      holdM = holdR.getM(var, year);
      holdW = holdR.getM();
      offsetY = (int)(((y+height)-(holdR.y+holdR.height))/resolution);
      offsetX = (int)((holdR.x-x)/resolution);
      for(int iY = 0; iY < holdM.length; iY++)
      {
        for(int iX = 0; iX < holdM[i].length; iX++)
        {
          if(!java.lang.Double.isNaN(holdM[iY][iX]))
          {
            if(java.lang.Double.isNaN(toReturn[(offsetY+iY)][(offsetX+iX)]))
              toReturn[(offsetY+iY)][(offsetX+iX)] = (holdM[iY][iX]*holdW[iY][iX]);
            else
              toReturn[(offsetY+iY)][(offsetX+iX)] += (holdM[iY][iX]*holdW[iY][iX]);
          }
        }
      }
    }
    return toReturn;
  }
  
  public byte[][] getBitMask()
  {
    byte[][] toReturn;
    double[][] w = getM();
    toReturn = new byte[w.length][w[0].length];
    for(int i = 0; i < w.length; i++)
      for(int k = 0; k < w[0].length; k++)
        if(w[i][k] != 0)
          toReturn[i][k] = 1;
    
    return toReturn;
  }
  
  public ReferenceWrapper[] getWorkingM(String var, String year)
  {
    Region holdR;
    ReferenceWrapper[] holdD;
    int currWrap = 0;
    ReferenceWrapper[] toReturn = new ReferenceWrapper[numSub];
    
    //?what is this doing? toReturn[0] = new DataWrapper(name, resolution, x, y, width, height);
    for(int i = 0; i < data.size(); i++)
    {
      holdR = (Region)data.get(i);
      holdD = holdR.getWorkingM(var, year);
      for(int k = 0; k < holdD.length; k++)
      {
        toReturn[currWrap] = holdD[k];
        currWrap++;
      }
    }
    
    return toReturn;
  }
  
  public String[] getTimeList(String var)
  {
    return ((Region)data.get(0)).getTimeList(var);
  }
  
  public Wrapper[] extractRegion(ReferenceVariable ref)
  {
    Region currR;
    Wrapper[] holdAdd;
    ArrayList holdWrappers = new ArrayList();
    
    for(int i = 0; i < data.size(); i++)
    {
      currR = (Region)data.get(i);
      holdAdd = currR.extractRegion(ref);
      for(int k = 0; k < holdAdd.length; k++)
      {
        holdWrappers.add(holdAdd[k]);
      }
    }
    
    return (Wrapper[])holdWrappers.toArray(new Wrapper[0]);
  }
}
