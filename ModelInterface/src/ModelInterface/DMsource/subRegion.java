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
 * \file subRegion.java
 * \ingroup DataManipulation
 * \brief Extension of Region which stored the actual data at the lowest level.
 *
 *  The atomic region. This region actually stores a matrix of information for its member
 * variables and times.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

package ModelInterface.DMsource;

import java.util.*;


import Jama.Matrix;

/**
 * Extension of Region which stored the actual data at the lowest level.
 * The atomic region. This region actually stores a matrix of information for its member
 * variables and times.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class subRegion extends Region
{
  TreeMap data;
  
  public subRegion()
  {
    name = "blank";
    resolution = 1;
    numSub = 1;
    level = 0;
    data = new TreeMap();
  }
  
  public boolean isSuper()
  {
    return false;
  }
  
  public double[][] getM()
  {
    return (double[][])((Map)data.get("weight")).get("0");
  }
  
  //returns the matrix of values for the specified variable during the specified year
  public double[][] getM(String var, String year)
  {
    Map holdVar = ((Map)data.get(var));
    
    if(holdVar.containsKey(year))
    { //user enterd a time which exists, return it
      return (double[][])holdVar.get(year);
    } else
    { //test if the user just forgot the .0 at the end of time, add for them
      if(holdVar.containsKey(year+".0"))
      {
        return (double[][])holdVar.get(year+".0");
      } else
      { //this time just straight up doesnt exist
        return null;
      }
    }
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
  
//returns a copy of this regions data for performing operations on as array of matrices of data
  public ReferenceWrapper[] getWorkingM(String var, String year)
  {
    ReferenceWrapper[] toReturn = new ReferenceWrapper[1];
    
    if(!data.containsKey(var))
    {
      toReturn[0] = new ReferenceWrapper(this);
      for(int i = 0; i < toReturn[0].data.length; i++)
        for(int k = 0; k < toReturn[0].data[0].length; k++)
          toReturn[0].data[i][k] = java.lang.Double.NaN;
      
    } else if(!((Map)data.get(var)).containsKey(year))
    {
      toReturn[0] = new ReferenceWrapper(this);
      for(int i = 0; i < toReturn[0].data.length; i++)
        for(int k = 0; k < toReturn[0].data[0].length; k++)
          toReturn[0].data[i][k] = java.lang.Double.NaN;
    } else
    {
      double[][] holdD = (double[][])((Map)data.get(var)).get(year);
      toReturn[0] = new ReferenceWrapper(this);
      
      for(int i = 0; i < holdD.length; i++)
        for(int k = 0; k < holdD[0].length; k++)
          toReturn[0].data[i][k] = holdD[i][k];
    }
    return toReturn;
  }
  /**
   * Gets the value of the passed var, at the passed time, at the passed location in the matrix.
   * @param var Variable you require data from.
   * @param year Time to get data from.
   * @param x X coordinate in the regions matrix of values.
   * @param y Y coordinate in the regions matrix of values.
   * @return value of data at the specified point.
   */
  public double get(String var, double year, int x, int y)
  {
    return ((Matrix)((Map)data.get(var)).get(String.valueOf(year))).get(x, y);
  }
  
  /**
   * Gets the value of the passed var, at the passed time, at the passed location in degrees.
   * @param var Variable you require data from.
   * @param year Time to get data from.
   * @param findX X coordinate in degrees latitude, longitude of the value.
   * @param findY Y coordinate in degrees latitude, longitude of the value.
   * @return value of data at the specified point.
   */
  public double getByDegree(String var, double year, double findX, double findY)
  {
    //i dunno if this math is right... but hey maybe it is!
    int X = (int)(Math.floor((findX-x)/resolution));
    int Y = (int)(Math.floor(((y+height)-findY)/resolution));
    return ((Matrix)((Map)data.get(var)).get(String.valueOf(year))).get(X, Y);
  }
  
  public ArrayList<String> getTimeList(String var)
  {
    Map.Entry e;
    ArrayList<String> toRet = new ArrayList<String>();
    Map times = (Map)data.get(var);
    Iterator it = times.entrySet().iterator();
    
    while(it.hasNext())
    {
      e = (Map.Entry)it.next();
      toRet.add((String)e.getKey());
    }
    
    return toRet;
  }
  
  public Wrapper[] extractRegion(ReferenceVariable ref)
  {
    Wrapper[] toRet = new Wrapper[1];
    Wrapper[] toSearch;
    
    toSearch = ref.getData();
    
    for(int i = 0; i < toSearch.length; i++)
    {
      if(((ReferenceWrapper)toSearch[i]).name.equals(name))
      {
        toRet[0] = toSearch[i];
        return toRet;
      }
    }
    
    return null;
  }
  
}
