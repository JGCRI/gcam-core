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

package ModelInterface.DMsource;

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
  
  List<Region> data; //a list of other stored regions, be they sub or super
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
    level = 1;
    data = new ArrayList<Region>();
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
      holdR = data.get(i);
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
            {
              /*TODO this is a logical problem, we are ignoring this weight value
                this is because we will have overlapping weights and we need them 
                to be distinct so that when we add boarder regions we dont get incorrect
                values this is currently working in the opposit way it should, should have
                2 weights of .5 each, when they are added they will equal a weight of 1 and fill the cell
                correctly, but are getting weights of liek 2.0 and cell values are something like 4x what 
                they should be
               */
              toReturn[(offsetY+iY)][(offsetX+iX)] += (holdM[iY][iX]);
            }
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
      holdR = data.get(i);
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
      holdR = data.get(i);
      holdD = holdR.getWorkingM(var, year);
      for(int k = 0; k < holdD.length; k++)
      {
        toReturn[currWrap] = holdD[k];
        currWrap++;
      }
    }
    
    return toReturn;
  }
  
  public ArrayList<String> getTimeList(String var)
  {
    return data.get(0).getTimeList(var);
  }
  
  public Wrapper[] extractRegion(ReferenceVariable ref)
  {
    Region currR;
    Wrapper[] holdAdd;
    ArrayList holdWrappers = new ArrayList();
    
    for(int i = 0; i < data.size(); i++)
    {
      currR = data.get(i);
      holdAdd = currR.extractRegion(ref);
      for(int k = 0; k < holdAdd.length; k++)
      {
        holdWrappers.add(holdAdd[k]);
      }
    }
    
    return (Wrapper[])holdWrappers.toArray(new Wrapper[0]);
  }

  public boolean containsRegion(String regionNameIn) {
	  boolean ret = name.equals(regionNameIn);
	  for(int i = 0; i < data.size() && !ret; ++i) {
		  Region currR = data.get(i);
		  ret = currR.containsRegion(regionNameIn);
	  }
	  return ret;
  }
}
