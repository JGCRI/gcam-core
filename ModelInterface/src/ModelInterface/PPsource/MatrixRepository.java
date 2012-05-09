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

import java.util.logging.*;
import java.awt.geom.Point2D;
import java.util.*;

public class MatrixRepository implements DataRepository
{
  /*
   * implements as a treemap of vars -> treemap of times -> double[][]
   */
  TreeMap<String, TreeMap<Double, double[][]>> root;
  Logger log = Logger.getLogger("Preprocess"); //log class to use for all logging output

  double[][] currLayer;

  String currName;

  int xSize;

  int ySize;

  //*********************************************************
  //*****************Class Constructors**********************
  //********************************************************* 

  public MatrixRepository()
  {
    root = new TreeMap<String, TreeMap<Double, double[][]>>();
    xSize = 360;
    ySize = 180;
    currName = "";
  }

  public MatrixRepository(int x, int y)
  {
    root = new TreeMap<String, TreeMap<Double, double[][]>>();
    xSize = x;
    ySize = y;
    currName = "";
  }

  //*********************************************************
  //*************Begin Functions Proper**********************
  //*********************************************************

  public void changeLayer(String varName, double time)
  {
    if(!currName.equals((varName+time)))
    {
      double[][] thisLayer = createLayer(varName, time);

      currLayer = thisLayer;
      currName = (varName+time);
    }
  }

  public double[][] createLayer(String varName, double time)
  {
    if(!root.containsKey(varName))
    { //create variable
      root.put(varName, new TreeMap<Double, double[][]>());
    }

    TreeMap<Double, double[][]> inVar = root.get(varName);
    if(!inVar.containsKey(time))
    { //create this field matrix
      double[][] newb = new double[xSize][ySize];
      //filling this new layer with NaN's
      for(int i = 0; i<xSize; i++)
        for(int k = 0; k<ySize; k++)
        {
          newb[i][k] = Double.NaN;
        }

      inVar.put(time, newb);
    }

    return inVar.get(time);
  }

  public double[][] getLayer(String varName, double time)
  {
    changeLayer(varName, time);

    return currLayer;
  }

  public void setValue(int X, int Y, double value)
  {
    currLayer[X][Y] = value;
  }

  public void setValue(String varName, double time, int X, int Y, double value)
  {
    changeLayer(varName, time);

    setValue(X, Y, value);
  }

  public void addValue(int X, int Y, double value)
  {
    try
    {
      if(Double.isNaN(currLayer[X][Y]))
      {
        currLayer[X][Y] = value;
      } else
      {
        currLayer[X][Y] += value;
      }
    } catch(ArrayIndexOutOfBoundsException e)
    {
      log.log(Level.WARNING, "("+X+","+Y+") out of bounds("+xSize+","+ySize
          +") with value: "+value);
      //System.exit(1);
    }

  }

  public void addValue(String varName, double time, int X, int Y, double value)
  {
    changeLayer(varName, time);

    addValue(X, Y, value);
  }

  public double getValue(int X, int Y)
  {
    return currLayer[X][Y];
  }

  public double getValue(String varName, double time, int X, int Y)
  {
    changeLayer(varName, time);

    return getValue(X, Y);
  }

  public int mergeHoldTo(String holdName, String varName)
  {
    //this will take whatever information is in the hold layer and merge it
    //with the designated variable
    //**this function can do multiple times at once, each time SHOULD align
    //**with a time in the variable (time == time)
    Map.Entry<Double, double[][]> holdEntry;
    Iterator<Map.Entry<Double, double[][]>> iH;
    Double thisTime;
    TreeMap<Double, double[][]> hold;
    TreeMap<Double, double[][]> overwrite;
    double[][] holdTime;
    double[][] overwriteTime;

    //make sure hold exists and the var to overwrite exists
    if((!root.containsKey(holdName))||(!root.containsKey(varName)))
    {
      return -1;
    }
    hold = root.get(holdName);
    overwrite = root.get(varName);

    //iterate through each time in hold
    iH = hold.entrySet().iterator();
    while(iH.hasNext())
    {
      holdEntry = iH.next();
      thisTime = holdEntry.getKey();
      if(!overwrite.containsKey(thisTime))
      {
	      // new time so just add the time
	      overwrite.put(thisTime, holdEntry.getValue());
      } else {
	      // really need to merge data
	      holdTime = holdEntry.getValue();
	      overwriteTime = overwrite.get(thisTime);

	      //actually putting them together
	      overwriteTime = overMerge(overwriteTime, holdTime);
	      //replaciung the old info with this new info
	      overwrite.put(thisTime, overwriteTime);
      }
    }

    //we have completely put hold's data in the passed var
    //remove hold
    root.remove(holdName);

    return 1; //success
  }

  public TreeMap<String, TreeMap<Double, Double>> getAllLayers(int X, int Y)
  {
    /*
     * this function will not return NaN values, they will be completly unreported
     * this is because a NaN represents something that was never changed
     * aka it was not in the data.
     */
    TreeMap<String, TreeMap<Double, Double>> toReturn = new TreeMap<String, TreeMap<Double, Double>>();
    TreeMap<Double, Double> holdVar;

    Map.Entry<String, TreeMap<Double, double[][]>> varEntry;
    Map.Entry<Double, double[][]> timeEntry;
    Iterator<Map.Entry<Double, double[][]>> iT;
    Iterator<Map.Entry<String, TreeMap<Double, double[][]>>> iV = root.entrySet().iterator();
    Double value;
    String varName;
    Double timeName;

    while(iV.hasNext())
    {
      varEntry = iV.next();
      varName = varEntry.getKey();
      iT = varEntry.getValue().entrySet().iterator();
      while(iT.hasNext())
      {
        timeEntry = iT.next();
        timeName = timeEntry.getKey();
        value = timeEntry.getValue()[X][Y];

        if(!Double.isNaN(value))
        {
          if(!toReturn.containsKey(varName))
          {
            toReturn.put(varName, new TreeMap<Double, Double>());
          }
          holdVar = toReturn.get(varName);

          holdVar.put(timeName, value);
        }
      }
    }

    return toReturn;
  }

  /**
   * Key routine that takes data from global data layers and transforms to 
   * regional data blocks. Also inserts the passed-in regional weight information, and -- if data
   * is available, calculates revised cell land-area.
   *
   */
  public Map<String, Map<String, Map<Point2D.Double, Double>>> getRegion(int X, int Y,
      double[][] weights, double xL, double yL, double res, RegionMask mask)
  {
    /*
     * X and Y are the top left corner
     */
    Map<String, Map<String, Map<Point2D.Double, Double>>> toReturn = new LinkedHashMap<String, Map<String, Map<Point2D.Double, Double>>>();
    Map<String, Map<Point2D.Double, Double>> holdVar;
    Map<Point2D.Double, Double> holdTime;
    Map.Entry<String, TreeMap<Double, double[][]>> varEntry;
    Map.Entry<Double, double[][]> timeEntry;
    Iterator<Map.Entry<Double, double[][]>> iT;
    Iterator<Map.Entry<String, TreeMap<Double, double[][]>>> iV = root.entrySet().iterator();
    String varName;
    Double timeName;
    double currXL, currYL;
    double[][] landFraction = root.get(DataBuilder.LAND_FRACTION).get(0.0); //Binary mask of land fractions
    double[][] landFractionDetail = root.get(DataBuilder.LAND_FRACTION).get(0.0); //Just so var is defined
     
    // Optional data that contains fraction of land within each cell
	if ( root.containsKey(DataBuilder.CELL_AREA_DATA) ) {
   		landFractionDetail = root.get(DataBuilder.CELL_AREA_DATA).get(0.0); // must set time in PP input to zero for this to work
    }
    
    // Put weight information into regional data variable. 
    toReturn.put("weight", new LinkedHashMap<String, Map<Point2D.Double, Double>>());
    holdVar = toReturn.get("weight");
    holdVar.put("0", new LinkedHashMap<Point2D.Double, Double>());
    holdTime = holdVar.get("0");
    currXL = xL;
    boolean isGlobalRegion = mask.name.equals(DataBuilder.GLOBAL_REGION_MASK);
    for(int x = 0; x<(weights[0].length); x++)
    {
      currYL = (yL-res);
      for(int y = (weights.length-1); y>=0; y--) // invert y coordinate
      {
        if(weights[(y)][(x)]>0)
        {
          //add weight
          Point2D.Double hold = new Point2D.Double(currXL, currYL);
          // Adjust weight for land-fraction.  Land fraction at this point counts how much much of 
          // each cell is in any region. Weight should be the split of each cell within some region.
          // This assures that sum of weights is = 1, even at water boundaries.
          try {
		  if(!isGlobalRegion) {
			 if ( landFraction[x+X][Y-((weights.length)-y)] > 0 ) {
			 	holdTime.put( hold, Double.valueOf(weights[(y)][(x)] / landFraction[x+X][Y-((weights.length)-y)])); 
			 } else {
				 log.log(Level.WARNING, "Inconsistent weight and land fraction value at ("+currXL+","+currYL+") ");
			 }
		  } else {
			  holdTime.put(hold, weights[y][x]);
		  }
		  } catch(ArrayIndexOutOfBoundsException e) {
			  System.out.println(e+" -- weight: "+weights[(y)][(x)]);
			  System.out.println("     indicies: ("+x+","+y+")  ("+(x+X)+","+(Y-((weights.length)-y))+")");
		  }
        }
        currYL -= res;
      }
      currXL += res;
    }

    while(iV.hasNext())
    {
      varEntry = iV.next();
      varName = varEntry.getKey();
      // Don't write out CELL_AREA_DATA as an explicit variable. 
      // It is incorporated into landFract below.
      // TODO: merge CELL_AREA_DATA into the exludes list
      if(varName.equals(DataBuilder.CELL_AREA_DATA) || mask.shouldExcludeVariable(varName)) {
	      continue;
      }

      //some variables do not exist in root.entrySet by this point
      //System.out.println(varName);
      toReturn.put(varName, new LinkedHashMap<String, Map<Point2D.Double, Double>>());
      holdVar = toReturn.get(varName);
      iT = varEntry.getValue().entrySet().iterator();
      while(iT.hasNext())
      {
        timeEntry = iT.next();
        timeName = timeEntry.getKey();
        holdVar.put(timeName.toString(), new LinkedHashMap<Point2D.Double, Double>());
        holdTime = holdVar.get(timeName.toString());
	boolean doAllOnes = isGlobalRegion && varName.equals(DataBuilder.LAND_FRACTION);

        changeLayer(varName, timeName);

        currXL = xL;
        for(int x = 0; x<(weights[0].length); x++)
        {
          currYL = (yL-res);
          for(int y = (weights.length-1); y>=0; y--)
          {
          	// Smallest weight if using 2.5 minute mask should be 2.5/60/res. So 1e-4 is ok.
            if( weights[(y)][(x)] > 1e-4 )
            {
	          // Adjust landfract by land area data if available 
	          // Landfract up until now is just fraction of native resolution grid 
	          // cells in the working resolution cell. This adjusts for actual land
	          // area within each cell, if this data was read-in.
			  double landFractionAdj = 1;
			  if ( varEntry.getKey().equals(DataBuilder.LAND_FRACTION) && root.containsKey(DataBuilder.CELL_AREA_DATA) ) {
  			    // This is ratio of read-in land area in cell to cell size. 
			    // Except for differences in assumed earth size, etc. this should always be < 1
		        landFractionAdj = landFractionDetail[x+X][Y-((weights.length)-y)] /
		      						   CoordConversions.area( res, res, currYL + res/2 );
			    // Correct for difference in resolution. 
			    // Cell size scale is res/native res larger than working res cell.
			    landFractionAdj = landFractionAdj *  (res * res) /
			          (DataBuilder.landFractionNativeResolution*DataBuilder.landFractionNativeResolution);
				  }

              //add this point's value to toReturn
              double dataValue = !doAllOnes ? currLayer[x+X][Y-((weights.length)-y)] * landFractionAdj : 1.0;
              holdTime.put(new Point2D.Double(currXL, currYL), dataValue );
            }
            currYL -= res;
          }
          currXL += res;
        }
      }
    }

    return toReturn;
  }

  public Map<String, Map<String, Map<Point2D.Double, Double>>> getLandFractPrintMap(double res) {
    Map<String, Map<String, Map<Point2D.Double, Double>>> toReturn = 
	    new LinkedHashMap<String, Map<String, Map<Point2D.Double, Double>>>(1);
    toReturn.put("weight", new LinkedHashMap<String, Map<Point2D.Double, Double>>());
    Map<String, Map<Point2D.Double, Double>> holdVar = toReturn.get("weight");
    holdVar.put("0", new LinkedHashMap<Point2D.Double, Double>(1));
    Map<Point2D.Double, Double> holdTime = holdVar.get("0");
    double[][] landFractValues = root.get(DataBuilder.LAND_FRACTION).get(0.0);
    double currXL = -180.0;
    double yL = 90.0;
    double currYL;
    for(int x = 0; x<(landFractValues.length); x++)
    {
      currYL = (yL-res);
      for(int y = (landFractValues[0].length-1); y>=0; y--)
      {
        if(landFractValues[(x)][(y)]>0)
        {
          //add fraction
          Point2D.Double hold = new Point2D.Double(currXL, currYL);
          holdTime.put(hold, Double.valueOf(landFractValues[(x)][(y)]));
        }
        currYL -= res;
      }
      currXL += res;
    }
    return toReturn;
  }

  //*********************************************************
  //*************Begin Private Functions*********************
  //*********************************************************

  private double[][] overMerge(double[][] oldData, double[][] newData)
  {
    if((oldData.length!=newData.length)||(oldData[0].length!=newData[0].length))
    {
      return null;
    }
    double[][] toReturn = oldData;

    for(int i = 0; i<oldData.length; i++)
    {
      for(int k = 0; k<oldData[0].length; k++)
      {
        if(!Double.isNaN(newData[i][k]))
        {
          //then we use the new value, otherwise just keep old value
          toReturn[i][k] = newData[i][k];
        }
      }
    }

    //will now have a mesh of new and old values (new wherever they exist)
    return toReturn;
  }

}
