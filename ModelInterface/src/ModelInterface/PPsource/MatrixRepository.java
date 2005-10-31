package ModelInterface.PPsource;

import java.util.*;

public class MatrixRepository implements DataRepository
{
  /*
   * implements as a treemap of vars -> treemap of times -> double[][]
   */
  TreeMap<String, TreeMap<Double, double[][]>> root;
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
      for(int i = 0; i < xSize; i ++)
        for(int k = 0; k < ySize; k++)
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
      System.out.println("SEVERE: ("+X+","+Y+") out of bounds - PROGRAM TERMINATING");
      System.exit(1);
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
        //this time didnt exist in what we are overwriting, cant merge them
        return -1;
      }
      holdTime = holdEntry.getValue();
      overwriteTime = overwrite.get(thisTime);
      
      //actually putting them together
      overwriteTime = overMerge(overwriteTime, holdTime);
      //replaciung the old info with this new info
      overwrite.put(thisTime, overwriteTime);
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

  private double[][] overMerge(double[][] oldData, double[][] newData)
  {
    if((oldData.length != newData.length)||(oldData[0].length != newData[0].length))
    {
      return null;
    }
    double[][] toReturn = oldData;
    
    for(int i = 0; i < oldData.length; i++)
    {
      for(int k = 0; k < oldData[0].length; k++)
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
