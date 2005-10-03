package ModelInterface.PPsource;

import java.util.*;

public class MatrixRepository implements DataRepository
{
  /*
   * implements as a treemap of times -> treemap of vars -> double[][]
   */
  TreeMap<Double, TreeMap<String, double[][]>> root;
  double[][] currLayer;
  String currName;
  int xSize;
  int ySize;
  
//*********************************************************
//*****************Class Constructors**********************
//********************************************************* 
  
  public MatrixRepository()
  {
    root = new TreeMap<Double, TreeMap<String, double[][]>>();
    xSize = 360;
    ySize = 180;
    currName = "";
  }
  public MatrixRepository(int x, int y)
  {
    root = new TreeMap<Double, TreeMap<String, double[][]>>();
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
    if(!root.containsKey(time))
    { //create time
      root.put(time, new TreeMap<String, double[][]>());
    }
    
    TreeMap<String, double[][]> inTime = root.get(time);
    if(!inTime.containsKey(varName))
    { //create this field matrix
      double[][] newb = new double[xSize][ySize];
      //filling this new layer with NaN's
      for(int i = 0; i < xSize; i ++)
        for(int k = 0; k < ySize; k++)
          newb[i][k] = Double.NaN;
      
      inTime.put(varName, newb);
    }
    
    return inTime.get(varName);
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
    if(Double.isNaN(currLayer[X][Y]))
    {
      currLayer[X][Y] = value;
    } else
    {
      currLayer[X][Y] += value;
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

  public TreeMap<String, TreeMap<Double, Double>> getAllLayers(int X, int Y)
  {
    /*
     * this function will not return NaN values, they will be completly unreported
     * this is because a NaN represents something that was never changed
     * aka it was not in the data.
     */
    TreeMap<String, TreeMap<Double, Double>> toReturn = new TreeMap<String, TreeMap<Double, Double>>();
    TreeMap<Double, Double> holdVar;
    
    Map.Entry<Double, TreeMap<String, double[][]>> timeEntry;
    Map.Entry<String, double[][]> varEntry;
    Iterator<Map.Entry<String, double[][]>> iV;
    Iterator<Map.Entry<Double, TreeMap<String, double[][]>>> iT = root.entrySet().iterator();
    Double value;
    String varName;
    Double timeName;
    
    while(iT.hasNext())
    {
      timeEntry = iT.next();
      timeName = timeEntry.getKey();
      iV = timeEntry.getValue().entrySet().iterator();
      while(iV.hasNext())
      {
        varEntry = iV.next();
        varName = varEntry.getKey();
        value = varEntry.getValue()[X][Y];
        
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

}
