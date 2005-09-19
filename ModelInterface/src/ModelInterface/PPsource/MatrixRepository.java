package ModelInterface.PPsource;

import java.util.TreeMap;

public class MatrixRepository implements DataRepository
{
  /*
   * implements as a treemap of times -> treemap of vars -> double[][]
   */
  TreeMap<Double, TreeMap<String, double[][]>> root;
  double resolution;
  double[][] currLayer;
  int xSize;
  int ySize;
  
//*********************************************************
//*****************Class Constructors**********************
//********************************************************* 
  
  public MatrixRepository()
  {
    root = new TreeMap<Double, TreeMap<String, double[][]>>();
    resolution = 1;
    xSize = 360;
    ySize = 180;
  }
  public MatrixRepository(double res, int x, int y)
  {
    root = new TreeMap<Double, TreeMap<String, double[][]>>();
    resolution = res;
    xSize = x;
    ySize = y;
  }
  
//*********************************************************
//*************Begin Functions Proper**********************
//*********************************************************
  
  public void changeLayer(String varName, double time)
  {
    if(!root.containsKey(time))
    { //create time
      root.put(time, new TreeMap<String, double[][]>());
    }
    
    TreeMap<String, double[][]> inTime = root.get(time);
    if(!inTime.containsKey(varName))
    { //create this field matrix
      inTime.put(varName, new double[xSize][ySize]);
    }
    
    currLayer = inTime.get(varName);
  }
  public void createLayer(String varName, double time)
  {
    if(!root.containsKey(time))
    { //create time
      root.put(time, new TreeMap<String, double[][]>());
    }
    
    TreeMap<String, double[][]> inTime = root.get(time);
    if(!inTime.containsKey(varName))
    { //create this field matrix
      inTime.put(varName, new double[xSize][ySize]);
    }
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
    currLayer[X][Y] += value;
    
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


}
