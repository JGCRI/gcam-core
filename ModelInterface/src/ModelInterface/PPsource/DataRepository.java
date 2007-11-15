package ModelInterface.PPsource;

import java.awt.geom.Point2D;
import java.util.*;

public interface DataRepository
{
  /*
   * there is a time of 0 which is for any untimed values
   */
  /*
   * ok so waht can this do... need to be abel to get data from a specific block
   * set data for a specific block, maybe get size of layer, res of blocks
   */
  
  public void changeLayer(String varName, double time);
  public double[][] createLayer(String varName, double time);
  public double[][] getLayer(String varName, double time);
  
  public void setValue(int X, int Y, double value);
  public void setValue(String varName, double time, int X, int Y, double value);
  
  public void addValue(int X, int Y, double value);
  public void addValue(String varName, double time, int X, int Y, double value);
  
  public double getValue(int X, int Y);
  public double getValue(String varName, double time, int X, int Y);
  
  public int mergeHoldTo(String holdName, String varName);
  
  public TreeMap<String, TreeMap<Double, Double>> getAllLayers(int X, int Y);
  
  public Map<String, Map<String, Map<Point2D.Double, Double>>> getRegion(int X, int Y, double[][] weights, double xL, double yL, double res);

  public Map<String, Map<String, Map<Point2D.Double, Double>>> getLandFractPrintMap(double resolution);
}
