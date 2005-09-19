package ModelInterface.PPsource;

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
  public void createLayer(String varName, double time);
  public double[][] getLayer(String varName, double time);
  
  public void setValue(int X, int Y, double value);
  public void setValue(String varName, double time, int X, int Y, double value);
  
  public void addValue(int X, int Y, double value);
  public void addValue(String varName, double time, int X, int Y, double value);
  
  public double getValue(int X, int Y);
  public double getValue(String varName, double time, int X, int Y);
}
