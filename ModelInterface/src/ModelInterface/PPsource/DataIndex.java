package ModelInterface.PPsource;

import java.util.TreeMap;

public interface DataIndex
{
  public void fillWorld(double res);
  public void addData(DataBlock val);
  public TreeMap extractMask(RegionMask m);
}
