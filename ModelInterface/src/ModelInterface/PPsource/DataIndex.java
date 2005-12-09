package ModelInterface.PPsource;

import java.util.Map;
import java.util.TreeMap;

public interface DataIndex
{
  public double getResolution();
  public void fillWorld(double res);
  public void addData(DataBlock val);
  public void addData(DataBlock val, boolean avg);
  public void resolveOverwrite(String holdName, String varName);
  public Map extractMask(RegionMask m);
}
