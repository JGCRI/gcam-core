/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
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
  
  public Map<String, Map<String, Map<Point2D.Double, Double>>> getRegion(int X, int Y, double[][] weights, double xL, double yL, double res, RegionMask mask);

  public Map<String, Map<String, Map<Point2D.Double, Double>>> getLandFractPrintMap(double resolution);
}
