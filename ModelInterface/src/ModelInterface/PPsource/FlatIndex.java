package ModelInterface.PPsource;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.logging.Logger;

public class FlatIndex implements DataIndex
{
  private DataRepository data; //this is where all data is stored and where we will get it from
  private double minX;
  private double maxX;
  private double minY;
  private double maxY;
  private boolean init;
  private TreeMap<String, TreeMap<String, TreeMap<Point2D.Double, Double>>> makeRegion;
  private Map<String, Map<Double, Double>> unAlteredSums;
  private boolean trackSums;
  
  public double resolution; //resolution of the data this index points to
  
//*********************************************************
//*****************Class Constructors**********************
//*********************************************************
  
  public FlatIndex()
  {
    //assumes we are using a globe
    minX = -180;
    maxX = 180;
    minY = -90;
    maxY = 90;
    resolution = -1;
    trackSums = false;
    unAlteredSums = new HashMap<String, Map<Double, Double>>();
  }
  
  public FlatIndex(double x1, double x2, double y1, double y2)
  {
    //dont really need this...
    minX = x1;
    maxX = x2;
    minY = y1;
    maxY = y2;
    resolution = -1;
    trackSums = false;
    unAlteredSums = new HashMap<String, Map<Double, Double>>();
  }
  
//*********************************************************
//*************Begin Functions Proper**********************
//*********************************************************
  
  public void setTrackSums(boolean trackSums) {
	  this.trackSums = trackSums;
  }

  public boolean getTrackSums() {
	  return trackSums;
  }

  /**
   * Print the gathered data sums to the screen if the trackSums flag has been set.
   * This could be useful in debugging where values are being lost.
   */
  public void printSums() {
	  // don't do anything if the flag has not been set
	  if(trackSums) {
		  final Logger log = Logger.getLogger("Preprocess"); //log class to use for all logging output
		  // use a buffer so that the logs don't get jumbled with the prints
		  final StringBuilder buff = new StringBuilder();
		  // print each variable
		  for(Iterator<Map.Entry<String, Map<Double, Double>>> itNames = unAlteredSums.entrySet().iterator(); 
				  itNames.hasNext(); ) {
			  Map.Entry<String, Map<Double, Double>> currVar = itNames.next();
			  buff.append("<variable name=\"").append(currVar.getKey()).append("\">\n");
			  // print each time and the sum for that time in this variable
			  for(Iterator<Map.Entry<Double, Double>> itTimes = currVar.getValue().entrySet().iterator(); 
					  itTimes.hasNext(); ) {
				  Map.Entry<Double, Double> currTime = itTimes.next();
				  log.info("Var: "+currVar.getKey()+" / Time: "+currTime.getKey()+" / Sum: "+currTime.getValue());
				  buff.append("\t<sum time=\"").append(currTime.getKey()).append("\">").append(currTime.getValue())
						  .append("</sum>\n");
			  }
			  buff.append("</variable>\n");
		  }
		  // use print instead of println because the buff will already have the \n in there
		  System.out.print(buff);
	  }
  }

  public double getResolution()
  {
    return resolution;
  }

  public void fillWorld(double res)
  {
    //doesnt actually fill anything as this is a representationless index
    //just sets resolution and moves on
    init(res);
  }

  public void addData(DataBlock val)
  {
    addData(val, true);
  }

  public void addData(DataBlock val, boolean avg)
  {
    Iterator i1, i2;
    Map.Entry vEntry, tEntry;
    Block indArea;
    Point2D.Double min, max;
    double weight;
    
    if(!init)
    {
      init(val.width);
    }

    addToTrackSum(val, avg);
    
    //System.out.println("block x: "+val.x+"-"+(val.x+val.width));
    //System.out.println("block y: "+val.y+"-"+(val.y+val.height));
    
    //find index bounds to work in
    min = point2index(new Point2D.Double(val.x, val.y), true);
    max = point2index(new Point2D.Double((val.x+val.width), (val.y+val.height)), false);

    //X, and Y are now indicies
    for(int Y = (int)min.y; Y < max.y; Y++)
    {
      for(int X = (int)min.x; X < max.x; X++)
      {
        //index area given location and resolution
        indArea = index2area(new Point2D.Double(X, Y));
        
        //find the weight value
        weight = getWeight(val, indArea, avg);
        
        //pass all that to data
        if(weight > 0)
        { 
          //then there is some overlap, add data in some way
          String varName;
          double weightValue, addValue, timeName;
          
          //should add each var and time (could be more than one)
          i1 = val.data.entrySet().iterator();
          while(i1.hasNext())
          { //for each variable...
            vEntry = (Map.Entry)i1.next();
            varName = (String)vEntry.getKey();
            TreeMap thisTime = (TreeMap)vEntry.getValue();
            
            i2 = thisTime.entrySet().iterator();
            while(i2.hasNext())
            { //for each time...
              tEntry = (Map.Entry)i2.next();
              timeName = (Double)tEntry.getKey();
              addValue = (Double)tEntry.getValue();
              
              weightValue = addValue*weight;
              
              data.addValue(varName, timeName, X, Y, weightValue);
            }
          }
        }
        //done adding data
      }
    }
  }

  public void resolveOverwrite(String holdName, String varName)
  {
    data.mergeHoldTo(holdName, varName);
  }

  public Map extractMask(RegionMask m)
  {
    Point2D.Double min, max;
    double[][] toMask;
    double weight;
    Block entry;
    if(m.name.equals("landFract")) {
	    return data.getLandFractPrintMap(resolution);
    }

    //identify rectangular bounds of region
    min = point2index(new Point2D.Double(m.x, m.y), true);
    max = point2index(new Point2D.Double((m.x+m.width+m.resolution), (m.y+m.height+m.resolution)), false);
    
    /*
    System.out.println("\n"+m.name);
    System.out.println("min X: "+(m.x)+" Y: "+(m.y));
    System.out.println("size X: "+(m.width)+" Y: "+(m.height));
    System.out.println("matrix X: "+(max.y-min.y)+" Y: "+(max.x-min.x));
    */
    if(((max.y-min.y) <= 0)||((max.x-min.x) <= 0))
    {
      return null;
    }
    toMask = new double[(int)(max.y-min.y)][(int)(max.x-min.x)];
    
    //X, and Y are now indicies
    //for each block of info in the repository
    for(int Y = (int)min.y; Y < max.y; Y++)
    {
      for(int X = (int)min.x; X < max.x; X++)
      {
        entry = index2area(new Point2D.Double(X, Y));
        weight = m.inRegion(entry.x, entry.y, entry.width, entry.height);
        toMask[(Y-(int)min.y)][(X-(int)min.x)] = weight;
      }
    }
    
    return data.getRegion((int)min.x, (int)max.y, toMask, m.x, (m.y+m.height+m.resolution), resolution); //returning data
  }
  
  /*
  public TreeMap extractMask(RegionMask m)
  {
    makeRegion = new TreeMap<String, TreeMap<String, TreeMap<Point2D.Double, Double>>>(); //setting up empty data structure to add to
    Point2D.Double min, max;

    //identify rectangular bounds of region
    min = point2index(new Point2D.Double(m.x, m.y), true);
    max = point2index(new Point2D.Double((m.x+m.width+m.resolution), (m.y+m.height+m.resolution)), false);
    
    //X, and Y are now indicies
    //for each block of info in the repository
    for(int Y = (int)min.y; Y < max.y; Y++)
    {
      for(int X = (int)min.x; X < max.x; X++)
      {
        extractBlock(new Point2D.Double(X, Y), m);
      }
    }
    
    return makeRegion; //returning data
  }
  */
  
//*********************************************************
//*************Begin Private Functions*********************
//*********************************************************
  
  private void init(double res)
  {
    resolution = res;
    init = true;
    //TODO maybe add an auto chooser based on resolution?
    //need to make disklayer faster first
    //data = new DiskLayerRepository((int)Math.floor((maxX-minX)/resolution), (int)Math.floor((maxY-minY)/resolution));
    data = new MatrixRepository((int)Math.floor((maxX-minX)/resolution), (int)Math.floor((maxY-minY)/resolution));
  }
  
  private Point2D.Double point2index(Point2D.Double p, boolean down)
  {
    //down is wether we should use floor or ceiling, wether we round down or up
    int x, y;//indexes are always ints
    
    //translate from a point in lat/lon to an index
    //possible given that we know resolution, which we do
    if(down)
    {
      x = (int)Math.floor(((p.x/resolution)+(180/resolution)));
      y = (int)Math.floor(((p.y/resolution)+(90/resolution)));
    } else
    {
      x = (int)Math.ceil(((p.x/resolution)+(180/resolution)));
      y = (int)Math.ceil(((p.y/resolution)+(90/resolution)));
    }
    
    return new Point2D.Double(x, y);
  }
  private Point2D.Double index2point(Point2D.Double i)
  {
    double x, y;
    
    //translate from index to point
    //no need to know wether we round up or down because index is always an
    //exact translation to lat/lon
    x = ((i.x-(180/resolution))*(resolution));
    y = ((i.y-(90/resolution))*(resolution));
    
    //return this index
    return new Point2D.Double(x, y);
  }
  private Block index2area(Point2D.Double i)
  {
    Point2D.Double p;
    //build an area given an index of said area and resolution
    //first get point corresponding to index
    p = index2point(i);
    //now return area from point
    return new Block(p.x, p.y, resolution, resolution);
  }
  private double getWeight(Block val, Block ind, boolean avg)
  {
    double weight = 0;
    double p1, p2;

    
    p1 = ind.getOverlap(val);
    p2 = val.getOverlap(ind);
    
    weight = p1;
    if(!avg)
    { //additive so have to account for limited addition of values (p2)
      weight *= p2;
    }
    
    return weight;
  }
  private void extractBlock(Point2D.Double i, RegionMask m)
  {
    //TreeMap<String, TreeMap<String, TreeMap<Point, Double>>>
    double weight;
    Block entry;
    TreeMap<String, TreeMap<Double, Double>> dataPoint;
    Point2D.Double addPoint;
    
    //check if part of region
    entry = index2area(i);
    weight = m.inRegion(entry.x, entry.y, entry.width, entry.height);
    
    //if so, add it
    if(weight > 0)
    {
      if(!makeRegion.containsKey("weight"))
      {
        //setting up the var and time for weight of blocks if not done yet
        TreeMap wTime = new TreeMap();
        wTime.put("0", new TreeMap(new coordComparePoint()));
        makeRegion.put("weight", wTime);
      }
      //adding a data point for the weight of this DB
      addPoint = new Point2D.Double(entry.x, entry.y);
      ((TreeMap)((TreeMap)makeRegion.get("weight")).get("0")).put(addPoint, new Double(weight));
      
      //*getting the data for this point from the data repository
      dataPoint = data.getAllLayers((int)i.x, (int)i.y);
      //*done getting data
      
      //add the data to the correct TreeMap (based on data name)
      //iterate through the data in this Node, by Var, then Time, adding to makeRegion
      Map.Entry<String, TreeMap<Double, Double>> var;
      Map.Entry<Double, Double> time;
      Iterator<Map.Entry<String, TreeMap<Double, Double>>> iV = dataPoint.entrySet().iterator();
      while(iV.hasNext())
      {
        //iterating through variables
        var = iV.next();
        if(!makeRegion.containsKey(var.getKey()))
        {//if makeRegion does not yet have a mapping for this variable, add it now
          makeRegion.put(var.getKey(), new TreeMap<String, TreeMap<Point2D.Double, Double>>());
        }
        Iterator<Map.Entry<Double, Double>> iT = var.getValue().entrySet().iterator();
        while(iT.hasNext())
        {
          time = iT.next();
          if(!((TreeMap)makeRegion.get(var.getKey())).containsKey(time.getKey()))
          {//if makeRegion's mapping for the variable does not contain this time yet, add it now
            ((TreeMap)makeRegion.get(var.getKey())).put(time.getKey(), new TreeMap(new coordComparePoint()));
          }
          //ok we can finally add the actual data as a (point, value) pair to lowest level treeMap
          addPoint = new Point2D.Double(entry.x, entry.y);
          ((TreeMap)((TreeMap)makeRegion.get(var.getKey())).get(time.getKey())).put(addPoint, time.getValue());
        }
      }
    }
  }

  /**
   * Takes all of the variables and time for a block and adds them to a world level sum.  If the val is
   * and avg (such as a percentage value) it will be multiplied by the area of the block before added to
   * the sum.  if trackSums has not been set this method will do nothing.
   * @param val The DataBlock that has all of the unaltered data values for a grid cell.
   * @param avg Whether this val would be averaged over cells or just split evenly.
   */
  private void addToTrackSum(DataBlock val, boolean avg) {
	  // only track the sums if the flag has been set
	  if(trackSums) {
		  // go through every variable name
		  for(Iterator<Map.Entry<String, TreeMap<Double, Double>>> itNames = val.data.entrySet().iterator(); 
				  itNames.hasNext(); ) {
			  Map.Entry<String, TreeMap<Double, Double>> currVar = itNames.next();
			  Map<Double, Double> sumVar = unAlteredSums.get(currVar.getKey());
			  // if this variable has not been summed yet add it to the map
			  if(sumVar == null) {
				  sumVar = new HashMap<Double, Double>();
				  unAlteredSums.put(currVar.getKey(), sumVar);
			  }
			  // go through all of the times
			  for(Iterator<Map.Entry<Double, Double>> itTimes = currVar.getValue().entrySet().iterator(); 
					  itTimes.hasNext(); ) {
				  Map.Entry<Double, Double> currTime = itTimes.next();
				  // only add the data if it is not NaN
				  if(!currTime.getValue().isNaN()) {
					  // get the current sum, if it has yet to be summed 
					  // set the initial to 0.0
					  Double currSum = sumVar.get(currTime.getKey());
					  if(currSum == null) {
						  currSum = new Double(0.0);
					  }
					  // if we have an avg val then we need to calculate the area
					  // of this cell and multiply the area with the val (which would
					  // probably be a %) then add it to the sum, otherwise we just need
					  // to add it to the sum
					  if(avg) {
						  currSum += (currTime.getValue() *
								  getArea(val));
					  } else {
						  currSum += currTime.getValue();
					  }
					  // have to put it back into the map because Double is 
					  // immutable
					  sumVar.put(currTime.getKey(), currSum);
				  }
			  }
		  }
	  }
  }

  /**
   * Calculates the earth surface area for the passed in block.  The x represents
   * the upper left longitude, y is the upper left latitude.  The height is in latitude
   * down from the upperleft, and width is the latitude right for the upper left.
   * @param block A rectage that represents a block of surface area on the earth all 
   * 	units for the block should be in latitude and longitude.
   * @return The surface area on the earth of the passed in block in Km^2.
   */
  private static double getArea(Rectangle2D.Double block) {
	  // TODO: maybe this should go somewhere else
	  final double POLAR_CIRCUM = 40008.00;
	  final double EQUAT_CIRCUM = 40076.5;
	  final double blockHeightKm = (POLAR_CIRCUM/(360/block.getHeight()));
	  final double circumAtLat = Math.abs(EQUAT_CIRCUM*Math.cos((block.getY())*(Math.PI/180)));
	  final double blockWidthKm = (circumAtLat/(360/block.getWidth()));
	  return blockHeightKm * blockWidthKm;
	  //double radiansS = (90.0 - (block.getY()+.25))*Math.PI/180;
	  /* This way seems to have trouble when res is 1.0 the calculated area is cut in half
	  double radiansS = (90.0 - (block.getY()+(block.getHeight()/2)))*Math.PI/180;
	  double cosinesS = Math.cos(radiansS) - Math.cos(radiansS+((block.getHeight()*Math.PI)/180));
	  double areaS = ((6371221.3*6371221.3)*Math.PI*cosinesS/360)*(.000001);
	  return areaS;
	  */
  }
  public static void main(String[] args) {
	  // TODO: remove this when done testing
	  FlatIndex thisInd = new FlatIndex();
	  Block ind = new Block(0, 0, 0.5, 0.5);
	  Block val = new Block(0, 0, 1.0, 1.0);
	  boolean avg = true;
	  System.out.println("Weight: "+thisInd.getWeight(ind, val, avg));
	  Rectangle2D.Double b1 = new Rectangle2D.Double(-172.5, -46.0, 0.5, 0.5);
	  Rectangle2D.Double b2 = new Rectangle2D.Double(172.5, 46.0, 0.5, 0.5);
	  System.out.println(b1+" -- "+getArea(b1));
	  System.out.println(b2+" -- "+getArea(b2));
	  Rectangle2D.Double currBlock = new Rectangle2D.Double();
	  double sum = 0.0;
	  for(double y = 90.0; y > -90.0; y-=0.5) {
		  for(double x = -180.0; x < 180.0; x+=0.5) {
			  currBlock.setRect(x, y, 0.5, 0.5);
			  sum += getArea(currBlock);
		  }
	  }
	  System.out.println("World surface area when using 0.5 res: "+sum);
	  sum = 0.0;
	  for(double y = 90.0; y > -90.0; y-=1.0) {
		  for(double x = -180.0; x < 180.0; x+=1.0) {
			  currBlock.setRect(x, y, 1.0, 1.0);
			  sum += getArea(currBlock);
		  }
	  }
	  System.out.println("World surface area when using 1.0 res: "+sum);
  }
}
