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

import java.awt.geom.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class TreeIndex implements DataIndex
{
  private DataRepository data; //this is where all data is stored and where we will get it from
  private QBNode root; //root node, might wanna hold on to this one
  private boolean init; //has this index been initialized? (if not must fillWorld)
  private TreeMap makeRegion;
  private double minX;
  private double maxX;
  private double minY;
  private double maxY;
  
  public double resolution; //resolution of the DataBlocks stored in teh tree
  
//*********************************************************
//*****************Class Constructors**********************
//*********************************************************
  public TreeIndex(double x1, double x2, double y1, double y2)
  {
    minX = x1;
    maxX = x2;
    double centerX = (x1+x2)/2;
    minY = y1;
    maxY = y2;
    double centerY = (y1+y2)/2;
    init = false;
    
    root = new QBNode(x1, x2, y1, y2);
    root.leaf = false;
    //defining the 4 initial quadrants - in an annoying way
    QBNode toAdd = new QBNode(minX, centerX, minY, centerY);
    toAdd.parent = root;
    root.data.add(0, toAdd);
    toAdd = new QBNode(minX, centerX, centerY, maxY);
    toAdd.parent = root;
    root.data.add(1, toAdd);
    toAdd = new QBNode(centerX, maxX, centerY, maxY);
    toAdd.parent = root;
    root.data.add(2, toAdd);
    toAdd = new QBNode(centerX, maxX, minY, centerY);
    toAdd.parent = root;
    root.data.add(3, toAdd);
  }
//*********************************************************
//*************Begin Functions Proper**********************
//*********************************************************
  
  public void setTrackSums(boolean trackSums) {
	  // TODO: resolve this.
	  throw new UnsupportedOperationException();
  }
  public void printSums() {
	  // TODO: resolve this.
	  throw new UnsupportedOperationException();
  }

  public double getResolution()
  {
    if(init)
      return resolution;
    else
      return -1;
  }
  
  public void fillWorld(double res)
  {
    resolution = res;
    IndexBlock toAdd;
    for(double i = (90-res); i >= -90; i-=res)
    {
      for(double k = -180; k < 180; k+=res)
      {
        toAdd = new IndexBlock(k, i, res, res);
        addBlock(toAdd);
      }
    }
    init();
  }

  public void addData(DataBlock val, boolean avg)
  {
    /*
     * 1. check sparse data as referenced in databuilder
     * 2. check readdtion from new data sources into an existing data set
     *  -will this be purely additive (i believe it currently is)
     *  -what do we want it to be? 
     *  -for multifile->single data set there are two cases-> additive or supplantal
     *  -additive will just add to the old value, assumes we are completing an old dataset
     *  -supplantal will replace the previous data, assumes later data is more correct
     *  or more important
     */
    QBNode nextNode;
    
    if(!init)
    {
      fillWorld(val.width);
    }
    
    //starting the recursive add function, will propagate through all blocks, adding where appropriate
    for(int i = 0; i < 4; i++)
    {
      nextNode = ((QBNode)root.data.get(i));
      if(nextNode.intersects(val.x, val.y, val.width, val.height))
      { //DB overlaps quad, so enter it and add or look at children
        addDataHelp(nextNode, val, avg);
      }
    }
  }

  public void resolveOverwrite(String holdName, String varName)
  {
    data.mergeHoldTo(holdName, varName);
  }
  
  public TreeMap extractMask(RegionMask m)
  {
    // This routine is does not appear to be used.
    makeRegion = new TreeMap();
    
    extractMaskHelp(root, m);
    return makeRegion;
  }

//*********************************************************
//*************Begin Private Functions*********************
//*********************************************************
  private void init()
  {
    init = true;
    data = new MatrixRepository( (int)Math.round((maxX-minX)/resolution ), (int)Math.round((maxY-minY)/resolution ));
  }
  
  private void addBlock(IndexBlock val)
  {
    addBlock(root, val);
  }
  private void addBlock(QBNode currNode, IndexBlock val)
  { //i love recursion
    if(currNode.leaf)
    { //leaf quadrant, add DB
      /*
       * the usage of 4 here is what makes the whole QuadBucketTree work
       * this allows variable unknown resolutions to be used in the structure without necessitating infinite splitting
       * idealy a quadrant will contain 1 corner of 4 seperate adjoining nodes
       * since we just store pointers this will not take too much memory
       */
      if(currNode.data.size() < 4)
      {//this quadrant has space, add the data!
        currNode.data.add(val);
      }else
      {//split this node into 4 quadrants
        split(currNode);
        //now that this node has been split, add to it
        addBlock(currNode, val);
      }
    } else
    {
      QBNode nextNode;
      //need to keep going down levels to find where DB should exist
      for(int i = 0; i < 4; i++)
      {
        nextNode = ((QBNode)currNode.data.get(i));
        if(nextNode.intersects(val.x, val.y, val.width, val.height))
        { //DB overlaps quad, so enter it and add or look at children
          addBlock(nextNode, val);
        }
      }
    }
    
  }
  private void split(QBNode currNode)
  {
    currNode.leaf = false;
    //saving the data to readd it to the new child nodes
    ArrayList hold = (ArrayList)currNode.data.clone();
    //creating child quadrants for this node
    QBNode toAdd = new QBNode(currNode.minX, currNode.centerX, currNode.minY, currNode.centerY);
    toAdd.parent = currNode;
    currNode.data.add(0, toAdd);
    toAdd = new QBNode(currNode.minX, currNode.centerX, currNode.centerY, currNode.maxY);
    toAdd.parent = currNode;
    currNode.data.add(1, toAdd);
    toAdd = new QBNode(currNode.centerX, currNode.maxX, currNode.centerY, currNode.maxY);
    toAdd.parent = currNode;
    currNode.data.add(2, toAdd);
    toAdd = new QBNode(currNode.centerX, currNode.maxX, currNode.minY, currNode.centerY);
    toAdd.parent = currNode;
    currNode.data.add(3, toAdd);
    //readding previous Data
    for(int i = 0; i < 4; i++)
    {
      //we add with addhelp because we already know these blocks will be a child of currNode, and to prevent adding to size
      addBlock(currNode, (IndexBlock)hold.get(i));
    }
  }
  private void addDataHelp(QBNode currNode, DataBlock val, boolean avg)
  {
    if(currNode.leaf)
    {
      double weight; //normalizing the value for addition into entry
      double p1, p2, p3; //proportion of overlaps
      IndexBlock index; //the current data we are working with
      IndexBlock obj1, obj2; //holder obj's to keep the shape n stuff
      Rectangle2D.Double holdNode;
      Iterator i1, i2;
      Map.Entry vEntry, tEntry;
      
      for(int i = 0; i < currNode.data.size(); i++)
      {
        weight = 0;
        index = (IndexBlock)currNode.data.get(i);
        
        holdNode = new Rectangle2D.Double(currNode.minX, currNode.minY, (currNode.maxX-currNode.minX), (currNode.maxY-currNode.minY));
        obj1 = new IndexBlock();
        obj2 = new IndexBlock();
        
        
        Rectangle2D.Double.intersect(index, val, obj1);//step 1
        p1 = val.getOverlap(index);//step 1
        
        p2 = obj1.getOverlap(holdNode);//step 2
        
        weight = p1*p2;
        if(!avg)
        { //additive so have to account for limited addition of values (p3)
          Rectangle2D.Double.intersect(obj1, holdNode, obj2); //step 2
          
          p3 = obj2.getOverlap(val); //step 3
          weight *= p3;
        }

        
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
              
              data.addValue(varName, timeName, index.getXIndex(), index.getYIndex(), weightValue);
            }
            
          }
        }
      }
    } else
    {
      QBNode nextNode;
      //need to keep going down levels to find where DB should exist
      for(int i = 0; i < 4; i++)
      {
        nextNode = ((QBNode)currNode.data.get(i));
        if(nextNode.intersects(val.x, val.y, val.width, val.height))
        { //DB overlaps quad, so enter it and add or look at children
          addDataHelp(nextNode, val, avg);
        }
      }
    }
  }
  
  private void extractMaskHelp(QBNode currNode, RegionMask m)
  {
    if(currNode.leaf)
    { //remember data in makeRegion is stored in TreeMap->TreeMap->TreeMap
      TreeMap<String, TreeMap<Double, Double>> dataPoint;
      IndexBlock entry;
      Point2D.Double addPoint;
      double weight;
      for(int i = 0; i < currNode.data.size(); i++)
      {
        entry = (IndexBlock)currNode.data.get(i);
        //making sure this block of data is not just within the regions bounding box, but also
        //lies over a region which is marked(1) as in the region
        weight = m.inRegion(entry.x, entry.y, entry.width, entry.height);
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
          dataPoint = data.getAllLayers(entry.getXIndex(), entry.getYIndex());
          //*done getting data
          
          //add the data to the correct TreeMap (based on data name)
          //iterate through the data in this Node, by Var, then Time, adding to makeRegion
          Map.Entry var, time;
          Iterator iV = dataPoint.entrySet().iterator();
          while(iV.hasNext())
          {
            //iterating through variables
            var = (Map.Entry)iV.next();
            if(!makeRegion.containsKey(var.getKey()))
            {//if makeRegion does not yet have a mapping for this variable, add it now
              makeRegion.put(var.getKey(), new TreeMap());
            }
            Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
            while(iT.hasNext())
            {
              time = (Map.Entry)iT.next();
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
    } else
    {
      QBNode nextNode;
      //need to keep going down levels to find where the data resides
      for(int i = 0; i < 4; i++)
      {
        nextNode = ((QBNode)currNode.data.get(i));
        if(nextNode.intersects(m.x, m.y, (m.width+m.resolution), (m.height+m.resolution)))
        { //Node overlaps boundingbox of mask in some way, so enter it and check children
          extractMaskHelp((QBNode)currNode.data.get(i), m);
        }
      }
    }
  }
}
