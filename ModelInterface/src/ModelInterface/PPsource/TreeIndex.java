package ModelInterface.PPsource;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class TreeIndex implements DataIndex
{
  private DataRepository data; //this is where all data is stored and where we will get it from
  private QBNode root; //root node, might wanna hold on to this one
  private boolean init; //has this index been initialized? (if not must fillWorld)
  
  public double resolution; //resolution of the DataBlocks stored in teh tree
  
//*********************************************************
//*****************Class Constructors**********************
//*********************************************************
  
//*********************************************************
//*************Begin Functions Proper**********************
//*********************************************************
  
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
  }

  public void addData(DataBlock val)
  {
    QBNode nextNode;
    //starting the recursive add function, will propagate through all blocks, adding where appropriate
    for(int i = 0; i < 4; i++)
    {
      nextNode = ((QBNode)root.data.get(i));
      if(nextNode.intersects(val.x, val.y, val.width, val.height))
      { //DB overlaps quad, so enter it and add or look at children
        addDataHelp(nextNode, val, true); //TODO avg was never implemented... oops
      }
    }
  }

  public TreeMap extractMask(RegionMask m)
  {
    // TODO Auto-generated method stub
    return null;
  }

//*********************************************************
//*************Begin Private Functions*********************
//*********************************************************
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
          TreeMap var, builder;
          double weightValue, addValue, timeName;
          
         
          
          //TODO here we get out the index from our indexBlock and use that to
          //address and add our data to the DataRespository
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
  
}
