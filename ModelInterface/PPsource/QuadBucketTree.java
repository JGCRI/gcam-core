/*
 * This software, which is provided in confidence, was prepared by employees
        of Pacific Northwest National Laboratory operated by Battelle Memorial
        Institute. Battelle has certain unperfected rights in the software 
        which should not be copied or otherwise disseminated outside your 
        organization without the express written authorization from Battelle. All rights in
        the software are reserved by Battelle.  Battelle makes no warranty,
        express or implied, and assumes no liability or responsibility for the
        use of this software.
 */
/*!
 * \file QuadBucketTree.java
 * \ingroup Preprocess
 * \brief PR-quadtree like data structure for storage of data with area.
 *
 *  This data structure is a PR-quadtree modified to have 4 data elements in each
 * leaf node. Data will exist in any number of nodes though ideally each of its
 * corners will be in a different node. This allows any resolution without
 * infinite splitting of leafs. This structure implements rotation as well as splitting
 * so as to remain as packed as possible.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */

/* DEVELOPER COMMENTS
 * USAGE:
 * create with QuadBucketTree(double, double, double, double)
 * set resolution by running fillWorld(double desiredResolution)
 * add data by having this quadbuckettree call merge on QuadBucketTrees with one set of data in them
 * 		for single sets of data, create a QBT and use add for every block of data you wish the set to contain
 * to get a higher resolution, create a new QBT and run fillWorld with the new rez, then merge the old tree into it
 * -idealy rez will be set once, as remaking the trees can be costly in terms of time
 * -try to add data in high rez to low rez order (or at least highest rez first)
 *
 * ISSUES:
 * this tree is nondeterministic, but this isnt actually a problem at all
 *
 * quadrants:
 * 1 2
 * 0 3
 */
/**
 * TODO: addData- i dont think i even really need this anymore, but its only like 3 more lines of code
 * 	each run of data will be created in its own QuadBucketTree. Trees will then be merged together.
 * 	Merging consists of the calling tree extracting data from the callee tree on a node by node basis
 */
package ModelInterface.PPsource;

import java.util.*;
import java.awt.geom.*;
import java.io.*;



/**
 * PR-quadtree like data structure for storage of data with area.
 * This data structure is a PR-quadtree modified to have 4 data elements in each
 * leaf node. Data will exist in any number of nodes though ideally each of its
 * corners will be in a different node. This allows any resolution without
 * infinite splitting of leafs. This structure implements rotation as well as splitting
 * so as to remain as packed as possible.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class QuadBucketTree
{
  private QBNode root; //root node, might wanna hold on to this one
  public int size; //the number of DB's in the tree
  public double resolution; //resolution of the DataBlocks stored in teh tree
  public boolean avg; //when only 1 data elem, whether it is averaged (true) or added (false) on aggregation
  private double minX; //these are the bounds of the Tree itself, all pretty obvious
  private double maxX;
  private double centerX;
  private double minY;
  private double maxY;
  private double centerY;
  //used as globals for some recursive functions
  private TreeSet rangePrint; //stores a list of data which was found in rangeSearch function
  private TreeMap makeRegion; //stores the data hierarchy to be returned in extractRegion
  
  private int occured; //a counter for the number of times something (ususally a datablock) has occured
  
//*********************************************************
//*****************Class Constructors**********************
//********************************************************* 
  /**
   * Default Constuctor
   * @deprecated has no bounds to create child nodes with
   */
  public QuadBucketTree()
  {
    //um, never use this one
    size = 0;
    root = new QBNode();
    root.leaf = false;
    QBNode toAdd;
    for(int i =0; i < 4; i++)
    {
      toAdd = new QBNode();
      toAdd.parent = root;
      root.data.add(i, toAdd);
    }
  }
  /**
   * Standard Constructor creates tree and child nodes with supplied bounds.
   * Assumes that all data in itself will be averaged when added over area.
   * 
   * @param x1 Smaller X coordinate of bounds.
   * @param x2 Larger X coordinate of bounds.
   * @param y1 Smaller Y coordinate of bounds.
   * @param y2 Larger Y coordinate of bounds.
   */
  public QuadBucketTree(double x1, double x2, double y1, double y2)
  {
    //setting up basic information
    size = 0;
    avg = true;
    minX = x1;
    maxX = x2;
    centerX = (x1+x2)/2;
    minY = y1;
    maxY = y2;
    centerY = (y1+y2)/2;
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
  /**
   * Create an filled QuadBucketTree with blocks of size res which contain no data.
   * Used to set up an initial tree if a user wishes to specify their own resolution.
   * Also used to change resolutions after already having created some data trees.
   * 
   * @param res Resolution of blocks which are to fill the tree.
   */
  public void fillWorld(double res)
  {
    //this will create a blank slate structure, filled with blocks of the passed in resolution
    //but containing no data. it is used when changing resolutions. and might become obsolete soon...
    //i think fillWorld will only be necessary if the user specifies a resolution which isnt native to the data
    resolution = res;
    DataBlock toAdd;
    for(double i = (90-res); i >= -90; i-=res)
    {
      for(double k = -180; k < 180; k+=res)
      {
        toAdd = new DataBlock(k, i, res, res);
        //haha im an idiot, i never actually added the new block...
        addBlock(toAdd);
      }
    }
    
  }
  /**
   * Adds some block of data to the tree. Rotates and splits the tree where appropriate.
   * 
   * @param val The {@link DataBlock} which is to be added to the tree.
   */
  public void add(DataBlock val)
  {
    // changed the way trees work... only add one piece of data now, the rest are put in with merge
    if(size == 0)
    {
      resolution = val.height;
    }
    addBlock(val);
  }
  /**
   * Adds a new block to the tree's structure with any contained data. 
   * 
   * @param val The {@link DataBlock} to be added.
   */
  public void addBlock(DataBlock val)
  {
    QBNode nextNode;
    size++;
    //starting the recursive add function, will propagate through all blocks, adding where appropriate
    for(int i = 0; i < 4; i++)
    {
      nextNode = ((QBNode)root.data.get(i));
      if(nextNode.intersects(val.x, val.y, val.width, val.height))
      { //DB overlaps quad, so enter it and add or look at children
        addBlockHelp(nextNode, val);
      }
    }
  }
  /**
   * Adds a blocks worth of data to the tree in the referenced position, but not a block itself.
   * Does not change the structure of the tree at all.
   * 
   * @param val The {@link DataBlock} defining the area and data to be added.
   */
  public void addData(DataBlock val, boolean avg)
  {
    QBNode nextNode;
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
  /**
   * Gets a blocks worth of data from the supplied location and fills the passed DataBlock with it.
   * 
   * @param val The {@link DataBlock} defining where to extract data from, which is then filled
   * with the data.
   * @return Whether or not the passed area could be located in the tree.
   */
  public boolean getData(DataBlock val)
  {
    /*
     * val defines the block of data we want
     * val is filled with this Tree's data and used to carry it to the calling tree
     */
    /*
     * this function is recursive fun: occured is how many nodes data was found in, 
     * if it remains 0 this area didnt exist in this data set
     * getDataHelp will return either 0 or a weighted value for the data which was found
     */
    double hold = 0;
    occured = 0;
    //i hope recursion works like i remember it...
    getDataHelp(root, val);
    if(occured > 0)
    { //then there was data for this block
      return true;
    }
    return false;
  }
  /**
   * Removes the passed block from the tree.
   * 
   * @param val The block to remove.
   * @deprecated Not Implemented.
   */
  public void remove(DataBlock val)
  {
    System.out.println("function has not been implemented");
  }
  /**
   * Checks to see if the supplied coordinates relate to a block of data in this tree.
   * 
   * @param x X coordinate to be checked.
   * @param y Y coordinate to be checked.
   * @return Whether or not the point corresponded to a block.
   */
  public boolean exists(double x, double y)
  {
    QBNode currNode = root;
    QBNode nextNode;
    //this will make sure bounds closed or not does not effect what contains returns
    double testX = x+(resolution/2);
    double testY = y+(resolution/2);
    
    while(!currNode.leaf)
    {
      for(int i = 0; i < 4; i++)
      {
        nextNode = ((QBNode)currNode.data.get(i));
        if(nextNode.contains(testX, testY))
        {
          currNode = nextNode;
          i=4;
        }
      }
    }
    DataBlock entry;
    for(int i = 0; i < currNode.data.size(); i++)
    {
      entry = (DataBlock)currNode.data.get(0);
      if(x==entry.x && y==entry.y)
        return true;
    }
    return false;
  }
  /**
   * Prints an XML output of any datablocks which are found in the passed circle.
   * 
   * @param x X-coordinate center of the circle.
   * @param y Y-coordinate center of the circle.
   * @param rad Radius of the circle to search.
   */
  public void rangeSearch(float x, float y, float rad)
  {
    /*
     * this rangeSearch doesnt actually range search, it searchs a square region approx. circle
     * this shouldnt matter because for the application this function will never get used except in testing
     */
    Comparator comp = new coordCompare();
    rangePrint = new TreeSet(comp);
    rangeHelp(root, x, y, rad);
    if(rangePrint.isEmpty())
    {
      printDeep("<error command=\"rangeSearch\" message=\"No DataBlocks exist in the given range\">", 1);
    }else
    {
      printDeep("<success command=\"rangeSearch\">", 1);
      printDeep("<dataList>", 2);
      Iterator i = rangePrint.iterator();
      DataBlock value = (DataBlock)i.next();
      printDeep("<data x=\""+(int)value.x+"\" y=\""+(int)value.y+"\" resolution=\""+(double)value.width+"\"/>", 3);
      while(i.hasNext())
      {
        value = (DataBlock)i.next();
        printDeep("<data x=\""+(int)value.x+"\" y=\""+(int)value.y+"\" resolution=\""+(double)value.width+"\"/>", 3);
      }
      printDeep("</dataList>", 2);
      printDeep("</success>", 1);
    }
  }
  /**
   * Returns the data which lies in the passed region. Works like a range search,
   * going from node to node based on simple rectangle bounds, then checking actual
   * blocks of data against a bitmask. Extracts data as the {@link RegionMask}'s resolution.
   * @param m RegionMask which defines what data is to be extracted.
   * @return TreeMap containing a listing of every variable, then everytime, and finally every location
   * of data which this region contains.
   */
  public TreeMap extractMask(RegionMask m)
  {
    /* based on the rangeSearch function, this essentially builds a region object based on the supplied mask
     * uses the native resolution of the tree which will be the sharpest possible resolution or a user
     * defined value.
     * Come in with a rectangle, use rectangle for logN runtime, test specific blocks against bitmask
     * blocks which overlap at all, add to TreeMap(var)->TreeMap(time)->TreeMap(coords)
     * array should be coord ordered, so can just print out and read in in predefined order, but how store this?
     * need something which doesnt allow duplicate members, could create a class which just stores coords, or use point2D
     * but that is gunna use alot of memory... might be unavoidable though, ok, store pairs of point2D's and data
     * use coord sorting which ill make a comparator for, this will print out in correct order and yay
     * oh, treeset doesnt allow duplicates so ill use that, no im stupid, need pairs so i guess its gunna be a treeMap
     * print out in this approx. form
     * WHAT ABOUT OFF RESOLUTIONS- apparently dont worry about it
     */
    makeRegion = new TreeMap();
    
    extractMaskHelp(root, m);
    return makeRegion;
  }
  /**
   * Adds the data from the passed tree into this tree. Extracts data by block
   * so diffrences in resolution are automatically handled. 
   * 
   * @param source QuadBucketTree whos data is to be added.
   */
  public void mergeTrees(QuadBucketTree source)
  {
    mergeTreesHelp(root, source);
  }
  /**
   * Prints the structure of this tree in XML format to standard out.
   *
   */
  public void printTree()
  {
    System.out.println("<quadbuckettree numBlocks=\""+size+"\">");
    printNode(root, 1);
    System.out.println("</quadbuckettree>");
  }
  /**
   * Prints the structure of this tree in XML format to the default file 'allData.xml'.
   *
   */
  public void printTreeXML()
  { //default version of printing to XML, uses default file name
    printTreeXML("allData.xml");
  }
  /**
   * Prints the structure of this tree in XML format to the passed file name.
   * 
   * @param fileName File which will be written with tree data.
   */
  public void printTreeXML(String fileName)
  { //same exact thing as you get with print tree, just printed to an XML file rather than consol
    try
    {
      BufferedWriter output = new BufferedWriter( new FileWriter(fileName));
      output.write("<quadbuckettree numBlocks=\""+size+"\">");
      output.newLine();
      printNodeXML(root, 1, output);
      output.write("</quadbuckettree>");
      output.newLine();
      output.flush();
      output.close();
      
    } catch (IOException e) 
    {
      System.out.println("Problem writing tree to xml file -> "+fileName);
    }
  }
  
//*********************************************************
//*************Begin Helper Functions**********************
//*********************************************************
  private void printNode(QBNode currNode, int currH)
  {
    if(currNode.leaf == false)
    {
      printDeep("<guide center=\""+currNode.centerX+","+currNode.centerY+"\">", currH);
      for(int i = 0; i < 4; i++)
      {
        printNode((QBNode)currNode.data.get(i), currH+1);
      }
      printDeep("</guide>", currH);
    }
    else
    {
      printDeep("<leaf center=\""+currNode.centerX+","+currNode.centerY+"\" height=\""+(currNode.maxY-currNode.minY)+"\" width=\""+(currNode.maxX-currNode.minX)+"\">", currH);
      if(currNode.data.size() == 0)
      {
        printDeep("<block value=\"empty\"/>", currH+1);
      }else
      {
        DataBlock entry;
        for(int i = 0; i < currNode.data.size(); i++)
        {
          entry = (DataBlock)currNode.data.get(i);
          printDeep("<block location=\"("+entry.x+","+entry.y+")\" resolution=\""+entry.height+"\">", currH+1);
          if(entry.data.isEmpty())
          {
            printDeep("<entry key=\"EMPTY\"/>", currH+2);
          } else
          {
            Map.Entry var, time;
            Iterator iV = entry.data.entrySet().iterator();
            //instead of just printing data, need to go one level lower to get time
            while(iV.hasNext())
            {
              var = (Map.Entry)iV.next();
              printDeep("<entry key=\""+var.getKey()+"\">", currH+2);
              Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
              while(iT.hasNext())
              {
                time = (Map.Entry)iT.next();
                printDeep("<data time=\""+time.getKey()+"\" value=\""+time.getValue()+"\"/>", currH+3);
              }
              printDeep("</entry>", currH+2);
            }
          }
            printDeep("</block>", currH+1);
        }
      }
      printDeep("</leaf>", currH);
    }
  }
  private void printNodeXML(QBNode currNode, int currH, BufferedWriter output) throws IOException
  {
    if(currNode.leaf == false)
    {
      printDeepXML("<guide center=\""+currNode.centerX+","+currNode.centerY+"\">", currH, output);
      for(int i = 0; i < 4; i++)
      {
        printNodeXML((QBNode)currNode.data.get(i), currH+1, output);
      }
      printDeepXML("</guide>", currH, output);
    }
    else
    {
      printDeepXML("<leaf center=\""+currNode.centerX+","+currNode.centerY+"\" height=\""+(currNode.maxY-currNode.minY)+"\" width=\""+(currNode.maxX-currNode.minX)+"\">", currH, output);
      if(currNode.data.size() == 0)
      {
        printDeepXML("<block value=\"empty\"/>", currH+1, output);
      }else
      {
        DataBlock entry;
        for(int i = 0; i < currNode.data.size(); i++)
        {
          entry = (DataBlock)currNode.data.get(i);
          printDeepXML("<block location=\"("+entry.x+","+entry.y+")\" resolution=\""+entry.height+"\">", currH+1, output);
          if(entry.data.isEmpty())
          {
            printDeepXML("<entry key=\"EMPTY\"/>", currH+2, output);
          } else
          {
            Map.Entry var, time;
            Iterator iV = entry.data.entrySet().iterator();
            //instead of just printing data, need to go one level lower to get time
            while(iV.hasNext())
            {
              var = (Map.Entry)iV.next();
              printDeepXML("<entry key=\""+var.getKey()+"\">", currH+2, output);
              Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
              while(iT.hasNext())
              {
                time = (Map.Entry)iT.next();
                printDeepXML("<data time=\""+time.getKey()+"\" value=\""+time.getValue()+"\"/>", currH+3, output);
              }
              printDeepXML("</entry>", currH+2, output);
            }
          }
            printDeepXML("</block>", currH+1, output);
        }
      }
      printDeepXML("</leaf>", currH, output);
    }
  }
  private void printDeep(String line, int indent)
  {
    for(int i = 0; i < indent; i++)
    {
      System.out.print("\t");
    }
    System.out.println(line);
  }
  private void printDeepXML(String line, int indent, BufferedWriter output) throws IOException
  {
    for(int i = 0; i < indent; i++)
    {
      output.write("\t");
    }
    output.write(line);
    output.newLine();
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
      addBlockHelp(currNode, (DataBlock)hold.get(i));
    }
  }
  private void rangeHelp(QBNode currNode, double x, double y, double rad)
  {
    if(currNode.leaf)
    {
      //hey its a leaf node! yay! if its not empty add DBs to the print list!
      if(currNode.data.size()>0)
      {
        DataBlock hold;
        for(int i = 0; i < currNode.data.size(); i++)
        {
          hold = (DataBlock)currNode.data.get(i);
          if(hold.intersects(x-rad, y+rad, (rad*2), (rad*2)))
          {
            rangePrint.add(hold);
          }
        }
      }
    }else
    {//this is a guide node, call rangehelp for all subnodes which overlap square in some way
      QBNode nextNode;
      for(int i = 0; i < 4; i++)
      {
        nextNode = ((QBNode)currNode.data.get(i));
        if(nextNode.intersects(x-rad, y+rad, (rad*2), (rad*2)))
        { //DB overlaps quad, so enter it and add or look at children
          rangeHelp(nextNode, x, y, rad);
        }
      }
    }
  }
  private void addDataHelp(QBNode currNode, DataBlock val, boolean avg)
  {
    if(currNode.leaf)
    {
      //System.out.println("\tnew leaf");
      double weight; //normalizing the value for addition into entry
      double p1, p2, p3; //proportion of overlaps
      DataBlock entry; //the current data we are working with
      DataBlock toWeight; //the intersection of entry and its containing leaf
      DataBlock p1block;
      Rectangle2D.Double holdNode;
      
      for(int i = 0; i < currNode.data.size(); i++)
      {
        weight = 0;
        entry = (DataBlock)currNode.data.get(i);
        
        p1block = new DataBlock();
        holdNode = new Rectangle2D.Double(currNode.minX, currNode.minY, (currNode.maxX-currNode.minX), (currNode.maxY-currNode.minY));
        Rectangle2D.Double.intersect(entry, holdNode, p1block);
        p1 = entry.getOverlap(holdNode);
        
        p2 = p1block.getOverlap(val);
        
        if(avg)
        {
        	weight = p1*p2;
        } else //add
        {
          p3 = val.getOverlap(p1block);
          weight = p1*p2*p3;
        }
        
        //System.out.println("\t\tnew block "+entry.x+","+entry.y+"->"+weight);
        
        if(weight > 0)
        { //then there is some overlap, add data in some way
          String varName;
          TreeMap var, builder;
          Double newValue, addValue, oldValue, timeName;
          
          //System.out.println(entry.x+" "+(entry.x+entry.width)+" - "+entry.y+" "+(entry.y+entry.height));
          //System.out.println(val.x+" "+(val.x+val.width)+" - "+val.y+" "+(val.y+val.height)+"\n");
          
          varName = (String)val.data.firstKey();
          var = (TreeMap)val.data.get(varName);
          timeName = (Double)var.firstKey();
          addValue = (Double)var.get(timeName);
          
          if(entry.data.containsKey(varName))
          { //already an entry for this variable
            if(((TreeMap)entry.data.get(varName)).containsKey(timeName))
            { //time already exists, add together values, replace old value
              oldValue = (Double)((TreeMap)entry.data.get(varName)).get(timeName);
              newValue = new Double(oldValue.doubleValue()+(addValue.doubleValue()*weight));
              ((TreeMap)entry.data.get(varName)).put(timeName, newValue);
              //System.out.println("D"+oldValue+"->"+newValue+" ");
            } else
            { //must add time
              newValue = new Double(addValue.doubleValue()*weight);
              ((TreeMap)entry.data.get(varName)).put(timeName, newValue);
              //System.out.println("\t\t\tT"+weight+" ");
            }
          } else
          { //must add variable
            builder = new TreeMap();
            newValue = new Double(addValue.doubleValue()*weight);
            builder.put(timeName, newValue);
            entry.data.put(varName, builder);
            //System.out.println("V"+newValue+" ");
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
  private void addBlockHelp(QBNode currNode, DataBlock val)
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
        addBlockHelp(currNode, val);
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
          addBlockHelp(nextNode, val);
        }
      }
    }
  }
  private void getDataHelp(QBNode currNode, DataBlock val)
  {
    /*
     * recurses through structure returning the valuewhere nodes overlap, weighted by how much they overlap
     */
    if(currNode.leaf)
    {
      DataBlock entry;
      Double iHateObjects;
      TreeMap builder;
      double weight = 0;
      for(int i = 0; i < currNode.data.size(); i++)
      {
        entry = (DataBlock)currNode.data.get(i);
        //ok i figured it out! the weight is correct, but its getting readded because up to 4 leafs point
        //to oneDB, so i need to additionally weight THAT weight by how much of that entry is contained in
        //it's holding leaf node. that way if only .25 of a DB is in each of 4 nodes when they are all added
        //it will = 1*value
        if(avg)
        { //weight is the fraction of val (the calling node) which is overlapped
          weight = val.getOverlap(entry);
        } else //add
        { //weight is the fraction of entry (the source node) which is overlapped
          weight = entry.getOverlap(val);
        }
        //normalizing weight in regard to its holding leaf node
        double Wrez = currNode.maxX-currNode.minX;
        double Hrez = currNode.maxY-currNode.minY;
        weight *= entry.getOverlap(currNode.minX, currNode.minY, Hrez, Wrez);
        if(weight > 0)
        { //then entry's information should be used in some way (add or average is the same here)
          occured++; //found somewhere to return data from
          if(val.data.isEmpty())
          { //this will fill val with all of this trees variable names
            Map.Entry var, time;
            Iterator iV = entry.data.entrySet().iterator();
            while(iV.hasNext())
            {
              //iterating through variables, create new empty treemap everytime
              var = (Map.Entry)iV.next();
              Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
              builder = new TreeMap();
              while(iT.hasNext())
              {
                //filling treemap with weighted values for each occured time
                time = (Map.Entry)iT.next();
                iHateObjects = new Double((((Double)time.getValue()).doubleValue()*weight));
                builder.put(time.getKey(), iHateObjects);
              }
              //adding the treemap of time for each seperate variable after it is built
              val.data.put(var.getKey(), builder);
            }
          }
          else
          { //same as above but adding together with old data in val
            Map.Entry var, time;
            Iterator iV = entry.data.entrySet().iterator();
            while(iV.hasNext())
            {
              //iterating through variables, create new empty treemap everytime
              var = (Map.Entry)iV.next();
              Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
              builder = new TreeMap();
              while(iT.hasNext())
              {
                //filling treemap with weighted values for each occured time
                time = (Map.Entry)iT.next();
                iHateObjects = new Double((((Double)((TreeMap)val.data.get(var.getKey())).get(time.getKey())).doubleValue()+(((Double)time.getValue()).doubleValue()*weight)));
                builder.put(time.getKey(), iHateObjects);
              }
              //adding the treemap of time for each seperate variable after it is built
              val.data.put(var.getKey(), builder);
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
        if(nextNode.intersects(val.x, val.y, val.width, val.height))
        { //DB overlaps quad, so enter it and look for data regions in there
          getDataHelp(nextNode, val);
        }
      }
    }
  }
  private void mergeTreesHelp(QBNode currNode, QuadBucketTree source)
  {
    if(currNode.leaf)
    {
      DataBlock entry, sourceDB;
      TreeMap builder;
      String newData;
      for(int i = 0; i < currNode.data.size(); i++)
      { //for each DB in the host tree, extract that size rectangle of data from source tree
        entry = (DataBlock)currNode.data.get(i);
        sourceDB = new DataBlock(entry);
        if(source.getData(sourceDB))
        { //found some data for this block, add all variables to its list
          //need to differentiate between adding a new variable and adding a new time to a variable
          //need to iterate through TreeSet to add all data
          //needless to say, iterating through treesets isnt sweet
          //*removed the basecase entirely (only 1 variable to merge) its the same code... so yeah
          Map.Entry var, time;
          Iterator iV = sourceDB.data.entrySet().iterator();
          while(iV.hasNext())
          {
            var = (Map.Entry)iV.next();
            if(entry.data.containsKey(var.getKey()))
            { //then add whatever time value was present
              Iterator iT = ((TreeMap)var.getValue()).entrySet().iterator();
              while(iT.hasNext())
              {
                time = (Map.Entry)iT.next();
                ((TreeMap)entry.data.get(var.getKey())).put(time.getKey(), time.getValue());
              }
            } else
            { //add the variable itself
              entry.data.put(var.getKey(), var.getValue());
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
        mergeTreesHelp((QBNode)currNode.data.get(i), source);
      }
    }
  }
  private void extractMaskHelp(QBNode currNode, RegionMask m)
  {
    if(currNode.leaf)
    { //remember data in makeRegion is stored in TreeMap->TreeMap->TreeMap
      DataBlock entry;
      Point2D.Double addPoint;
      double weight;
      String newData;
      for(int i = 0; i < currNode.data.size(); i++)
      {
        entry = (DataBlock)currNode.data.get(i);
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
          //add the data to the correct TreeMap (based on data name)
          //iterate through the data in this Node, by Var, then Time, adding to makeRegion
          Map.Entry var, time;
          Iterator iV = entry.data.entrySet().iterator();
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
