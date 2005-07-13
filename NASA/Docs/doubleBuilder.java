/*
 * Created on May 6, 2005
 * by Vincent Nibali
 * for JGCRI, College Park, MD
 */

/*
 * This is a very memory intensive application. the -Xmx argument to the JVM should be set to 128M to be safe at res=1
 * actual usage at resolution of 1 is approx 70M, at higher resolutions this will grow drastically
 */

/**
 * TODO: remake xml readers using a DOM parser for greater readability
 */

import java.io.*;
import java.util.*;
import java.awt.geom.*;
import ucar.ma2.*;
import ucar.nc2.*;
import org.jdom.*;
import org.jdom.input.*;

import javax.xml.stream.*;

public class DataBuilder
{
  public QuadBucketTree dataTree; //a tree of all the aggregated data to be used to create regions
  public TreeSet regionList; //a list of all the defined regions' names
  public TreeMap maskList; //all of the regionmasks which will be used to extract regions of data
  public TreeMap printList; //a holder of unwrapped regions before they are printed
  private String iSource;
  private String rSource;
  private String outFile;
  private XMLStreamReader iReader;
  private XMLStreamReader rReader;
  private Document iDocument;
  private Document rDocument;
  private BufferedWriter rWriter;
  private boolean init;
  private boolean URes;

//*********************************************************
//*****************Class Constructors**********************
//*********************************************************  
  
  public DataBuilder()
  {
    init = false;
    URes = false;
    iSource = "sources.xml";
    rSource = "regions.xml";
    outFile = "regionTrees.xml";
    dataTree = new QuadBucketTree(-180, 180, -90, 90);
    regionList = new TreeSet();
    maskList = new TreeMap();
    printList = new TreeMap();
  }
  
  public DataBuilder(String i, String r, String o)
  {
    init = false;
    URes = false;
    iSource = i;
    rSource = r;
    outFile = o;
    dataTree = new QuadBucketTree(-180, 180, -90, 90);
    regionList = new TreeSet();
    maskList = new TreeMap();
    printList = new TreeMap();
  }
  
//*********************************************************
//*************Begin Functions Proper**********************
//********************************************************* 
  
  public void runAll()
  {
    makeStreams();
    buildTree2();
    readMasks2();
    fillRegions();
    writeRegions();
  }
  
  public void buildTree()
  {
    //TODO: add new file types as needed
    //MAIN XML READ LOOP FOR QBTREE BUILDING
    try
    {
      while(iReader.hasNext()) //iterate through document
      {
        iReader.next();
        if(iReader.getEventType() == XMLStreamConstants.START_ELEMENT)
        { //at a new element, process it
          if(iReader.getLocalName().equals("input"))
          { //find out if user is forcing his own resolution
            if(Double.parseDouble(iReader.getAttributeValue(null, "resolution")) > 0)
            {
              URes = true;
              double userRes = Double.parseDouble(iReader.getAttributeValue(null, "resolution"));
              dataTree.fillWorld(userRes);
              init = true;
            }
          } else if(iReader.getLocalName().equals("file"))
          { //determine file type and pass stream to corresponding function
            if(iReader.getAttributeValue(null, "type").equals("txt"))
            {
              addTxtData();
            } else if(iReader.getAttributeValue(null, "type").equals("somethingelse"))
            {
              
            } else
            {
              System.out.println("Unsupported File Type -> "+iReader.getAttributeValue(null, "type"));
            }
          }
          else if(iReader.getLocalName().equals("somethingelse"))
          {
            
          }
          else
          {
            System.out.println("Unknown XML Element -> "+iReader.getLocalName());
          }
          
        }
      }
    } catch(XMLStreamException e)
    {
      System.out.println("input's file stream has broken");
    }
    //END MAIN XML LOOP FOR TREE BUILD
  }
  
  public void buildTree2()
  {
    //TODO: add new file types as needed
    Element currFile;
    Element root = iDocument.getRootElement();
    List fileChildren = root.getChildren("file");
    
    if(Double.parseDouble(root.getAttributeValue("resolution")) > 0)
    { //getting a user resolution if supplied
      URes = true;
      double userRes = Double.parseDouble(root.getAttributeValue(null, "resolution"));
      dataTree.fillWorld(userRes);
      init = true;
    }
    
    //MAIN XML READ LOOP FOR QBTREE BUILDING
    for(int i = 0; i < fileChildren.size(); i++)
    {
      currFile = (Element)fileChildren.get(i);
      if(currFile.getAttributeValue("type").equals("txt"))
      {
        addTxtData2(currFile);
      } else
      {
        System.out.println("Unsupported File Type -> "+currFile.getAttributeValue(null, "type"));
      }
    }
    //END MAIN XML LOOP FOR TREE BUILD
  }
  
  public void readMasks()
  {
    /*
     * im having some moral issues with this function... how am i going to read in bitmasks and get a good bounding rect
     * dont want to have a world sized box, that would kill runtime for region extractions
     * but how can i build the matrices dynamically, if i use a single run i wont know least bounds, so would have to
     * reallocate everytime something new was read in and shift the data around (horrible)
     * if i read the data in 2 runs that would probably be faster, BUT still not good, cuz its 2 runs
     * one run could determine the bounds for each region to be read, and the next would actually gather the masks
     * this would be optimal, but i dont know how long it takes to go through a run of data...
     * alternatively, regions could be stored as just a bounded mask with x,y start coordinates already in there
     * i dont know how hard that would be to get
     * 
     * what about 2 runs vs 1 run and a trim? how fast or slow would a trim function be?
     * im guessing that 2 runs would be preferable when dealing with multiple regions in one file
     * but if your just reading one region trimming might be faster than an addition run through the data
     * though... o!
     * what about a single run which reads into world sized matrices and tracks bounds, and then just use get subMatrix
     * or whatever to get the matrix with correct bounds out, i wonder how fast that is... also i wonder how much space
     * these untrimmed matricies are gunna take up. resolution 10 or 30 no problem, but resolution .1... thats alot of data
     * in that case 2 runs still might be best.
     * 
     * ok im gunna do two runs, it will be a decent speed because every location or data element will only be checked twice
     * 2n isnt lightning quick, but i think its worth it for the later speedup in extraction etc.
     */
    //BEGIN MAIN XML LOOP FOR READING IN REGION MASKS
    try
    {
      while(rReader.hasNext()) //iterate through document
      {
        rReader.next();
        if(rReader.getEventType() == XMLStreamConstants.START_ELEMENT)
        { //at a new element, process it
          if(rReader.getLocalName().equals("file"))
          { //determine file type and pass stream to corresponding function
            if(rReader.getAttributeValue(null, "type").equals("txt"))
            {
              addTxtRegion();
            } else if(rReader.getAttributeValue(null, "type").equals("netcdf"))
            {
              addNetCDFRegion();
            } else
            {
              System.out.println("Unsupported File Type -> "+rReader.getAttributeValue(null, "type"));
            }
          }else if(rReader.getLocalName().equals("input"))
          {
            //dont need to do anything just didnt want to generate a unkown element thing
          }else if(rReader.getLocalName().equals("somethingelse"))
          {
            //holder code incase we need additional processing capabilities
          }
          else
          {
            System.out.println("Unknown XML Element -> "+rReader.getLocalName());
          }
          
        }
      }
    } catch(XMLStreamException e)
    {
      System.out.println("region's file stream has broken");
    }
    //END REGION MASK READING LOOP
  }
  
  public void readMasks2()
  {	//TODO redoing readMasks using a DOM parser instead of StAX, this is for ease of changes later
    //and because i believe the code will be much more solid and able to prevent user errors
    Element root = rDocument.getRootElement();
    List fileChildren = root.getChildren("file");
    Element currChild;

    //BEGIN MAIN XML LOOP FOR READING IN REGION MASKS
    for(int i = 0; i < fileChildren.size(); i++)
    {
      currChild = (Element)fileChildren.get(i);
      if(currChild.getAttributeValue("type").equals("txt"))
      {
        addTxtRegion2(currChild);
      } else if(currChild.getAttributeValue("type").equals("netcdf"))
      {
        addNetCDFRegion2(currChild);
      } else
      {
        System.out.println("Unsupported File Type -> "+currChild.getAttributeValue(null, "type"));
      }
    }
    //END REGION MASK READING LOOP
  }
  
  public void fillRegions()
  {
    //for each region in 'regionList' use the mask in 'maskList' to get data out of
    //'dataTree' using the extractMask function, store this information in printList
    TreeMap holdToPrint;
    String rName;
    Iterator it = regionList.iterator();
    while(it.hasNext())
    {
      rName = (String)it.next();
      holdToPrint = dataTree.extractMask((RegionMask)maskList.get(rName));
      printList.put(rName, holdToPrint);
    }
    //guess what, the treeMap's in printList are really just a region without a wrapper
    //congratulations the actual work of the proprocess stage is done once this works
    //yeah... i just congratulated myself... in comments
  }
  
  public void writeRegions()
  {
    /*
     * writes all of our info in the chosen resolution to a txt file
     * each region will havea list of variables, which will have a list of times, which will have a list of points and value
     * the points will be the array indicies which the data should be stored at in the region
     * this is calculated using the normal bounds found below, each region is followed by its x and y in lat and long degrees
     * and by the size of the matrix which will store its data
     * this gets rid of alot of computation and work when the actual data manipulation phase is run
     */
    /*
     * this function now writes its output to an xml file rather than a txt file so that it can more easily be read later
     */
    TreeMap holdToPrint;
    String rName, holder;
    Iterator itName, itVar, itTime, itData;
    int msizeX, msizeY;
    double normX, normY, normW, normH; //the normalized bounds of the region (rounded to a multiple of data resolution)
    double holdMX, holdMY, work;
    Map.Entry var, time, data;
    try
    {
      rWriter.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n");
      rWriter.write("<input num=\""+regionList.size()+"\" res=\""+dataTree.resolution+"\">");
      rWriter.newLine();

      itName = regionList.iterator(); //iterates through names
    //BEGIN MAIN REGION WRITING LOOP
      while(itName.hasNext())
      {
        rName = (String)itName.next();
        holdToPrint = (TreeMap)printList.get(rName);
        if(!holdToPrint.isEmpty())
        {
        //BEGIN WRITING OF SPECIFIC REGION DATA
          //calculating normalized bounds of region
          work = ((RegionMask)maskList.get(rName)).x+((RegionMask)maskList.get(rName)).width;
          holdMX = Math.ceil(work/dataTree.resolution)*dataTree.resolution;
          work = ((RegionMask)maskList.get(rName)).y+((RegionMask)maskList.get(rName)).height;
          holdMY = Math.ceil(work/dataTree.resolution)*dataTree.resolution;
          normX = Math.floor(((RegionMask)maskList.get(rName)).x/dataTree.resolution)*dataTree.resolution;
          normY = Math.floor(((RegionMask)maskList.get(rName)).y/dataTree.resolution)*dataTree.resolution;
          normW = holdMX-normX;
          normH = holdMY-normY;
          msizeY = (int)Math.ceil(normH/dataTree.resolution);
          msizeX = (int)Math.ceil(normW/dataTree.resolution);
          //printing regionName x y matrixHeight matrixWidth
          holder = "<region  name=\""+rName+"\" x=\""+normX+"\" y=\""+normY+"\" sizeY=\"";
          holder += (msizeY)+"\" sizeX=\""+(msizeX)+"\">";
          rWriter.write("\t"+holder);
          rWriter.newLine();
          //iterate through variables
          itVar = holdToPrint.entrySet().iterator();
          while(itVar.hasNext())
          {
            var = (Map.Entry)itVar.next();
            holder = "<variable value=\""+var.getKey()+"\">";
            rWriter.write("\t\t"+holder);
            rWriter.newLine();
            //iterate through times
            itTime = ((TreeMap)var.getValue()).entrySet().iterator();
            while(itTime.hasNext())
            {
              time = (Map.Entry)itTime.next();
              holder = "<time value=\""+time.getKey()+"\">";
              rWriter.write("\t\t\t"+holder);
              rWriter.newLine();
              //iterate through data, print in array style
              itData = ((TreeMap)time.getValue()).entrySet().iterator();
              while(itData.hasNext())
              {
                data = (Map.Entry)itData.next();
                //prints yIndex xIndex dataValue
                holder = "<data y=\""
                    +(Math.abs((int)(((((Point2D.Double)data.getKey()).y+dataTree.resolution)-(normY+normH))/dataTree.resolution)));
                holder += "\" x=\""+(int)((((Point2D.Double)data.getKey()).x-normX)/dataTree.resolution)
                    +"\" value=\"";
                holder += data.getValue()+"\" />";
                rWriter.write("\t\t\t\t"+holder);
                rWriter.newLine();
              }
              rWriter.write("\t\t\t</time>\n");
              rWriter.flush();
            }
            rWriter.write("\t\t</variable>\n");
          }
          rWriter.write("\t</region>\n");
          //END WRITING REGION
        }
      }
    //END WRITING LOOP
      rWriter.write("</input>\n");
      rWriter.flush();
      rWriter.close();
    } catch (IOException e){}
  }
  
//**********************************************************
//********************Component Readers*********************
//**********************************************************
  
  private void addTxtData() throws XMLStreamException
  { /* function will add the data from the specified file of type 'txt'
     * 'txt'- defined as 180/resolution lines of 360/resolution data elements
     * tagged or untagged - basically using this to debug program as i build it
     */
    boolean readTags = true;
    boolean tagged = true;
    boolean dec = true;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    double time = 0;
    double res = 1;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    QuadBucketTree toMerge = new QuadBucketTree(-180, 180, -90, 90);
    
    //check if tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(iReader.getAttributeValue(null, "tagged"))).booleanValue();
    
  //getting file info from XML
    while(readTags)
    {
      iReader.next();
      if(iReader.getEventType() == XMLStreamConstants.START_ELEMENT)
      { //at a new tag element, process it
        if(iReader.getLocalName().equals("res"))
        {
          res = Double.parseDouble(iReader.getAttributeValue(null, "value"));
        } else if(iReader.getLocalName().equals("data"))
        {
          dataName = iReader.getAttributeValue(null, "value");
        } else if(iReader.getLocalName().equals("date"))
        {
          time = Double.parseDouble(iReader.getAttributeValue(null, "value"));
        } else if(iReader.getLocalName().equals("format"))
        { //are numbers in decimal or scientific format
          if(iReader.getAttributeValue(null, "value").equals("scientific"))
          {
            dec = false;
          }
        } else if(iReader.getLocalName().equals("name"))
        {
          //name is always the last element of file, after this can open and read the file itself
          readTags = false;
          fileName = iReader.getAttributeValue(null, "value");
        } else
        {
          System.out.println("Unknown File Tag -> "+iReader.getLocalName());
        }
      }
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("FileNotFoundException!!!");
    }
  //txt file opened

    if(tagged)
    { //if didnt get these before in the xml file
      dataName = readWord(input);
      time = Double.parseDouble(readWord(input));
    	res = Double.parseDouble(readWord(input));
    	if(readWord(input).equals("scientific"))
    	{
    	  dec = false;
    	}
    }
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataTree.fillWorld(res);
      init = true;
    }
  //reading the data from the file
    for(double i = (90-res); i >= -90; i-=res)
    {
      for(double k = -180; k < 180; k+=res)
      {
        if(dec)
        { //numbers stored in decimal format
          dataValue = Double.valueOf(readWord(input));
        } else
        { //numbers stored in scientific notation
          dataValue = new Double(scientificToDouble(readWord(input)));
        }
        toAdd = new DataBlock(k, i, res, res);
        timeValue = new TreeMap();
        timeValue.put(new Double(time), dataValue);
        toAdd.data.put(dataName, timeValue);
        toMerge.add(toAdd);
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
    
  //merging this data with the current tree
    if((dataTree.resolution > res)&&(!URes))
    { //the new tree has a lower resolution... this shouldnt ever happen but hey people are dumb, and by people i mean me
      //this doesnt matter if there is a user defined resolution, if it exists always use it
      toMerge.mergeTrees(dataTree);
      dataTree = toMerge;
    } else
    {
      dataTree.mergeTrees(toMerge);
    }
  //done merging into single tree - dataTree now contains all data
  }
  
  private void addTxtData2(Element currFile)
  { /* function will add the data from the specified file of type 'txt'
     * 'txt'- defined as 180/resolution lines of 360/resolution data elements
     * tagged or untagged - basically using this to debug program as i build it
     */
    boolean readTags = true;
    boolean tagged = true;
    boolean dec = true;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    double time = 0;
    double res = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    QuadBucketTree toMerge = new QuadBucketTree(-180, 180, -90, 90);
    
    //check if tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(currFile.getAttributeValue(null, "tagged"))).booleanValue();
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("data"))
      {
        dataName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("date"))
      {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("format"))
      {
        if(currElem.getAttributeValue("value").equals("scientific"))
        { //dec is default to true
          dec = false;
        }
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else
      {
        System.out.println("Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("FileNotFoundException!!!");
    }
  //txt file opened

    if(tagged)
    { //if didnt get these before in the xml file
      dataName = readWord(input);
      time = Double.parseDouble(readWord(input));
    	res = Double.parseDouble(readWord(input));
    	if(readWord(input).equals("scientific"))
    	{
    	  dec = false;
    	}
    }
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataTree.fillWorld(res);
      init = true;
    }
  //reading the data from the file
    for(double i = (90-res); i >= -90; i-=res)
    {
      for(double k = -180; k < 180; k+=res)
      {
        if(dec)
        { //numbers stored in decimal format
          dataValue = Double.valueOf(readWord(input));
        } else
        { //numbers stored in scientific notation
          dataValue = new Double(scientificToDouble(readWord(input)));
        }
        toAdd = new DataBlock(k, i, res, res);
        timeValue = new TreeMap();
        timeValue.put(new Double(time), dataValue);
        toAdd.data.put(dataName, timeValue);
        toMerge.add(toAdd);
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
    
  //merging this data with the current tree
    if((dataTree.resolution > res)&&(!URes))
    { //the new tree has a lower resolution... this shouldnt ever happen but hey people are dumb, and by people i mean me
      //this doesnt matter if there is a user defined resolution, if it exists always use it
      toMerge.mergeTrees(dataTree);
      dataTree = toMerge;
    } else
    {
      dataTree.mergeTrees(toMerge);
    }
  //done merging into single tree - dataTree now contains all data
  }
  
  private void addTxtRegion() throws XMLStreamException
  {
    int nRegions = 0;
    int rblock;
    boolean readTags = true;
    boolean tagged = true;
    String fileName = "it is initialized thanks";
    String holdK; //the number which corresponds to a specific region in this file
    double res = 1;
    RegionMask holdR;
    TreeMap newRegions = new TreeMap();
    
    //tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(rReader.getAttributeValue(null, "tagged"))).booleanValue();
    
  //getting file info from XML
    while(readTags)
    {
      rReader.next();
      if(rReader.getEventType() == XMLStreamConstants.START_ELEMENT)
      { //at a new tag element, process it
        if(rReader.getLocalName().equals("res"))
        {
          res = Double.parseDouble(rReader.getAttributeValue(null, "value"));
        } else if(rReader.getLocalName().equals("region"))
        { //what region or regions this file describes
          nRegions = Integer.parseInt(rReader.getAttributeValue(null, "num"));
          for(int i = 0; i < nRegions; i++)
          { //setting up the RegionMask class for each region defined in this file
            rReader.next();
            while(rReader.getEventType() != XMLStreamConstants.START_ELEMENT)
            { //go until were at the next element
              rReader.next();
            }
            //*create an empty RegionMask with the supplied name and resolution for later filling*
            holdK = rReader.getAttributeValue(null, "key");
            holdR = new RegionMask(rReader.getAttributeValue(null, "value"), res);
            newRegions.put(holdK, holdR);
          }
        } else if(rReader.getLocalName().equals("name"))
        {
          //name is always the last element of file, after this can open and read the file itself
          readTags = false;
          fileName = rReader.getAttributeValue(null, "value");
        } else
        {
          System.out.println("Unknown File Tag -> "+rReader.getLocalName());
        }
      }
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("FileNotFoundException!!!");
    }
  //txt file opened
    
    if(tagged)
    { //if didnt get these before in the xml file
      res = Double.parseDouble(readWord(input));
      nRegions = Integer.parseInt(readWord(input));
      for(int i = 0; i < nRegions; i++)
      { //setting up the RegionMask class for each region defined in this file
        //*create an empty RegionMask with the supplied name and resolution for later filling*
        holdK = readWord(input);
        holdR = new RegionMask(holdK, res);
        newRegions.put(new Integer(i), holdR);
      }
    }
    
  //finding each regions bounding box
    
    for(double y = (90-res); y >= -90; y-=res)
    {
      for(double x = -180; x < 180; x+=res)
      {
        rblock = Integer.parseInt(readWord(input))-1;
        if(rblock >= 0)
        {
          holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
          if(holdR.height==-1)
          { //this is the first block being added to this region nothing to
            // test against yet
            holdR.y = y;
            holdR.x = x;
            holdR.height = res;
            holdR.width = res;
          } else
          { //test against old bounds, if outside them, change them
            if(y<holdR.y)
            { //y will never be higher, only lower
              holdR.height += (holdR.y-y);
              holdR.y = y;
            }
            if(x<holdR.x)
            { //x may be higher or lower
              holdR.width += (holdR.x-x);
              holdR.x = x;
            }
            if((x+res)>(holdR.x+holdR.width))
            {
              holdR.width = ((x+res)-holdR.x);
            }
          }
        }
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done finding bounds yessssssssss
    
  //initializing byte matrix for each region
    for(int i = 0; i <nRegions; i++)
    {
      ((RegionMask)newRegions.get(String.valueOf(i))).makeMatrix();
    }
  //done initializing byte matricies
    
  //its not exactly 'cool' that i have to open this file again... but who am i to complain
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("seriously, the file was just there, it really really shouldnt be not found");
    }
  //txt file opened
    
  //finally actually reading in the bitmask for the regions
    if(tagged)
    { //flushing through the tag information to get to region mask data
      readWord(input);
      readWord(input);
      try{
      input.readLine();
      } catch(IOException e){}
    } //FINALLY
    for(double y = (90-res); y >= -90; y-=res)
    {
      for(double x = -180; x < 180; x+=res)
      {
        rblock = Integer.parseInt(readWord(input))-1;
        if(rblock >= 0)
        {
          holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
        	holdR.setPointTrue(x, y);
        }
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done getting region bitmasks
    
  //adding these regions masks to the master list of masks
    for(int i = 0; i <newRegions.size(); i++)
    {
      holdR = ((RegionMask)newRegions.get(String.valueOf(i)));
      regionList.add(holdR.name);
      maskList.put(holdR.name, holdR);
    }
  //done adding region masks
    
  }
  
  private void addTxtRegion2(Element currFile)
  {
    int nRegions = 0;
    int rblock;
    boolean readTags = true;
    boolean tagged = true;
    String fileName = "it is initialized thanks";
    String holdK; //the number which corresponds to a specific region in this file
    double res = 1;
    Element currInfo;
    List infoList;
    RegionMask holdR;
    TreeMap newRegions = new TreeMap();
    
    //tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(currFile.getAttributeValue(null, "tagged"))).booleanValue();
  //getting file info from XML file
    infoList = currFile.getChildren();
    for(int i = 0; i < infoList.size(); i++)
    {
      currInfo = (Element)infoList.get(i);
      if(currInfo.getName().equals("res"))
      {
        res = Double.parseDouble(currInfo.getAttributeValue("value"));
      } else if(currInfo.getName().equals("name"))
      {
        fileName = currInfo.getAttributeValue("value");
      } else if(currInfo.getName().equals("region"))
      {
        List rList = currInfo.getChildren("RID");
        Element currR;
        for(int k = 0; k < rList.size(); k++)
        {
          currR = (Element)rList.get(k);
          holdK = currR.getAttributeValue("key");
          holdR = new RegionMask(currR.getAttributeValue("value"), res);
          newRegions.put(holdK, holdR);
        }
      } else
      {
        System.out.println("Unknown File Tag -> "+currInfo.getName());
      }
    }
  //done getting file info from XML
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("FileNotFoundException!!!");
    }
  //txt file opened
    
    if(tagged)
    { //if didnt get these before in the xml file
      res = Double.parseDouble(readWord(input));
      nRegions = Integer.parseInt(readWord(input));
      for(int i = 0; i < nRegions; i++)
      { //setting up the RegionMask class for each region defined in this file
        //*create an empty RegionMask with the supplied name and resolution for later filling*
        holdK = readWord(input);
        holdR = new RegionMask(holdK, res);
        newRegions.put(new Integer(i), holdR);
      }
    }
    
  //finding each regions bounding box
    
    for(double y = (90-res); y >= -90; y-=res)
    {
      for(double x = -180; x < 180; x+=res)
      {
        rblock = Integer.parseInt(readWord(input))-1;
        if(rblock >= 0)
        {
          holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
          if(holdR.height==-1)
          { //this is the first block being added to this region nothing to
            // test against yet
            holdR.y = y;
            holdR.x = x;
            holdR.height = res;
            holdR.width = res;
          } else
          { //test against old bounds, if outside them, change them
            if(y<holdR.y)
            { //y will never be higher, only lower
              holdR.height += (holdR.y-y);
              holdR.y = y;
            }
            if(x<holdR.x)
            { //x may be higher or lower
              holdR.width += (holdR.x-x);
              holdR.x = x;
            }
            if((x+res)>(holdR.x+holdR.width))
            {
              holdR.width = ((x+res)-holdR.x);
            }
          }
        }
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done finding bounds yessssssssss
    
  //initializing byte matrix for each region
    for(int i = 0; i <nRegions; i++)
    {
      ((RegionMask)newRegions.get(String.valueOf(i))).makeMatrix();
    }
  //done initializing byte matricies
    
  //its not exactly 'cool' that i have to open this file again... but who am i to complain
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      System.out.println("seriously, the file was just there, it really really shouldnt be not found");
    }
  //txt file opened
    
  //finally actually reading in the bitmask for the regions
    if(tagged)
    { //flushing through the tag information to get to region mask data
      readWord(input);
      readWord(input);
      try{
      input.readLine();
      } catch(IOException e){}
    } //FINALLY
    for(double y = (90-res); y >= -90; y-=res)
    {
      for(double x = -180; x < 180; x+=res)
      {
        rblock = Integer.parseInt(readWord(input))-1;
        if(rblock >= 0)
        {
          holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
        	holdR.setPointTrue(x, y);
        }
      }
    }
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done getting region bitmasks
    
  //adding these regions masks to the master list of masks
    for(int i = 0; i <newRegions.size(); i++)
    {
      holdR = ((RegionMask)newRegions.get(String.valueOf(i)));
      regionList.add(holdR.name);
      maskList.put(holdR.name, holdR);
    }
  //done adding region masks
    
  }
  
  private void addNetCDFRegion() throws XMLStreamException
  {
    /*
     * ok... here we go, basically just extract a 2D array from the netcdf variable in the file
     * then run through it just like in addTxtRegion, should be snap
     */
    boolean getName = true;
    double res = 1;
    int nRegions = 0;
    int rblock, NaN;
    String fileName = "init";
    String dataVar = "ctry";
    String holdK;
    int maskArray[][];
    RegionMask holdR;
    TreeMap newRegions = new TreeMap();
    
    while(getName)
    {
      rReader.next();
      if(rReader.getEventType() == XMLStreamConstants.START_ELEMENT)
      { //at a new tag element, process it
        if(rReader.getLocalName().equals("res"))
        {
          res = Double.parseDouble(rReader.getAttributeValue(null, "value"));
        } else if(rReader.getLocalName().equals("variable"))
        {
          dataVar = rReader.getAttributeValue(null, "value");
        } else if(rReader.getLocalName().equals("region"))
        { //what region or regions this file describes
          nRegions = Integer.parseInt(rReader.getAttributeValue(null, "num"));
          for(int i = 0; i < nRegions; i++)
          { //setting up the RegionMask class for each region defined in this file
            rReader.next();
            while(rReader.getEventType() != XMLStreamConstants.START_ELEMENT)
            { //go until were at the next element
              rReader.next();
            }
            //*create an empty RegionMask with the supplied name and resolution for later filling*
            holdK = rReader.getAttributeValue(null, "value");
            holdR = new RegionMask(rReader.getAttributeValue(null, "key"), res);
            newRegions.put(holdK, holdR);
          }
        } else if(rReader.getLocalName().equals("name"))
        {
          //name is always the last element of file, after this can open and read the file itself
          fileName = rReader.getAttributeValue(null, "value");
          getName = false;
        } else
        {
          System.out.println("Unknown File Tag in addNetCDFRegion -> "+rReader.getLocalName());
        }
      }
    }
    
    //CREATING A MATRIX OF MASK VALUES FROM NETCDF VARIABLE AND FINDING REGION BOUNDS AS IT IS CREATED
    //creating a big array of the mask numbers (as defined in some external file)
    //finding region bounds as we read into array (as in addTxtRegion)
    try 
    {
      NetcdfFile nc = NetcdfFile.open(fileName);
      /* Read a variable named ctry from the file, it contains the masks */
      Variable data = nc.findVariable(dataVar);
      Array ma2Array = data.read();
      maskArray = new int[(int)Math.ceil(180/res)][(int)Math.ceil(360/res)];
      Index in = ma2Array.getIndex();
      int i = 0;
      int k = 0;
      NaN = (int)data.findAttribute("missing_value").getNumericValue().floatValue();
      for(double y = (90-res); y >= -90; y-=res)
      {
        k = 0;
        for(double x = -180; x < 180; x+=res)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
          if(rblock == NaN)
          {
            maskArray[i][k] = 0;
          } else
          {
            maskArray[i][k] = rblock;
            if(rblock >= 0)
            { //updating someones bounds
              holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
              if(holdR.height==-1)
              { //this is the first block being added to this region nothing to
                // test against yet
                holdR.y = y;
                holdR.x = x;
                holdR.height = res;
                holdR.width = res;
              } else
              { //test against old bounds, if outside them, change them
                if(y<holdR.y)
                { //y will never be higher, only lower
                  holdR.height += (holdR.y-y);
                  holdR.y = y;
                }
                if(x<holdR.x)
                { //x may be higher or lower
                  holdR.width += (holdR.x-x);
                  holdR.x = x;
                }
                if((x+res)>(holdR.x+holdR.width))
                {
                  holdR.width = ((x+res)-holdR.x);
                }
              }
            }
          }
          k++;
        } //these two are kindof important, thats how we iterate through the data! huzzah!
        i++;
      }
      //DONE CREATING MASK MATRIX AND FINDING BOUNDS
      
      //initializing byte matrix for each region
      Iterator it = newRegions.entrySet().iterator();
      Map.Entry me;
      while(it.hasNext())
      {
        me = (Map.Entry)it.next();
        ((RegionMask)me.getValue()).makeMatrix();
      }
      //done initializing byte matricies
      
      //MAIN BYTEMASK CREATION LOOP
      i = 0;
      k = 0;
      for(double y = (90-res); y >= -90; y-=res)
      {
        k=0;
        for(double x = -180; x < 180; x+=res)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
          if((rblock > 0)&&(rblock != NaN))
          {
            holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
          	holdR.setPointTrue(x, y);
          }
          k++;
        }
        i++;
      }
      //DONE SETTING MASKS   
    } catch (java.io.IOException e) {
      System.out.println("Error reading NetCDF file -> "+fileName);
    }
    
    //adding these regions masks to the master list of masks
    Iterator it = newRegions.entrySet().iterator();
    Map.Entry me;
    while(it.hasNext())
    {
      me = (Map.Entry)it.next();
      holdR = ((RegionMask)me.getValue());
      regionList.add(holdR.name);
      maskList.put(holdR.name, holdR);
    }
    //done adding region masks
  }
  
  private void addNetCDFRegion2(Element currFile)
  {
    /*
     * ok... here we go, basically just extract a 2D array from the netcdf variable in the file
     * then run through it just like in addTxtRegion, should be snap
     */
    boolean getName = true;
    double res = 1;
    int nRegions = 0;
    int rblock, NaN;
    String fileName = "init";
    String dataVar = "ctry";
    String holdK;
    int maskArray[][];
    List infoList;
    Element currInfo;
    RegionMask holdR;
    TreeMap newRegions = new TreeMap();
    
  //getting file info from XML file
    infoList = currFile.getChildren();
    for(int i = 0; i < infoList.size(); i++)
    {
      currInfo = (Element)infoList.get(i);
      if(currInfo.getName().equals("res"))
      {
        res = Double.parseDouble(currInfo.getAttributeValue("value"));
      } else if(currInfo.getName().equals("name"))
      {
        fileName = currInfo.getAttributeValue("value");
      } else if(currInfo.getName().equals("variable"))
      {
        dataVar = currInfo.getAttributeValue("value");
      } else if(currInfo.getName().equals("region"))
      {
        List rList = currInfo.getChildren("RID");
        Element currR;
        for(int k = 0; k < rList.size(); k++)
        {
          currR = (Element)rList.get(k);
          //value and key are backwards cuz i copied stuff wrong, fix that TODO
          holdK = currR.getAttributeValue("value");
          holdR = new RegionMask(currR.getAttributeValue("key"), res);
          newRegions.put(holdK, holdR);
        }
      } else
      {
        System.out.println("Unknown File Tag in addNetCDFRegion -> "+currInfo.getName());
      }
    }
  //done getting file info from XML
    
    //CREATING A MATRIX OF MASK VALUES FROM NETCDF VARIABLE AND FINDING REGION BOUNDS AS IT IS CREATED
    //creating a big array of the mask numbers (as defined in some external file)
    //finding region bounds as we read into array (as in addTxtRegion)
    try 
    {
      NetcdfFile nc = NetcdfFile.open(fileName);
      /* Read a variable named ctry from the file, it contains the masks */
      Variable data = nc.findVariable(dataVar);
      Array ma2Array = data.read();
      maskArray = new int[(int)Math.ceil(180/res)][(int)Math.ceil(360/res)];
      Index in = ma2Array.getIndex();
      int i = 0;
      int k = 0;
      NaN = (int)data.findAttribute("missing_value").getNumericValue().floatValue();
      for(double y = (90-res); y >= -90; y-=res)
      {
        k = 0;
        for(double x = -180; x < 180; x+=res)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
          if(rblock == NaN)
          {
            maskArray[i][k] = 0;
          } else
          {
            maskArray[i][k] = rblock;
            if(rblock >= 0)
            { //updating someones bounds
              holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
              if(holdR.height==-1)
              { //this is the first block being added to this region nothing to
                // test against yet
                holdR.y = y;
                holdR.x = x;
                holdR.height = res;
                holdR.width = res;
              } else
              { //test against old bounds, if outside them, change them
                if(y<holdR.y)
                { //y will never be higher, only lower
                  holdR.height += (holdR.y-y);
                  holdR.y = y;
                }
                if(x<holdR.x)
                { //x may be higher or lower
                  holdR.width += (holdR.x-x);
                  holdR.x = x;
                }
                if((x+res)>(holdR.x+holdR.width))
                {
                  holdR.width = ((x+res)-holdR.x);
                }
              }
            }
          }
          k++;
        } //these two are kindof important, thats how we iterate through the data! huzzah!
        i++;
      }
      //DONE CREATING MASK MATRIX AND FINDING BOUNDS
      
      //initializing byte matrix for each region
      Iterator it = newRegions.entrySet().iterator();
      Map.Entry me;
      while(it.hasNext())
      {
        me = (Map.Entry)it.next();
        ((RegionMask)me.getValue()).makeMatrix();
      }
      //done initializing byte matricies
      
      //MAIN BYTEMASK CREATION LOOP
      i = 0;
      k = 0;
      for(double y = (90-res); y >= -90; y-=res)
      {
        k=0;
        for(double x = -180; x < 180; x+=res)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
          if((rblock > 0)&&(rblock != NaN))
          {
            holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
          	holdR.setPointTrue(x, y);
          }
          k++;
        }
        i++;
      }
      //DONE SETTING MASKS   
    } catch (java.io.IOException e) {
      System.out.println("Error reading NetCDF file -> "+fileName);
    }
    
    //adding these regions masks to the master list of masks
    Iterator it = newRegions.entrySet().iterator();
    Map.Entry me;
    while(it.hasNext())
    {
      me = (Map.Entry)it.next();
      holdR = ((RegionMask)me.getValue());
      regionList.add(holdR.name);
      maskList.put(holdR.name, holdR);
    }
    //done adding region masks
  }
  
//**********************************************************
//*********************Helper Functions*********************
//**********************************************************
  public void makeStreams()
  {
    //this function initializes all of the XML stream readers
    //i will add the code for additional readers as i need them
    XMLInputFactory inputFactory = XMLInputFactory.newInstance();
    try
    {
      SAXBuilder builder = new SAXBuilder();
      iDocument = builder.build(iSource);
      rDocument = builder.build(rSource);
      iReader = inputFactory.createXMLStreamReader(new java.io.FileInputStream(iSource));
      rReader = inputFactory.createXMLStreamReader(new java.io.FileInputStream(rSource));
      rWriter = new BufferedWriter( new java.io.FileWriter(outFile));
    } catch(FileNotFoundException e)
    {
      System.out.println("FileNotFound! oh noes! in -> makeStreams");
    } catch(XMLStreamException e)
    {
      System.out.println("the Stream is broken! oh noes! in -> makeStreams");
    } catch(IOException e)
    {
      System.out.println("IOException encountered! oh noes! in -> makeStreams");
    } catch(JDOMException e)
    {
      System.out.println("JDOM Exception! grarrrr! in -> makeStreams");
    }
  }
  private String readWord(BufferedReader input)
  {
    //reads an entire word from an input stream rather than just a character
    //words delimited by any whitespace 'space, new line, tab'
    String build = new String();
    int read;
    char hold;
    try {
      read = input.read();
      while((read != -1)&&(((hold = (char)read) == ' ')||(hold == '\n')||(hold == '\t')))
      {/*flushing whitespace from the input stream*/
        read = input.read();
      }
      build = build.concat(String.valueOf((char)read));
      while(((read = input.read()) != -1)&&((hold = (char)read) != ' ')&&(hold != '\n')&&(hold != '\t'))
      {
        build = build.concat(String.valueOf(hold));
      }
    } catch (IOException ex)
    {
      System.out.println("IOException!!!");
    }

    if(build.length() > 0)
      return build.trim();
    else
      return null;
  }
  private double scientificToDouble(String sc)
  {
    //takes a string of the form #.###E+### and converts it to a double
    double mantissa, exponent, expValue;
    boolean expSignPos = false;
    int E = sc.indexOf('E');
    mantissa = Double.parseDouble(sc.substring(0, E));
    if(sc.substring(E+1, E+2).equals("+"))
      expSignPos = true;
    exponent = Double.parseDouble(sc.substring(E+2, sc.length()));
    if(expSignPos)
      expValue = Math.pow(10, exponent);
    else
      expValue = Math.pow(10, (-1*exponent));
    
    if(mantissa != 0)
    {
      return mantissa*expValue;
    }
    else
    {
      return 0;
    }
  }
  
}
