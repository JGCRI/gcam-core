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
 * \file Preprocess.java
 * \ingroup Preprocess
 * \brief Driver class for preprocessor, runs based on supplied XML files.
 *
 *  This class runs based on the supplied XML files which define a list of files which
 * contain data, files which define the regions that data will be split into, and the
 * destination for all the output data in an xml file. Works by building a 
 * QuadBucketTree of data, reading in all the regions, extracting regions worth of data
 * from the tree, then printing normalized regions to xml.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
/* DEVELOPER COMMENTS
 * This is a very memory intensive application. the -Xmx argument to the JVM should be set to 128M to be safe at res=1
 * actual usage at resolution of 1 is approx 70M, at higher resolutions this will grow drastically
 */

/**
 * TODO: nothing currently
 */
package ModelInterface.PPsource;


import java.io.*;
import java.net.URL;
import java.util.*;
import java.util.logging.*;
import java.awt.geom.*;

import ucar.ma2.*;
import ucar.nc2.*;
import org.jdom.*;
import org.jdom.input.*;
import org.jdom.output.*;
import org.geotools.data.*;
import org.geotools.data.shapefile.*;
import org.geotools.data.shapefile.shp.*;
import org.geotools.feature.*;
import org.geotools.filter.AreaFunction;

import com.vividsolutions.jts.geom.*;

import com.sun.media.jai.codec.FileSeekableStream;

import org.geotiff.image.jai.*;
import org.libtiff.jai.codec.*;

import ModelInterface.PPsource.util.HomolosineToDegreeConversion;

/**
 * Driver class for preprocessor, runs based on supplied XML files.This class runs based on
 * the supplied XML files which define a list of files which
 * contain data, files which define the regions that data will be split into, and the
 * destination for all the output data in an xml file. Works by building a 
 * QuadBucketTree of data, reading in all the regions, extracting regions worth of data
 * from the tree, then printing normalized regions to xml.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class DataBuilder
{
  public DataIndex dataStruct; //a tree of all the aggregated data to be used to create regions
  public TreeSet regionList; //a list of all the defined regions' names
  public TreeMap maskList; //all of the regionmasks which will be used to extract regions of data
  public TreeMap printList; //a holder of unwrapped regions before they are printed
  public TreeMap dataAvg; //a listing for each data type as to wether it is averaged or added
  public TreeMap dataRef; //a listing of the references for each data type for which one was supplied
  public TreeMap dataUnits; //a listing of the units which each variables values represent, for use in DM
  private String iSource; //the name of the data input XML file
  private String sSource; //the name of the seed XML file
  private String rSource; //the name of the region definition XML file
  private String outFile; //the name of the XML file to be outputed to
  private Document iDocument; //the main input Document from which other data files are called
  private Document rDocument; //the region Document which all region files are called form
  private Document sDocument; //the seed data which will be added to
  private BufferedWriter rWriter; //the writer to the output file
  private boolean init; //whether or not the dataStruct has been initialized as of yet
  private boolean URes; //whether or not the User wishes to use their own resolution
  Logger log = Logger.getLogger("Preprocess"); //log class to use for all logging output

//*****************************************************************************
//*****************Class Constructors******************************************
//*****************************************************************************
  
  /**
   * Default constructor. Uses as the data file 'source.xml', as the
   * region file 'regions.xml' and as the output file 'regionTrees.xml'.
   */
  public DataBuilder()
  {
    init = false;
    URes = false;
    iSource = "sources.xml";
    rSource = "regions.xml";
    outFile = "out.xml";
    log.log(Level.CONFIG, "DataBuilder seed files: sources.xml, regions.xml, out.xml");
    dataStruct = new FlatIndex(-180, 180, -90, 90);
    regionList = new TreeSet();
    maskList = new TreeMap();
    printList = new TreeMap();
    dataAvg = new TreeMap();
    dataRef = new TreeMap();
    dataUnits = new TreeMap();
  }
  /**
   * Standard file name constructor. Takes in three file names to later
   * run based on their content.
   * 
   * @param i XML file which contains input data.
   * @param r XML file which defines regions.
   * @param o Destination file for XML output.
   */
  public DataBuilder(String i, String r, String o)
  {
    init = false;
    URes = false;
    iSource = i;
    rSource = r;
    outFile = o;
    log.log(Level.CONFIG, "DataBuilder seed files: "+i+", "+r+", "+o);
    //can change the Index type used to change memory behavior
    dataStruct = new FlatIndex(-180, 180, -90, 90);
    regionList = new TreeSet();
    maskList = new TreeMap();
    printList = new TreeMap();
    dataAvg = new TreeMap();
    dataRef = new TreeMap();
    dataUnits = new TreeMap();
  }
  
//*****************************************************************************
//*************Begin Functions Proper******************************************
//*****************************************************************************
  /**
   * Wrapper function which executes all others in the correct order.
   * Allows user to only call runAll to gather input and generate
   * output. runAll correctly accounts for both a regular run and the
   * option of aggregating datafiles into a passed seed file.
   */
  public void runAll()
  {
    /*
    try{
      System.out.println("BEGIN: waiting...");
      System.in.read();
      System.in.read();
      System.out.println("...going");
    } catch(IOException e) {}
    */
    log.log(Level.FINE, "Calling makeStreams()");
    makeStreams();
    
    Element root = iDocument.getRootElement();
    Element seed = root.getChild("seed");
    if(seed != null)
    { //we have a seed tree of data, add the new data to it
      log.log(Level.INFO, "running with seed data");
      log.log(Level.FINE, "Calling readCurrentData()");
      readCurrentData();
      //if we have a seed tree must use that resolution
      URes = true;
      double userRes = Double.parseDouble(sDocument.getRootElement().getAttributeValue("res"));
      dataStruct.fillWorld(userRes);
      init = true;
      //done setting res
      log.log(Level.FINE, "Calling buildTree()");
      buildTree();
      log.log(Level.FINE, "Calling readMasks()");
      readMasks();
      log.log(Level.FINE, "Calling fillRegions()");
      fillRegions();
      log.log(Level.FINE, "Calling addNewData()");
      addNewData();
      log.log(Level.FINE, "Calling writeAggregatedTree()");
      writeAggregatedTree();
    } else
    { //this is an original data run, output the data
      log.log(Level.INFO, "running with NO seed data");
      log.log(Level.FINE, "Calling buildTree()");
      buildTree();
      log.log(Level.FINE, "Calling readMasks()");
      readMasks();
      log.log(Level.FINE, "Calling fillRegions()");
      fillRegions();
      try
      {
        rWriter = new BufferedWriter( new java.io.FileWriter(outFile));
      } catch(IOException e)
      {
        log.log(Level.SEVERE, "IOException in -> runall");
      }
      log.log(Level.FINE, "Calling writeRegions()");
      writeRegions();
    }
    /*
    try{
      System.out.println("END: waiting...");
      System.in.read();
      System.in.read();
      System.out.println("...going");
    } catch(IOException e) {}
    */
  }
  
  /**
   * Reads all listed data files into a {@link QuadBucketTree}.
   * Opens every file listed in the XML data file seperately and
   * creates a new QuadBucketTree from the data before merging with the
   * existing master tree.
   */
  public void buildTree()
  {
    Element currFile;
    Element root = iDocument.getRootElement();
    //List fileChildren = root.getChildren("file");
    List fileChildren = root.getChildren();
    
    if((!init)&&(Double.parseDouble(root.getAttributeValue("resolution")) > 0))
    { //getting a user resolution if supplied
      URes = true;
      double userRes = Double.parseDouble(root.getAttributeValue("resolution"));
      log.log(Level.FINER, "calling fillWorld to create access structure");
      dataStruct.fillWorld(userRes);
      log.log(Level.INFO, "data structure initialized with "+userRes+" resolution");
      init = true;
    }
    
    
    //MAIN XML READ LOOP FOR QBTREE BUILDING
    for(int i = 0; i < fileChildren.size(); i++)
    {
      /*
      try{
        System.out.println("FILE: waiting...");
        System.in.read();
        System.in.read();
        System.out.println("...going");
      } catch(IOException e) {}
      */
      currFile = (Element)fileChildren.get(i);
      // TODO: redo this so that it doesn't call getAttributeValue everytime
      if(currFile.getName().equals("setTrackSums")) {
	      dataStruct.setTrackSums(Boolean.valueOf(currFile.getAttributeValue("value")));
      } else if(currFile.getName().equals("printSums")) {
	      dataStruct.printSums();
      } else if(currFile.getName().equals("seed")) {
	      // skip this
      } else if(currFile.getAttributeValue("type").equals("txt"))
      {
        Element stor = currFile.getChild("storage");
        if(stor != null)
        {
          if(stor.getAttributeValue("type").equals("values"))
          { //we are just storing the values in the shapefile normally
            addTxtData(currFile);
          } else
          { //the values are enumerated types, each gets its own variable
            //values are the percent coverage for each block
            addTxtEnum(currFile);
          }
        } else
        {
          addTxtData(currFile);
        }
      } else if(currFile.getAttributeValue("type").equals("UNEP"))
      {
        addUNEPData(currFile);
      } else if(currFile.getAttributeValue("type").equals("1x1"))
      {
        add1x1Data(currFile);
      } else if(currFile.getAttributeValue("type").equals("ASC"))
      {
        addASCData(currFile);
      } else if(currFile.getAttributeValue("type").equals("NASA"))
      {
        addNASAData(currFile);
      } else if(currFile.getAttributeValue("type").equals("netcdf"))
      {
        Element stor = currFile.getChild("storage");
        if(stor != null)
        {
          if(stor.getAttributeValue("type").equals("values"))
          { //we are just storing the values in the shapefile normally
            addNetCDFData(currFile);
          } else
          { //the values are enumerated types, each gets its own variable
            //values are the percent coverage for each block
            addNetCDFEnum(currFile);
          }
        } else
        {
          addNetCDFData(currFile);
        }
      } else if(currFile.getAttributeValue("type").equals("pointShapefile"))
      {
        Element stor = currFile.getChild("storage");
        if(stor.getAttributeValue("type").equals("values"))
        { //we are just storing the values in the shapefile normally
          addPointShapeFileData(currFile);
        } else
        { //the values are enumerated types, each gets its own variable
          //values are the percent coverage for each block
          addPointShapeFileEnum(currFile);
        }
      } else if(currFile.getAttributeValue("type").equals("polygonShapefile"))
      {
        Element stor = currFile.getChild("storage");
        if(stor.getAttributeValue("type").equals("values"))
        { //we are just storing the values in the shapefile normally
          addPolyShapeFileData(currFile);
        } else
        { //the values are enumerated types, each gets its own variable
          //values are the percent coverage for each block
          addPolyShapeFileEnum(currFile);
        }
        
      } else if(currFile.getAttributeValue("type").equals("raster"))
      {
        addRasterData(currFile);
      } else if(currFile.getAttributeValue("type").equals("geoTiff"))
      {
	      addGeoTiffFile(currFile);
      } else if(currFile.getAttributeValue("type").equals("flt"))
      {
	      addFLTFile(currFile);
      } else
      {
        log.log(Level.WARNING, "Unsupported File Type -> "+currFile.getAttributeValue(null, "type"));
      }
    }
    //END MAIN XML LOOP FOR TREE BUILD
  }
  /**
   * Creates a list of {@link RegionMask}s from listed files.
   * Region information is supplied in the XML file which runs this
   * portion.
   */
  public void readMasks()
  {	//redoing readMasks using a DOM parser instead of StAX, this is for ease of changes later
    //and because i believe the code will be much more solid and able to prevent user errors
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
    Element root = rDocument.getRootElement();
    List fileChildren = root.getChildren("file");
    Element currChild;

    //BEGIN MAIN XML LOOP FOR READING IN REGION MASKS
    for(int i = 0; i < fileChildren.size(); i++)
    {
      currChild = (Element)fileChildren.get(i);
      if(currChild.getAttributeValue("type").equals("txt"))
      {
        addTxtRegion(currChild);
      } else if(currChild.getAttributeValue("type").equals("netcdf"))
      {
        addNetCDFRegion(currChild);
      } else if(currChild.getAttributeValue("type").equals("unep"))
      {
        addUNEPRegion(currChild);
      } else
      {
        log.log(Level.WARNING, "Unsupported File Type -> "+currChild.getAttributeValue(null, "type"));
      }
    }
    //END REGION MASK READING LOOP
  }
  /**
   * Extracts a region's worth of data from the {@link QuadBucketTree} for
   * each {@lnk RegionMask} in the list. This data is a tree hierarchy of
   * matrices of doubles.
   *
   */
  public void fillRegions()
  {
    //for each region in 'regionList' use the mask in 'maskList' to get data out of
    //'dataStruct' using the extractMask function, store this information in printList
    Map holdToPrint;
    String rName;
    Iterator it = regionList.iterator();
    while(it.hasNext())
    {
      rName = (String)it.next();
      //System.out.println("filling "+rName);
      holdToPrint = dataStruct.extractMask((RegionMask)maskList.get(rName));
      if(holdToPrint != null)
      {
        printList.put(rName, holdToPrint);
      }
    }
    //guess what, the treeMap's in printList are really just a region without a wrapper
    //congratulations the actual work of the proprocess stage is done once this works
    //yeah... i just congratulated myself... in comments
  }
  /**
   * Writes an XML structure which defines each region and its corresponding
   * data. Does as much computation as possible in terms of matrix bounds and
   * indicies so as to remove that burden from the reading program.
   *
   */
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
    Map holdToPrint;
    String rName, holder;
    Iterator itName, itVar, itTime, itData;
    int msizeX, msizeY;
    double normX, normY, normW, normH; //the normalized bounds of the region (rounded to a multiple of data resolution)
    double holdMX, holdMY, work;
    Map.Entry var, time, data;
    try
    {
      //xml header information
      rWriter.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>");
      rWriter.newLine();
      rWriter.write("<input num=\""+regionList.size()+"\" res=\""+dataStruct.getResolution()+"\">");
      rWriter.newLine();
      
      //outputting the global data on variables (avg, units, reference)
      rWriter.write("\t<variableInfo>");
      rWriter.newLine();
      itVar = dataAvg.entrySet().iterator();
      while(itVar.hasNext())
      {
        var = (Map.Entry)itVar.next();
        rWriter.write("\t\t<variable name=\""+var.getKey()+"\">");
	rWriter.newLine();
        rWriter.write("\t\t\t<average value=\""+var.getValue()+"\" />");
	rWriter.newLine();
        if(dataUnits.containsKey(var.getKey()))
        {
          rWriter.write("\t\t\t<units value=\""+dataUnits.get(var.getKey())+"\" />");
	  rWriter.newLine();
        }
        if(dataRef.containsKey(var.getKey()))
        {
          rWriter.write("\t\t\t<reference value=\""+dataRef.get(var.getKey())+"\" />");
	  rWriter.newLine();
        }
        rWriter.write("\t\t</variable>");
	rWriter.newLine();
      }
      rWriter.write("\t</variableInfo>");
      rWriter.newLine();
      
      
      itName = regionList.iterator(); //iterates through names
    //BEGIN MAIN REGION WRITING LOOP
      while(itName.hasNext())
      {
        rName = (String)itName.next();
        holdToPrint = (Map)printList.get(rName);
        if(holdToPrint != null)
        {
          if(!holdToPrint.isEmpty())
          {
          //BEGIN WRITING OF SPECIFIC REGION DATA
            //calculating normalized bounds of region
            work = ((RegionMask)maskList.get(rName)).x+((RegionMask)maskList.get(rName)).width;
            holdMX = Math.ceil(work/dataStruct.getResolution())*dataStruct.getResolution();
            work = ((RegionMask)maskList.get(rName)).y+((RegionMask)maskList.get(rName)).height;
            holdMY = Math.ceil(work/dataStruct.getResolution())*dataStruct.getResolution();
            normX = Math.floor(((RegionMask)maskList.get(rName)).x/dataStruct.getResolution())*dataStruct.getResolution();
            normY = Math.floor(((RegionMask)maskList.get(rName)).y/dataStruct.getResolution())*dataStruct.getResolution();
            normW = holdMX-normX;
            normH = holdMY-normY;
            msizeY = (int)Math.ceil(normH/dataStruct.getResolution());
            msizeX = (int)Math.ceil(normW/dataStruct.getResolution());
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
              if(var.getKey() == "weight")
              { //weight is output not as a regular variable
                holder = "<weight>";
              } else
              { //normal case
                holder = "<variable value=\""+var.getKey()+"\">";
              }
              rWriter.write("\t\t"+holder);
              rWriter.newLine();
              //iterate through times
              itTime = ((Map)var.getValue()).entrySet().iterator();
              while(itTime.hasNext())
              {
                time = (Map.Entry)itTime.next();
                holder = "<time value=\""+time.getKey()+"\">";
                rWriter.write("\t\t\t"+holder);
                rWriter.newLine();
                //iterate through data, print in array style
                itData = ((Map)time.getValue()).entrySet().iterator();
                while(itData.hasNext())
                {
                  data = (Map.Entry)itData.next();
		  if(!((Double)data.getValue()).isNaN()) {
			  //prints yIndex xIndex dataValue
			  holder = "<data y=\""
				  +(Math.abs((int)(((((Point2D.Double)data.getKey()).y+dataStruct.getResolution())-(normY+normH))/dataStruct.getResolution())));
			  holder += "\" x=\""+(int)((((Point2D.Double)data.getKey()).x-normX)/dataStruct.getResolution())
				  +"\" value=\"";
			  //holder = "<data y=\""+(int)((Point2D.Double)data.getKey()).y;
			  //holder += "\" x=\""+(int)((Point2D.Double)data.getKey()).x+"\" value=\"";
			  holder += data.getValue()+"\" />";
			  rWriter.write("\t\t\t\t"+holder);
			  rWriter.newLine();
		  }
                }
                rWriter.write("\t\t\t</time>");
		rWriter.newLine();
                rWriter.flush();
              }
              if(var.getKey() == "weight")
              { //weight is output not as a regular variable
                rWriter.write("\t\t</weight>");
		rWriter.newLine();
              } else
              { //normal case
                rWriter.write("\t\t</variable>");
		rWriter.newLine();
              }  
            }
            rWriter.write("\t</region>");
	    rWriter.newLine();
            //END WRITING REGION
          }
        }
      }
    //END WRITING LOOP
      rWriter.write("</input>");
      rWriter.newLine();
      rWriter.flush();
      rWriter.close();
    } catch (IOException e){}
  }
  /**
   * Gets location of the seed data from input xml file and creates a Document
   * object from that file which is returned. Used to add a set of data to an
   * already created larger set of data.
   * @return Document which contains previous data.
   */
  public void readCurrentData()
  {
    Document docMerge;
    List nodeMerge;
    Element root = iDocument.getRootElement();
    List seeds = root.getChildren("seed");
    Element seed = root.getChild("seed");
    
    //XMLInputFactory inputFactory = XMLInputFactory.newInstance();
    try
    {
      SAXBuilder builder = new SAXBuilder();
      
      sSource = seed.getAttributeValue("name");
      sDocument = builder.build(sSource);
      for(int i = 1; i < seeds.size(); i++)
      {
        //reading any additional seed files and merging into the first seed
        //this currently performs an insanely simple merge
        //alot more work is needed to make this work in all cases
        //TODO: different resolutions, make sure regions go in existing regions, etc.
        seed = (Element)seeds.get(i);
        sSource = seed.getAttributeValue("name");
        docMerge = builder.build(sSource);
        nodeMerge = docMerge.cloneContent();
        sDocument.addContent(nodeMerge);
      }
    } catch(FileNotFoundException e)
    {
      log.log(Level.SEVERE, "FileNotFound! in -> readCurrentData");
    } catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException encountered! in -> readCurrentData");
    } catch(JDOMException e)
    {
      log.log(Level.SEVERE, "JDOM Exception! in -> readCurrentData");
    }
  }
  /**
   * Adds all data read by the program into a pre-created xml document.
   * This allows new data to be added to large aggregated sets without as 
   * large a computation time.
   * @param data Document which new data will be added to.
   */
  public void addNewData()
  {
    /*
     * First need to find out which variable already exist, if a variable does not already exist
     * then add it to the variableInfo element with all of its data. If it does exist dont add info there
     * maybe keep track of which exist and dont exist... no dont. For each region which would normally be written
     * get that region out of the current tree and add data elements as appropriate
     * check to see if the variable already exists, if it doesnt then add a variable element, if it does
     * exist then get that element and add a time element to it.
     * 
     * how find out if elements exist??? list and search through by attribute name? (slow)
     */
    Iterator itName, itVar, itTime, itData;
    double normX, normY, normH; //the normalized bounds of the region (rounded to a multiple of data resolution)
    double holdMY, work;
    Map.Entry var, time, data;
    Element root = sDocument.getRootElement();
    Element currElem;
    Element currReg, currVar;
    Element toAdd, toChild, toChild2;
    String rName, vName, tName;
    Map holdToPrint;
    
//adding to variableInfo*******************************************************
    currElem = root.getChild("variableInfo");
    itVar = dataAvg.entrySet().iterator();
    while(itVar.hasNext())
    {
      var = (Map.Entry)itVar.next();
      String thisVar = (String)var.getKey();
      
      if(attributeInChild(currElem, "name", thisVar) == null)
      { //There is no entry for this variable as of yet, enter it!
        toAdd = new Element("variable");
        toAdd.setAttribute("name", thisVar);
        
        //every entry will have average or add information, so add it
        toChild = new Element("average");
        toChild.setAttribute("value", ((Boolean)dataAvg.get(thisVar)).toString());
        toAdd.addContent(toChild);
        
        if(dataRef.containsKey(thisVar))
        { //variable has a reference, add it to list
          toChild = new Element("reference");
          toChild.setAttribute("value", (String)dataRef.get(thisVar));
          toAdd.addContent(toChild);
        }
        
        if(dataUnits.containsKey(thisVar))
        { //variable has units, add it to list
          toChild = new Element("units");
          toChild.setAttribute("value", (String)dataUnits.get(thisVar));
          toAdd.addContent(toChild);
        }
        
        //done creating variable entry, add it
        currElem.addContent(toAdd);
      }
    }
//variableInfo updated*********************************************************
    
//running through each region and adding a variable or time where necessary****
    itName = regionList.iterator(); //iterates through regions
    while(itName.hasNext())
    {
      rName = (String)itName.next();
      holdToPrint = (LinkedHashMap)printList.get(rName);
      currReg = attributeInChild(root, "name", rName); //getting region node in currReg
      if(currReg == null) {
	      // this region is not in this seed file.
	      continue;
      }
      if(holdToPrint != null)
      {
        if(!holdToPrint.isEmpty())
        {
          //BEGIN ADDING OF SPECIFIC REGION DATA
          //calculating normalized bounds of region
          work = ((RegionMask)maskList.get(rName)).x
              +((RegionMask)maskList.get(rName)).width;
          //holdMX = Math.ceil(work/dataStruct.getResolution())*dataStruct.getResolution();
          work = ((RegionMask)maskList.get(rName)).y
              +((RegionMask)maskList.get(rName)).height;
          holdMY = Math.ceil(work/dataStruct.getResolution())*dataStruct.getResolution();
          normX = Math.floor(((RegionMask)maskList.get(rName)).x/dataStruct.getResolution())
              *dataStruct.getResolution();
          normY = Math.floor(((RegionMask)maskList.get(rName)).y/dataStruct.getResolution())
              *dataStruct.getResolution();
          //normW = holdMX-normX;
          normH = holdMY-normY;
          //msizeY = (int)Math.ceil(normH/dataStruct.getResolution());
          //msizeX = (int)Math.ceil(normW/dataStruct.getResolution());

          //iterate through variables
          itVar = holdToPrint.entrySet().iterator();
          while(itVar.hasNext())
          {
            var = (Map.Entry)itVar.next();
            vName = (String)var.getKey();
            if(vName!="weight")
            { //we never want to add anything to the weight variable if it already exists
              //test to see if variable already exists in this region
              currVar = attributeInChild(currReg, "value", vName);
              if(currVar==null)
              { //there is no entry for this variable (field) create one and add
                // the time
                toAdd = new Element("variable");
                toAdd.setAttribute("value", vName);

                //iterating through times
                itTime = ((LinkedHashMap)var.getValue()).entrySet().iterator();
                while(itTime.hasNext())
                {
                  time = (Map.Entry)itTime.next();
                  tName = ((String)time.getKey());
                  toChild = new Element("time");
                  toChild.setAttribute("value", tName);

                  //iterate through data, print in array style
                  itData = ((LinkedHashMap)time.getValue()).entrySet().iterator();
                  while(itData.hasNext())
                  {
                    data = (Map.Entry)itData.next();
		    if(!((Double)data.getValue()).isNaN()) {
			    toChild2 = new Element("data");
			    toChild2.setAttribute("y",String.valueOf(Math
						    .abs((int)(((((Point2D.Double)data.getKey()).y+dataStruct.getResolution())
									    -(normY+normH))/dataStruct.getResolution()))));
			    toChild2.setAttribute("x", String.valueOf((int)((((Point2D.Double)data
									    .getKey()).x-normX)/dataStruct.getResolution())));
			    toChild2.setAttribute("value", ((Double)data.getValue()).toString());
			    //adding the created data to this time
			    toChild.addContent(toChild2);
		    }
                  }
                  //adding the created time to this variable
                  toAdd.addContent(toChild);
                }
                //adding the created variable to this region
                currReg.addContent(toAdd);
              } else
              { //this variable already exists, add the time to it
                //iterating through times
                itTime = ((LinkedHashMap)var.getValue()).entrySet().iterator();
                while(itTime.hasNext())
                {
                  time = (Map.Entry)itTime.next();
                  tName = time.getKey().toString();
                  if((toAdd = attributeInChild(currVar, "value", tName)) == null)
                  {
                    toAdd = new Element("time");
                    toAdd.setAttribute("value", tName);

                    //iterate through data, print in array style
                    itData = ((LinkedHashMap)time.getValue()).entrySet().iterator();
                    while(itData.hasNext())
                    {
                      data = (Map.Entry)itData.next();
		      if(!((Double)data.getValue()).isNaN()) {
			      toChild = new Element("data");
			      toChild.setAttribute("y",String.valueOf(Math
					      .abs((int)(((((Point2D.Double)data.getKey()).y+dataStruct.getResolution())-(normY+normH))/dataStruct.getResolution()))));
			      toChild.setAttribute("x", String.valueOf((int)((((Point2D.Double)data
								      .getKey()).x-normX)/dataStruct.getResolution())));
			      toChild.setAttribute("value", ((Double)data.getValue()).toString());
			      //adding the created data to this time
			      toAdd.addContent(toChild);
		      }
                    }
                    //adding the created time to this variable
                    currVar.addContent(toAdd);
                  } else
                  { //this time already exists! Will overwrite any new values
                    //iterate through data, print in array style
                    itData = ((LinkedHashMap)time.getValue()).entrySet().iterator();
		    List children = toAdd.getChildren();
		    //log.log(Level.FINER, "Before processing all");
                    while(itData.hasNext())
                    {
                      data = (Map.Entry)itData.next();
                      String currY = String.valueOf((Math
                          .abs((int)(((((Point2D.Double)data.getKey()).y+dataStruct.getResolution())-(normY+normH))/dataStruct.getResolution()))));
                      String currX = String.valueOf(((int)((((Point2D.Double)data
                          .getKey()).x-normX)/dataStruct.getResolution())));
		      toChild = null;
		      //for(int childPos = 0; childPos < children.size() && toChild == null; ++childPos) {
		      //log.log(Level.FINER, "Before loop");
		      for(Iterator it = children.iterator(); it.hasNext() && toChild == null; ) {
			      //Element currChild = (Element)children.get(childPos);
			      Element currChild = (Element)it.next();
			      if(currChild.getAttributeValue("y").equals(currY) && currChild.getAttributeValue("x").equals(currX)) {
				      toChild = currChild;
				      //log.log(Level.FINER, "Found It: "+toChild);
			      }
		      }
		      //log.log(Level.FINER, "After loop");
		      /*
		      if(toChild == null) {
			      // didn't have the value before so we will have to create it
			      toChild = new Element("data");
			      toChild.setAttribute("y", currY);
			      toChild.setAttribute("x", currX);
			      toChild.setAttribute("value", "NaN");
		      } else {
			      log.log(Level.FINER, "Got back "+toChild);
		      }
		      */
		      //log.log(Level.FINER, "before if");
		      //if(!((Double)data.getValue()).isNaN() || (toChild != null && !toChild.getAttribute("value").equals("NaN"))) {
		      if(!((Double)data.getValue()).isNaN()) {
			      if(toChild == null) {
				      //log.log(Level.FINER, "has to create data");
				      // for efficiencies sake avoid creating this 
				      toChild = new Element("data");
				      toChild.setAttribute("y", currY);
				      toChild.setAttribute("x", currX);
				      toAdd.addContent(toChild);
			      }
			      //log.log(Level.FINER, "going to set data");
			      toChild.setAttribute("value", data.getValue().toString());
		      }
		      // if toChild is still NaN should I just delete it?
		      //log.log(Level.FINER, "after if");
		    }
		    //log.log(Level.FINER, "After processing all");
                  }
                }
              }
            }
          }//END VARIABLE ITERATE
        }//END WRITING REGION
      }
    }
//done adding all data to regions**********************************************
  }
  /**
   * After creating a new xml tree of data by aggregating the previous tree and
   * new data sets, writes that tree to a file. Thats about it.
   *
   */
  public void writeAggregatedTree()
  {
    XMLOutputter out = new XMLOutputter(Format.getPrettyFormat());
    
    if(outFile.equals(sSource))
    {
      log.log(Level.WARNING, "Seed file equals Output file. Appending '2' to Output file name.");
      int p = outFile.indexOf(".");
      outFile = outFile.substring(0, p)+"2"+outFile.substring(p,outFile.length());
    }
    
    try
    {
      rWriter = new BufferedWriter( new java.io.FileWriter(outFile));
      out.output(sDocument, rWriter);
    } catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException in -> writeAggregatedTree");
    }
  }
  
//*****************************************************************************
//********************Component Readers****************************************
//*****************************************************************************
  
  private void addTxtData(Element currFile)
  { /* function will add the data from the specified file of type 'txt'
     * 'txt'- defined as 180/resolution lines of 360/resolution data elements
     * tagged or untagged - basically using this to debug program as i build it
     */
    log.log(Level.FINER, "begin function");
    boolean tagged = true;
    boolean avg = true;
    boolean dec = true;
    boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    int skipLines = 0;
    double startLat = 90;
    double endLat = -90;
    double NaN = -9999;
    
    //check if tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(currFile.getAttributeValue("tagged"))).booleanValue();
    
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
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("NaN"))
      {
        NaN = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("skip-lines"))
      {
        skipLines = Integer.parseInt(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("start-latitude"))
      {
        startLat = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("end-latitude"))
      {
        endLat = Double.parseDouble(currElem.getAttributeValue("value"));
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
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
      System.exit(1);
    }
  //txt file opened
    
    // skip lines if necessary before reading any values
    try {
	    for(int currLine = 0; currLine < skipLines; ++currLine) {
		    String skippedLine = input.readLine();
		    log.log(Level.FINER, "Skipped: "+skippedLine);
	    }
    } catch(IOException ioe) {
	    log.log(Level.SEVERE, "Error reading data: "+ioe);
	    System.exit(1);
    }

    
    
    if(tagged)
    { //if didnt get these before in the xml file
      dataName = readWord(input);
      time = Double.parseDouble(readWord(input));
    	res = Double.parseDouble(readWord(input));
    	if(readWord(input).equals("false"))
    	{
    	  avg = false;
    	}
    	if(readWord(input).equals("scientific"))
    	{
    	  dec = false;
    	}
    	unit = readWord(input);
    	try{
    	  ref = input.readLine();
    	}catch(IOException e) {}
    }
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
    
    
    
    
    double curri =0;
    double currk =0;
    try {
  //reading the data from the file
    for(double i = (startLat-res); i >= endLat; i-=res)
    {
      for(double k = -180; k < 179.9999; k+=res)
      {
	      curri = i;
	      currk = k;
        //System.out.println(k);
        if(dec)
        { //numbers stored in decimal format
          dataValue = Double.valueOf(readWord(input));
        } else
        { //numbers stored in scientific notation
          dataValue = new Double(scientificToDouble(readWord(input)));
        }
	if(dataValue != NaN) {
        toAdd = new DataBlock(k, i, res, res);
        timeValue = new TreeMap();
        timeValue.put(time, dataValue);
        
        //check overwrite bit, if so, use hold instead of dataName
        if(overwrite)
        {
          //just replace name with hold, later, we will merge hold over old data
          toAdd.data.put("hold", timeValue);
        } else
        {
          //add data as normal
          toAdd.data.put(dataName, timeValue);
        }
        
      //merging this data into the current tree
        dataStruct.addData(toAdd, avg);
      }
      }
    }
  } catch(NullPointerException e) {
	  log.log(Level.WARNING, "Error reading data at (lat,long) ("+curri+", "+currk+"): "+e);
	  e.printStackTrace();
  }
    
    //done adding all data, if overwrite, must merge with old data now
    if(overwrite)
    {
      dataStruct.resolveOverwrite("hold", dataName);
    } //else we are done already
    
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
  }
  
  private void addUNEPData(Element currFile)
  {
    //Data from the United Nations Environment Program
    //txt file with some specific tags in the beginning
    //these tags tell where the data is and what resolution it is stored at
    log.log(Level.FINER, "begin function");
    boolean avg = true;
    boolean dec = true;
    boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String ref = null;
    String unit = null;
    int numCols, numRows;
    double xLL, yLL;
    double currX, currY;
    double ignore;
    double time = 0;
    double res = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    
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
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
    //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
      System.exit(0);
    }
  //txt file opened
    
    
    /*
     * read numcols
     * numrows
     * xll corner
     * yll corner
     * cellsize (res)
     * NODATA_value
     */
    
    double shift;
    readWord(input); //reading ncols
    numCols = Integer.valueOf(readNumber(input));
    //System.out.println("numCols "+numCols);
    readWord(input); //reading nrows
    numRows = Integer.valueOf(readNumber(input));
    //System.out.println("numRows "+numRows);
    readWord(input); //reading xllcorner
    xLL = Double.valueOf(readNumber(input));
    //System.out.println("xLL "+xLL);
    readWord(input); //reading yllcorner
    yLL = Double.valueOf(readNumber(input));
    //System.out.println("yLL "+yLL);
    readWord(input); //reading cellsize
    res = Double.valueOf(readNumber(input));
    res = 360.0 / numCols;
    //System.out.println("res "+res);
    readWord(input); //reading NODATA_value
    ignore = Double.valueOf(readNumber(input));
    //System.out.println("ignore "+ignore);
    
    //rectifying bounds if they are out of legal values
    if(xLL < -180)
    {
      shift = (xLL*-1)-180;
      xLL = -180;
      log.log(Level.WARNING, dataName+" shifted right by "+shift+" to "+xLL);
    } else
    {
      shift = (xLL+(numCols*res))-180;
      if(shift > 0)
      {
        xLL -= shift;
        log.log(Level.WARNING, dataName+" shifted left by "+shift+" to "+xLL);
      }
    }
    if(yLL < -90)
    {
      shift = (xLL*-1)-90;
      yLL = -90;
      log.log(Level.WARNING, dataName+" shifted up by "+shift+" to "+yLL);
    } else
    {
      shift = (yLL+((numRows+1)*res))-90;
      if(shift > 0)
      {
        yLL -= shift;
        log.log(Level.WARNING, dataName+" shifted down by "+shift+" to "+yLL);
      }
    }
    //done rectifying bounds
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
    
    
    //reading the data from the file
    currY = (yLL+(numRows*res));
    for(int i = 0; i < numRows; i++)
    {
      currX = xLL;
      for(int k = 0; k <numCols; k++)
      {
        dataValue = Double.valueOf(readNumber(input));
        
        /**
         * SPECIAL CASE-- for the tree data, this will take out values 255 and 254
         * which correspond essentially to 0
         */
        if(dataName.equals("ForestCover"))
        {
          if((dataValue == 255)||(dataValue == 254))
          {
            dataValue = new Double(0);
          }
        }
        
        
        
        //only add this data if it is not the ignore value
        if(dataValue != ignore)
        {
          toAdd = new DataBlock(currX, currY, res, res);
          timeValue = new TreeMap();
          timeValue.put(time, dataValue);
          
          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite)
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put("hold", timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(dataName, timeValue);
          }
          
        //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
        }
        currX += res;
      }
      currY -= res;
    }
    
    //done adding all data, if overwrite, must merge with old data now
    if(overwrite)
    {
      dataStruct.resolveOverwrite("hold", dataName);
    } //else we are done already
    
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
  }
  
  private void add1x1Data(Element currFile)
  { /* function will add the data from the specified file of type '1x1'
     * '1x1'- defined as (180x360) lines of data values, each with its x and y
     * indices and value for that location in x,y,value form.
     */
    log.log(Level.FINER, "begin function");
    
    boolean tagged = true;
    boolean avg = true;
    boolean dec = true;
    boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    
    //check if tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(currFile.getAttributeValue("tagged"))).booleanValue();
    
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
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
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
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
      System.exit(1);
    }
  //txt file opened

    if(tagged)
    { //if didnt get these before in the xml file
      dataName = readWord(input);
      time = Double.parseDouble(readWord(input));
    	if(readWord(input).equals("false"))
    	{
    	  avg = false;
    	}
    	if(readWord(input).equals("scientific"))
    	{
    	  dec = false;
    	}
    	unit = readWord(input);
    	try{
    	  ref = input.readLine();
    	}catch(IOException e) {}
    }
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
  
  //reading the data from the file
    try{
      int firstComma, secondComma;
      double mY, mX;
      String sVal;
      String readString = input.readLine();
      while((readString!=null))
      {
        firstComma = readString.indexOf(',');
        secondComma = readString.indexOf(',', (firstComma+1));
        mX = Double.parseDouble(readString.substring(0, firstComma));
        mY = Double.parseDouble(readString.substring((firstComma+1), secondComma));
        sVal = readString.substring((secondComma+1), readString.length());
        if(dec)
        { //numbers stored in decimal format
          dataValue = Double.valueOf(sVal);
        } else
        { //numbers stored in scientific notation
          dataValue = new Double(scientificToDouble(sVal));
        }

        toAdd = new DataBlock(mX, mY, 1, 1);
        timeValue = new TreeMap();
        timeValue.put(new Double(time), dataValue);
        
        //check overwrite bit, if so, use hold instead of dataName
        if(overwrite)
        {
          //just replace name with hold, later, we will merge hold over old data
          toAdd.data.put("hold", timeValue);
        } else
        {
          //add data as normal
          toAdd.data.put(dataName, timeValue);
        }
        
      //merging this data into the current tree
        dataStruct.addData(toAdd, avg);

        //prepping for next run
        readString = input.readLine();
      }
      
//    done adding all data, if overwrite, must merge with old data now
      if(overwrite)
      {
        dataStruct.resolveOverwrite("hold", dataName);
      } //else we are done already
      
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
    
  }
  
  private void addASCData(Element currFile)
  {
    /* function will add the data from the specified file of type 'asc'
     * 'asc'- defined as 180/resolution lines of 360/resolution data elements
     * values are coma seperated,
     * first line of values corresponds to -90 degreens latitude, no 90 degrees
     */
    
    log.log(Level.FINER, "begin function");
    boolean avg = true;
    //boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    double nan = Double.NaN;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    String storage = "values";
    String nameConvention = "natural";
    String target = null;
    String prefix = null;
    TreeMap<String, Boolean> overwrite = new TreeMap<String, Boolean>();
    HashMap nameMap = null;
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("date"))
      {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("NaN"))
      {
        nan = scientificToDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage")) {
	      storage = currElem.getAttributeValue("type");
      } else if(currElem.getName().equals("data"))
      {
	      nameConvention = currElem.getAttributeValue("type");
	      if(nameConvention != null)
	      {
		      if(nameConvention.equals("prefix"))
		      {//names are a prefix concatedated with the value
			      Element preElem = currElem.getChild("prefix");
			      prefix = preElem.getAttributeValue("value");
		      } else if(nameConvention.equals("manual"))
		      {//each value has a mapping to a name to use
			      nameMap = new HashMap();
			      List mapList = currElem.getChildren("map");
			      Element currMap;

			      for(int k = 0; k < mapList.size(); k++)
			      {
				      currMap = (Element)mapList.get(k);
				      nameMap.put(currMap.getAttributeValue("key"), currMap.getAttributeValue("name"));
			      }

			      if(!nameMap.containsKey("null"))
			      {
				      nameMap.put("null", null);
			      }
		      } //else use natual naming
	      } else
	      { //everything goes in one!
		      nameConvention = "single";
		      dataName = currElem.getAttributeValue("value");
	      }
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
  //opening asc file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException -> "+fileName+" -> exiting.");
      System.exit(1);
    }
  //asc file opened
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      //overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      /*
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
      */
    }
    
    //reading the data from the file
    for(double i = (-90); i <= 89.9999; i+=res)
    {
      for(double k = -180; k < 179.9999; k+=res)
      {
        //System.out.println(k);
        //numbers stored in scientific notation
        dataValue = new Double(scientificToDouble(readComma(input)));
        
        if(dataValue == nan)
        {
          dataValue = Double.NaN;
        }
	if(storage.equals("coverage")) {
          if(nameConvention.equals("single"))
          {
            target = dataName;
          } else
          {
            target = dataValue.toString();

            if(nameConvention.equals("manual"))
            {

              target = (String)nameMap.get(target);
            } else if(nameConvention.equals("prefix"))
            {
              target = prefix+target;
            }
          } //target now has the data name we are storing this geometry in
          
	  dataValue = new Double(1.0);
	} else {
		target = dataName;
	}
          if(!overwrite.containsKey(target))
          { //this is the first run, check for overwrite properties now
            boolean over = false;
            if(dataAvg.containsKey(target))
            {
              over = true;
            }
            overwrite.put(target, over);
          }
          
          if(!overwrite.get(target))
          {
            //setting whether contained data is additive or averaged
            if(!dataAvg.containsKey(target))
            { //i cant believe this is the only way to do this
              //its going to take forever to test every run
              dataAvg.put(target, new Boolean(avg));
              if(ref != null)
              {
                dataRef.put(target, ref);
              }
            }
            //done settign avg/add ref and units
          }
        
        toAdd = new DataBlock(k, i, res, res);
        timeValue = new TreeMap();
        timeValue.put(time, dataValue);



          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite.get(target))
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put(("hold"+target), timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(target, timeValue);
          }

	
        
	  /*
        //check overwrite bit, if so, use hold instead of dataName
        if(overwrite)
        {
          //just replace name with hold, later, we will merge hold over old data
          toAdd.data.put("hold", timeValue);
        } else
        {
          //add data as normal
          toAdd.data.put(dataName, timeValue);
        }
	*/
        
      //merging this data into the current tree
        dataStruct.addData(toAdd, avg);
      }
    }


    //done adding all data, if overwrite, must merge with old data now
    Map.Entry me;
    Iterator overIt = overwrite.entrySet().iterator();
    while(overIt.hasNext())
    {
      me = (Map.Entry)overIt.next();
      if((Boolean)me.getValue())
      {
        //then this particular 'target' was overwritten
        dataStruct.resolveOverwrite(("hold"+(String)me.getKey()), (String)me.getKey());
      }
    }
    
    
    /*
    //done adding all data, if overwrite, must merge with old data now
    if(overwrite)
    {
      dataStruct.resolveOverwrite("hold", dataName);
    } //else we are done already
    */
    
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
  }
  
  private void addNetCDFData(Element currFile)
  { /* function will add the data from the specified file of type 'netcdf'
     * 'netcdf'- defined as a .nc file associated with the NetCDF standard. Data
     * can be at any resolution and appears in a matrix of values and NaN's. 
     */
    log.log(Level.FINER, "begin function");
    
    boolean avg = true;
    boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String dataVar = "farea"; //what variable in the NetCDF file to get data from
    String ref = null;
    String unit = null;
    double time = 0;
    double resX = 1;
    double resY = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    boolean isEosWeb = false;
    boolean shouldFlip = false;
    int internalTimeIndex = 0;
    int numTime = 1;
    int timeStep = 1;
    
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
	String tempAttr;
        //time = Double.parseDouble(currElem.getAttributeValue("value"));
	if((tempAttr = currElem.getAttributeValue("value")) != null) {
		time = Double.parseDouble(tempAttr);
	} else {
		time = Double.parseDouble(currElem.getAttributeValue("start"));
		Double tempEndTime = Double.parseDouble(currElem.getAttributeValue("end"));
		numTime = (int)(tempEndTime - time) + 1;
		timeStep = Integer.parseInt(currElem.getAttributeValue("dateStep"));
		if(numTime < 0) {
			log.log(Level.WARNING, "End date is before start date");
		}
	}
	/*
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
	*/
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("variable"))
      {
        dataVar = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else if(currElem.getName().equals("eos-webster"))
      {
	      isEosWeb = true;
	      if(currElem.getAttributeValue("flip") != null) {
		      shouldFlip = Boolean.parseBoolean(currElem.getAttributeValue("flip"));
	      }
	      internalTimeIndex = Integer.parseInt(currElem.getAttributeValue("date-index"));
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    

    
  //reading the data from the file
    try 
    {
      NetcdfFile nc = NetcdfFile.open(fileName);
      /* Read a variable named *readin* from the file, it contains the masks */
      Variable data = nc.findVariable(dataVar);
      Array ma2Array = data.read();
      
      int[] shp = ma2Array.getShape();
      resY = 180.0 / shp[shp.length-2];
      resX = 360.0 / shp[shp.length-1];
      //System.out.println("Has shape: "+shp[0]+"x"+shp[1]/*+"x"+shp[2]*/);
      double startY;
      double endY;

      Variable latIndex = nc.findVariable(data.getDimension(shp.length-2).getName());
      if(latIndex != null) {
	      Array latArray = latIndex.read();
	      int[] latShp = latArray.getShape();
	      Index latI = latArray.getIndex();
	      startY = latArray.getDouble(latI.set(0));
	      endY = latArray.getDouble(latI.set(latShp[0]-1));
	      resY = (startY-endY) / (shp[shp.length-2]-1);

	      // are the dang ol' .5s gettin in the way?
	      startY = Math.ceil(startY) - resY;
	      //endY = Math.floor(endY); // floor because it is negative, is it always -?
	      //endY -= resY;
      } else {
	      startY = 90;
	      endY = -90;
      }

      //checking for overwrite and setting basic information (avg, ref, units)
      if(dataAvg.containsKey(dataName))
      {
	      //then we are overwriting this data!
	      log.log(Level.FINER, "data being added is an overwrite");
	      overwrite = true;
      } else
      {
	      //dont want to add all this information if we already have!!!
	      if(!init)
	      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
		      dataStruct.fillWorld(resX);
		      init = true;
	      }
	      //setting whether contained data is additive or averaged and references
	      dataAvg.put(dataName, new Boolean(avg));
	      if(ref != null)
	      {
		      dataRef.put(dataName, ref);
	      }
	      if(unit != null)
	      {
		      dataUnits.put(dataName, unit);
	      }
	      //done settign avg/add and references
      }
      //done doing overwrite check

      Index in = ma2Array.getIndex();
      int i = 0;
      int k = 0;
      ucar.nc2.Attribute tempAttr = data.findAttribute("missing_value");
      float NaN;
      if(tempAttr != null) {
	      NaN = tempAttr.getNumericValue().floatValue();
      } else {
	      NaN = Float.POSITIVE_INFINITY;
      }
      Variable timeVar = null;
      Array timeArr = null;
      Index timeInd = null;
      if(!isEosWeb) {
	      timeVar = nc.findVariable("time");
	      if(timeVar != null) {
		      timeArr = timeVar.read();
		      timeInd = timeArr.getIndex();
	      } else {
		      log.log(Level.WARNING, "Couldn't find time data defaulting to 0");
		      internalTimeIndex = 0;
	      }
      } else {
	      // is this right? should it be - timeStep
	      // --internalTimeIndex;
	      internalTimeIndex -= timeStep;
      }

      /*
      System.out.println("Have settings:");
      System.out.println("resY: "+resY);
      System.out.println("resX: "+resX);
      System.out.println("missing value: "+NaN);
      System.out.println("internal time index: "+internalTimeIndex);
      System.out.println("num times: "+numTime);
      System.out.println("time step : "+timeStep);
      System.out.println("startY: "+startY);
      System.out.println("endY: "+endY);
      */


      for(int currTimeIndex = 0; currTimeIndex < numTime; currTimeIndex += timeStep)
      {
	      if(!isEosWeb && timeVar != null) {
		      internalTimeIndex = findYearIndex(timeArr, timeInd, 0, timeArr.getShape()[0]-1, (float)(time+currTimeIndex));
		      if(internalTimeIndex == -1) {
			      log.log(Level.WARNING, "Couldn't find "+time+" in time variable defaulting to 0");
			      internalTimeIndex = 0;
		      }
	      } else {
		      internalTimeIndex += timeStep;
	      }
	      //for(double y = (90-resY); y >= -90; y-=resY)
	      for(double y = startY; y >= endY; y-=resY)
	      {
		      k=0;
		      for(double x = -180; x < 180; x+=resX)
		      {
			      if(ma2Array.getRank() == 2)
			      {
				      dataValue = new Double((double)ma2Array.getFloat(in.set(i, k)));
			      } else if(ma2Array.getRank() == 3)
			      {
				      dataValue = new Double(ma2Array.getDouble(in.set(internalTimeIndex, i, k)));
			      } else if(ma2Array.getRank() == 4)
			      {
				      dataValue = new Double((double)ma2Array.getFloat(in.set(0, internalTimeIndex, i, k)));
			      } else
			      {
				      log.log(Level.SEVERE, "Array rank of "+ma2Array.getRank()+" not supported.");
				      return;
			      }

			      if(dataValue != NaN)
			      {
				      if(!shouldFlip) {
					      toAdd = new DataBlock(x, y, resX, resY);
				      } else {
					      toAdd = new DataBlock(x, -1*y, resX, resY);
				      }
				      timeValue = new TreeMap();
				      timeValue.put(new Double(time+currTimeIndex), dataValue);

				      //check overwrite bit, if so, use hold instead of dataName
				      if(overwrite)
				      {
					      //just replace name with hold, later, we will merge hold over old data
					      toAdd.data.put("hold", timeValue);
				      } else
				      {
					      //add data as normal
					      toAdd.data.put(dataName, timeValue);
				      }

				      //merging this data into the current tree
				      //System.out.println("sending "+dataValue);
				      dataStruct.addData(toAdd, avg);
			      }
			      //else this value SUCKS!!!!

			      k++;
		      }
		      i++;
	      }
	      i = 0;
      }
      //done adding all data, if overwrite, must merge with old data now
      if(overwrite)
      {
        dataStruct.resolveOverwrite("hold", dataName);
      } //else we are done already
      
    } catch (java.io.IOException e) {
      log.log(Level.SEVERE, "Error reading NetCDF file -> "+fileName);
      System.exit(1);
    }
  //done reading data from file
    
  }

  /**
   * Finds the year index from the passed in array.
   * @param arr The array to search in.
   * @param ind The index for the array
   * @param low The lowest index the value could be in.
   * @param high The highest index the value could be in.
   * @param find The value to find.
   * @return The index of the given year in the given array, or -1 if not found.
   * @warning The array must be sorted.
   */
  private int findYearIndex(Array arr, Index ind, int low, int high, float find) {
	  // since the array is sorted can do something like binary search
	  if(high < low || low > high) {
		  return -1;
	  }
	  int mid = (int)((high+low)/2);
	  float midValue = arr.getFloat(ind.set(mid));
	  if(find == midValue) {
		  return mid;
	  } else if(find < midValue) {
		  return findYearIndex(arr, ind, low, mid-1, find);
	  } else {
		  return findYearIndex(arr, ind, mid+1, high, find);
	  }
  }
  
  private void addPointShapeFileData(Element currFile)
  { //use geotools to open and read from a shapefile
    log.log(Level.FINER, "begin function");
    
    List infoChildren;
    Element currElem;
    
    boolean overwrite = false;
    String fileName = "init";
    String attrName = "init";
    String dataName = "shutup,";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    double x, y, mult;
    boolean avg = true;
    boolean typeWarn = false;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    
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
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("attribute"))
      {
        attrName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
    
    try
    {
      File shapeFile = new File(fileName);
      URL shapeURL = shapeFile.toURL();
      ShapefileDataStore store = new ShapefileDataStore(shapeURL);
      String name = store.getTypeNames()[0];
      FeatureSource source = store.getFeatureSource(name);
      //FeatureResults fsShape = source.getFeatures();
      //FeatureCollection collection = fsShape.collection();
      FeatureCollection collection = source.getFeatures();
      Iterator iter = collection.iterator();
      try
      {
        while(iter.hasNext())
        {
          Feature inFeature = (Feature)iter.next();
          Geometry geom = inFeature.getDefaultGeometry();
          Point cent = geom.getCentroid();
          Object holdAttr = inFeature.getAttribute(attrName);
          
          
          if(holdAttr instanceof Long)
          {
            dataValue = new Double(((Long)holdAttr).doubleValue());
          } else if(holdAttr instanceof Double)
          {
            dataValue = (Double)holdAttr;
          } else if(holdAttr instanceof Integer)
          {
            dataValue = new Double(((Integer)holdAttr).doubleValue());
          } else
          {
            if(!typeWarn)
            {
              log.log(Level.WARNING, "Unknown attribute data type");
              typeWarn = true;
            }
            dataValue = new Double(-1);
          }
          
          x = cent.getX();
          mult = x/res;
          mult = Math.floor(mult);
          x = mult*res;
          
          y = cent.getY();
          mult = y/res;
          mult = Math.floor(mult);
          y = mult*res;
          
          //System.out.println(cent.getX()+" "+cent.getY()+" - "+x+" "+y);
          
          toAdd = new DataBlock(x, y, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), dataValue);

          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite)
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put("hold", timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(dataName, timeValue);
          }

          //System.out.println("new data");
          //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
          //System.out.println(" - "+dataValue);
        }
        
        //done adding all data, if overwrite, must merge with old data now
        if(overwrite)
        {
          dataStruct.resolveOverwrite("hold", dataName);
        } //else we are done already
      } finally
      {
        collection.close(iter);
      }

    } catch(ShapefileException e)
    {
      log.log(Level.WARNING, "That aint no ShapeFile fool! -> "+fileName);
    } catch(IOException e)
    {
      log.log(Level.WARNING, "IOException dont give me none of that!! -> "+fileName);
    }
    
    log.log(Level.FINE, "Done adding new PointShapefileData");
  }
  
  private void addPolyShapeFileData(Element currFile)
  { //use geotools to open and read from a shapefile
    log.log(Level.FINER, "begin function");
    
    List infoChildren;
    Element currElem;
    
    String fileName = "init";
    String attrName = "init";
    String dataName = "shutup,";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = dataStruct.getResolution();
    double x, y, mult;
    boolean avg = true;
    boolean typeWarn = false;
    boolean overwrite = false;
    TreeMap timeValue = new TreeMap();
    Double dataValue;
    DataBlock toAdd = new DataBlock(0, 0, 0, 0);
    
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
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("attribute"))
      {
        attrName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
    
    try
    {
      final Double timeDouble = new Double(time);
      File shapeFile = new File(fileName);
      URL shapeURL = shapeFile.toURL();
      ShapefileDataStore store = new ShapefileDataStore(shapeURL);
      String name = store.getTypeNames()[0];
      FeatureSource source = store.getFeatureSource(name);
      //FeatureResults fsShape = source.getFeatures();
      //FeatureCollection collection = fsShape.collection();
      FeatureCollection collection = source.getFeatures();
      AreaFunction areaCalculator = new AreaFunction();
      Iterator iter = collection.iterator();
      try
      {
        while(iter.hasNext())
        {
          Feature inFeature = (Feature)iter.next();
          Geometry geom = inFeature.getDefaultGeometry();
          Geometry env = geom.getEnvelope();
          Object holdAttr = inFeature.getAttribute(attrName);
          
          
          if(holdAttr instanceof Long)
          {
            dataValue = new Double(((Long)holdAttr).doubleValue());
          } else if(holdAttr instanceof Double)
          {
            dataValue = (Double)holdAttr;
          } else if(holdAttr instanceof Integer)
          {
            dataValue = new Double(((Integer)holdAttr).doubleValue());
          } else
          {
            if(!typeWarn)
            {
              log.log(Level.WARNING, "Unknown attribute data type");
              typeWarn = true;
            }
            dataValue = new Double(-1);
          }
          
          if(env instanceof Point)
          {
            Point area = (Point)env;
            //normalizing to res grid
            x = area.getX();
            mult = x/res;
            mult = Math.floor(mult);
            x = mult*res;
            
            y = area.getY();
            mult = y/res;
            mult = Math.floor(mult);
            y = mult*res;
            
            toAdd = new DataBlock(x, y, res, res);
            timeValue = new TreeMap();
            timeValue.put(new Double(time), dataValue);

            //check overwrite bit, if so, use hold instead of dataName
            if(overwrite)
            {
              //just replace name with hold, later, we will merge hold over old data
              toAdd.data.put("hold", timeValue);
            } else
            {
              //add data as normal
              toAdd.data.put(dataName, timeValue);
            }

            //merging this data into the current tree
            dataStruct.addData(toAdd, avg);
          } else //env is a Polygon
          {
            Polygon area = (Polygon)env;
            double minX, maxX, minY, maxY;
            Coordinate[] coords = area.getCoordinates();
            
	    // find the smallest box that includes all
	    // of the coordinates of the polygon
            minX = coords[0].x;
            maxX = coords[0].x;
            minY = coords[0].y;
            maxY = coords[0].y;
            for(int i  = 1 ; i < coords.length; i++)
            {
              if(coords[i].x > maxX)
                maxX = coords[i].x;
              if(coords[i].x < minX)
                minX = coords[i].x;
              if(coords[i].y > maxY)
                maxY = coords[i].y;
              if(coords[i].y < minY)
                minY = coords[i].y;
            }
            
            //normalizes lower bounds (upper dont matter)
	    // so that the lower left would match with the lower
	    // left of one of our grid cells
            mult = minX/res;
            mult = Math.floor(mult);
            minX = mult*res;
            
            mult = minY/res;
            mult = Math.floor(mult);
            minY = mult*res;
            
	    // break up the box we just created into res sized boxes
	    // and add the dataValue for the polygon for each res sized
	    // sub-box, then find the area that the sub-box takes up of
	    // the polygon and weight dataValue by it and add that
	    // dataBlock to our grid
            for(double X = minX; X < maxX; X+=res)
            { // cant just += res because minX and minY arent normalized
              //normalize once before instead of every time after
	      //do the above comments still apply?
              for(double Y = maxY; Y > minY; Y-=res)
              {
                //there should be SOMETHING to do to weight the data for sparse
                /*
                 * look into superhigh resolution / sparse data sets
                 * and how they might be affected by overwrites, or at elast
                 * how they can be accounted for.
                 * 1. should this accounting take place here
                 * 2. should sparceness information be sent into the data collection
                 */
                
		toAdd.setRect(X, Y, res, res);
		Coordinate[] boxPoints = {new Coordinate(X, Y), new Coordinate(X+res, Y),
			new Coordinate(X+res, Y-res), new Coordinate(X, Y-res), new Coordinate(X, Y)};
		Geometry subBox = new Polygon(new LinearRing(boxPoints, area.getPrecisionModel(), area.getSRID()),
				area.getPrecisionModel(), area.getSRID());
		final double polySubArea = areaCalculator.getArea(area.intersection(subBox));
                timeValue.put(timeDouble, dataValue * polySubArea);
                
                //check overwrite bit, if so, use hold instead of dataName
                if(overwrite)
                {
                  //just replace name with hold, later, we will merge hold over old data
                  toAdd.data.put("hold", timeValue);
                } else
                {
                  //add data as normal
                  toAdd.data.put(dataName, timeValue);
                }
                
                //merging this data into the current tree
                dataStruct.addData(toAdd, avg);
              }
            }
            
          }
        }
        
        //done adding all data, if overwrite, must merge with old data now
        if(overwrite)
        {
          dataStruct.resolveOverwrite("hold", dataName);
        } //else we are done already
      } finally
      {
        collection.close(iter);
      }

    } catch(ShapefileException e)
    {
      log.log(Level.WARNING, "That aint no ShapeFile fool! -> "+fileName);
    } catch(IOException e)
    {
      log.log(Level.WARNING, "IOException dont give me none of that!! -> "+fileName);
    }
    
    log.log(Level.FINE, "Done adding new PolyShapefileData");
  }

  private void addTxtEnum(Element currFile)
  { /* function will add the data from the specified file of type 'txt'
     * 'txt'- defined as 180/resolution lines of 360/resolution data elements
     * tagged or untagged - basically using this to debug program as i build it
     */
    log.log(Level.FINER, "begin function");
    boolean tagged = true;
    boolean avg = true;
    boolean dec = true;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String nameConvention = "natural";
    String target = "null";
    String prefix = "";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    double NaN = -9999;
    HashMap nameMap = null;
    double startLat = 90;
    double endLat = -90;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    TreeMap<String, Boolean> overwrite = new TreeMap<String, Boolean>();
    int skipLines = 0;
    
    //check if tags are contained in the xml file, dont get them later
    tagged = (Boolean.valueOf(currFile.getAttributeValue("tagged"))).booleanValue();
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("data"))
      {
        nameConvention = currElem.getAttributeValue("type");
        if(nameConvention != null)
        {
          if(nameConvention.equals("prefix"))
          {//names are a prefix concatedated with the value
            Element preElem = currElem.getChild("prefix");
            prefix = preElem.getAttributeValue("value");
          } else if(nameConvention.equals("manual"))
          {//each value has a mapping to a name to use
            nameMap = new HashMap();
            List mapList = currElem.getChildren("map");
            Element currMap;
            
            for(int k = 0; k < mapList.size(); k++)
            {
              currMap = (Element)mapList.get(k);
              nameMap.put(currMap.getAttributeValue("key"), currMap.getAttributeValue("name"));
            }
            
            if(!nameMap.containsKey("null"))
            {
              nameMap.put("null", null);
            }
          } //else use natual naming
        } else
        { //everything goes in one!
          nameConvention = "single";
          dataName = currElem.getAttributeValue("value");
        }
      } else if(currElem.getName().equals("date"))
      {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("format"))
      {
        if(currElem.getAttributeValue("value").equals("scientific"))
        { //dec is default to true
          dec = false;
        }
      } else if(currElem.getName().equals("NaN"))
      {
        NaN = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("skip-lines"))
      {
        skipLines = Integer.parseInt(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("start-latitude"))
      {
        startLat = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("end-latitude"))
      {
        endLat = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
      System.exit(1);
    }
  //txt file opened

    // skip lines if necessary before reading any values
    try {
	    for(int currLine = 0; currLine < skipLines; ++currLine) {
		    String skippedLine = input.readLine();
		    log.log(Level.FINER, "Skipped: "+skippedLine);
	    }
    } catch(IOException ioe) {
	    log.log(Level.SEVERE, "Error reading data: "+ioe);
	    System.exit(1);
    }
    
    if(tagged)
    { //if didnt get these before in the xml file
      dataName = readWord(input);
      time = Double.parseDouble(readWord(input));
        res = Double.parseDouble(readWord(input));
        if(readWord(input).equals("false"))
        {
          avg = false;
        }
        if(readWord(input).equals("scientific"))
        {
          dec = false;
        }
        unit = readWord(input);
        try{
          ref = input.readLine();
        }catch(IOException e) {}
    }
    
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataStruct.fillWorld(res);
      init = true;
    }

    
    
  //reading the data from the file
    for(double i = (startLat-res); i >= endLat; i-=res)
    {
      for(double k = -180; k < 179.9999; k+=res)
      {
        //System.out.println(k);
        if(dec)
        { //numbers stored in decimal format
          dataValue = Double.valueOf(readWord(input));
        } else
        { //numbers stored in scientific notation
          dataValue = new Double(scientificToDouble(readWord(input)));
        }
        
        if(dataValue != NaN)
        {
          if(nameConvention.equals("single"))
          {
            target = dataName;
          } else
          {
            target = dataValue.toString();

            if(nameConvention.equals("manual"))
            {

              target = (String)nameMap.get(target);
            } else if(nameConvention.equals("prefix"))
            {
              target = prefix+target;
            }
          } //target now has the data name we are storing this geometry in
          
          if(!overwrite.containsKey(target))
          { //this is the first run, check for overwrite properties now
            boolean over = false;
            if(dataAvg.containsKey(target))
            {
              over = true;
            }
            overwrite.put(target, over);
          }
          
          if(!overwrite.get(target))
          {
            //setting whether contained data is additive or averaged
            if(!dataAvg.containsKey(target))
            { //i cant believe this is the only way to do this
              //its going to take forever to test every run
              dataAvg.put(target, new Boolean(avg));
              if(ref != null)
              {
                dataRef.put(target, ref);
              }
            }
            //done settign avg/add ref and units
          }
          
          toAdd = new DataBlock(k, i, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), new Double(1));
          
          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite.get(target))
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put(("hold"+target), timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(target, timeValue);
          }
          
        //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
        }
      }
    }
    
    //done adding all data, if overwrite, must merge with old data now
    Map.Entry me;
    Iterator overIt = overwrite.entrySet().iterator();
    while(overIt.hasNext())
    {
      me = (Map.Entry)overIt.next();
      if((Boolean)me.getValue())
      {
        //then this particular 'target' was overwritten
        dataStruct.resolveOverwrite(("hold"+(String)me.getKey()), (String)me.getKey());
      }
    }
    
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
  }
  
  private void addNetCDFEnum(Element currFile)
  { /* function will add the data from the specified file of type 'netcdf'
     * 'netcdf'- defined as a .nc file associated with the NetCDF standard. Data
     * can be at any resolution and appears in a matrix of values and NaN's. 
     */
    log.log(Level.FINER, "begin function");
    
    boolean avg = true;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String dataVar = "farea"; //what variable in the NetCDF file to get data from
    String nameConvention = "natural";
    String target = "null";
    String prefix = "";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    HashMap nameMap = null;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    TreeMap<String, Boolean> overwrite = new TreeMap<String, Boolean>();
    
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("data"))
      {
        nameConvention = currElem.getAttributeValue("type");
        if(nameConvention != null)
        {
          if(nameConvention.equals("prefix"))
          {//names are a prefix concatedated with the value
            Element preElem = currElem.getChild("prefix");
            prefix = preElem.getAttributeValue("value");
          } else if(nameConvention.equals("manual"))
          {//each value has a mapping to a name to use
            nameMap = new HashMap();
            List mapList = currElem.getChildren("map");
            Element currMap;
            
            for(int k = 0; k < mapList.size(); k++)
            {
              currMap = (Element)mapList.get(k);
              nameMap.put(currMap.getAttributeValue("key"), currMap.getAttributeValue("name"));
            }
            
            if(!nameMap.containsKey("null"))
            {
              nameMap.put("null", null);
            }
          } //else use natual naming
        } else
        { //everything goes in one!
          nameConvention = "single";
          dataName = currElem.getAttributeValue("value");
        }
      } else if(currElem.getName().equals("date"))
      {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("variable"))
      {
        dataVar = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    

    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataStruct.fillWorld(res);
      init = true;
    }
    
  //reading the data from the file
    try 
    {
      NetcdfFile nc = NetcdfFile.open(fileName);
      /* Read a variable named *readin* from the file, it contains the masks */
      Variable data = nc.findVariable(dataVar);
      Array ma2Array = data.read();
      
      Index in = ma2Array.getIndex();
      //System.out.println("***RANK: "+ma2Array.getRank());

      int i = 0;
      int k = 0;
      float NaN = data.findAttribute("missing_value").getNumericValue().floatValue();

      for(double y = (90-res); y >= -90; y-=res)
      {
        k=0;
        for(double x = -180; x < 180; x+=res)
        {
          if(ma2Array.getRank() == 2)
          {
            dataValue = new Double((double)ma2Array.getFloat(in.set(i, k)));
          } else if(ma2Array.getRank() == 3)
          {
            dataValue = new Double((double)ma2Array.getFloat(in.set(0, i, k)));
          } else if(ma2Array.getRank() == 4)
          {
            dataValue = new Double((double)ma2Array.getFloat(in.set(0, 0, i, k)));
          } else
          {
            log.log(Level.SEVERE, "Array rank of "+ma2Array.getRank()+" not supported.");
            return;
          }
          
          if(dataValue != NaN)
          {
            
            if(nameConvention.equals("single"))
            {
              target = dataName;
            } else
            {
              target = dataValue.toString();

              if(nameConvention.equals("manual"))
              {

                target = (String)nameMap.get(target);
              } else if(nameConvention.equals("prefix"))
              {
                target = prefix+target;
              }
            } //target now has the data name we are storing this geometry in
            
            if(!overwrite.containsKey(target))
            { //this is the first run, check for overwrite properties now
              boolean over = false;
              if(dataAvg.containsKey(target))
              {
                over = true;
              }
              overwrite.put(target, over);
            }
            
            if(!overwrite.get(target))
            {
              //setting whether contained data is additive or averaged
              if(!dataAvg.containsKey(target))
              { //i cant believe this is the only way to do this
                //its going to take forever to test every run
                dataAvg.put(target, new Boolean(avg));
                if(ref != null)
                {
                  dataRef.put(target, ref);
                }
              }
              //done settign avg/add ref and units
            }
            
            toAdd = new DataBlock(x, y, res, res);
            timeValue = new TreeMap();
            timeValue.put(new Double(time), new Double(1));

            //check overwrite bit, if so, use hold instead of dataName
            if(overwrite.get(target))
            {
              //just replace name with hold, later, we will merge hold over old data
              toAdd.data.put(("hold"+target), timeValue);
            } else
            {
              //add data as normal
              toAdd.data.put(target, timeValue);
            }

          //merging this data into the current tree
            //System.out.println("sending "+dataValue);
            dataStruct.addData(toAdd, avg);
          }
          //else this value SUCKS!!!!
          
          k++;
        }
        i++;
      }
      
      //done adding all data, if overwrite, must merge with old data now
      Map.Entry me;
      Iterator overIt = overwrite.entrySet().iterator();
      while(overIt.hasNext())
      {
        me = (Map.Entry)overIt.next();
        if((Boolean)me.getValue())
        {
          //then this particular 'target' was overwritten
          dataStruct.resolveOverwrite(("hold"+(String)me.getKey()), (String)me.getKey());
        }
      }
      
    } catch (java.io.IOException e) {
      log.log(Level.SEVERE, "Error reading NetCDF file -> "+fileName);
      System.exit(1);
    }
  //done reading data from file
    
  }
  
  private void addPointShapeFileEnum(Element currFile)
  {
    log.log(Level.FINER, "begin function");
    
    List infoChildren;
    Element currElem;
    
    String fileName = "init";
    String attrName = "init";
    String nameConvention = "natural";
    String dataName = "null";
    String target = "null";
    String prefix = "";
    String ref = null;
    double time = 0;
    double res = 1;
    double x, y, mult;
    boolean avg = true; //avg is always true for coverage readings
    boolean typeWarn = false;
    TreeMap timeValue;
    TreeMap<String, Boolean> overwrite = new TreeMap<String, Boolean>();
    HashMap nameMap = null;
    Double dataValue;
    DataBlock toAdd;
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("data"))
      {
        nameConvention = currElem.getAttributeValue("type");
        if(nameConvention != null)
        {
          if(nameConvention.equals("prefix"))
          {//names are a prefix concatedated with the value
            Element preElem = currElem.getChild("prefix");
            prefix = preElem.getAttributeValue("value");
          } else if(nameConvention.equals("manual"))
          {//each value has a mapping to a name to use
            nameMap = new HashMap();
            List mapList = currElem.getChildren("map");
            Element currMap;
            
            for(int k = 0; k < mapList.size(); k++)
            {
              currMap = (Element)mapList.get(k);
              nameMap.put(currMap.getAttributeValue("key"), currMap.getAttributeValue("name"));
            }
            
            if(!nameMap.containsKey("null"))
            {
              nameMap.put("null", null);
            }
          } //else use natual naming
        } else
        { //everything goes in one!
          nameConvention = "single";
          dataName = currElem.getAttributeValue("value");
        }
      } else if(currElem.getName().equals("date"))
      {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("attribute"))
      {
        attrName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataStruct.fillWorld(res);
      init = true;
    }
    
    try
    {
      File shapeFile = new File(fileName);
      URL shapeURL = shapeFile.toURL();
      ShapefileDataStore store = new ShapefileDataStore(shapeURL);
      String name = store.getTypeNames()[0];
      FeatureSource source = store.getFeatureSource(name);
      //FeatureResults fsShape = source.getFeatures();
      //FeatureCollection collection = fsShape.collection();
      FeatureCollection collection = source.getFeatures();
      Iterator iter = collection.iterator();
      
      dataValue = new Double(1); //always sending 1 as the value, so a full overlap
      //will give the value of one to that block
      try
      {
        while(iter.hasNext())
        {
          Feature inFeature = (Feature)iter.next();
          Geometry geom = inFeature.getDefaultGeometry();
          Point cent = geom.getCentroid();
          
          
          if(nameConvention.equals("single"))
          {
            target = dataName;
          } else
          {
            Object holdAttr = inFeature.getAttribute(attrName);

            if(holdAttr instanceof Long)
            {
              target = ((Long)holdAttr).toString();
            } else if(holdAttr instanceof Double)
            {
              target = ((Double)holdAttr).toString();
            } else if(holdAttr instanceof Float)
            {
              target = ((Float)holdAttr).toString();
            } else if(holdAttr instanceof Integer)
            {
              target = ((Integer)holdAttr).toString();
            } else if(holdAttr instanceof String)
            {
              target = ((String)holdAttr);
            } else
            {
              if(!typeWarn)
              {
                log.log(Level.WARNING, "Unknown attribute data type -> "
                    +target.getClass());
                typeWarn = true;
              }

              if(nameConvention.equals("manual"))
              {
                target = (String)nameMap.get("null");
              } else
              {
                target = null;
              }
            } // this is all we need with natural naming

            if(nameConvention.equals("manual"))
            {

              target = (String)nameMap.get(target);
            } else if(nameConvention.equals("prefix"))
            {
              target = prefix+target;
            }
          } //target now has the data name we are storing this geometry in
          
          if(!overwrite.containsKey(target))
          { //this is the first run, check for overwrite properties now
            boolean over = false;
            if(dataAvg.containsKey(target))
            {
              over = true;
            }
            overwrite.put(target, over);
          }
          
          if(!overwrite.get(target))
          {
            //setting whether contained data is additive or averaged
            if(!dataAvg.containsKey(target))
            { //i cant believe this is the only way to do this
              //its going to take forever to test every run
              dataAvg.put(target, new Boolean(avg));
              if(ref != null)
              {
                dataRef.put(target, ref);
              }
            }
            //done settign avg/add ref and units
          }
          
          
          x = cent.getX();
          mult = x/res;
          mult = Math.floor(mult);
          x = mult*res;
          
          y = cent.getY();
          mult = y/res;
          mult = Math.floor(mult);
          y = mult*res;
          
          //System.out.println(cent.getX()+" "+cent.getY()+" - "+x+" "+y);
          
          toAdd = new DataBlock(x, y, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), dataValue);
          
          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite.get(target))
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put(("hold"+target), timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(target, timeValue);
          }

          //System.out.println("new data");
          //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
          //System.out.println(" - "+dataValue);
        }
        
        //done adding all data, if overwrite, must merge with old data now
        Map.Entry me;
        Iterator overIt = overwrite.entrySet().iterator();
        while(overIt.hasNext())
        {
          me = (Map.Entry)overIt.next();
          if((Boolean)me.getValue())
          {
            //then this particular 'target' was overwritten
            dataStruct.resolveOverwrite(("hold"+(String)me.getKey()), (String)me.getKey());
          }
        }
      } finally
      {
        collection.close(iter);
      }

    } catch(ShapefileException e)
    {
      log.log(Level.WARNING, "That aint no ShapeFile! -> "+fileName);
    } catch(IOException e)
    {
      log.log(Level.WARNING, "IOException!! -> "+fileName);
      e.printStackTrace();
    }
    log.log(Level.FINE, "Done adding new PointShapefileEnum");
  }
  
  private void addPolyShapeFileEnum(Element currFile)
  {
    // a feature is a piece of data (a polygon) an attribute is a name
    //of this feature (a data name)
    log.log(Level.FINER, "begin function");
    
    List infoChildren;
    Element currElem;
    
    String fileName = "init";
    String attrName = "init";
    String nameConvention = "natural";
    String dataName = "null";
    String target = "null";
    String prefix = "";
    String ref = null;
    double time = 0;
    double res = 1;
    double x, y, mult;
    boolean avg = true; //avg is always true for coverage readings
    boolean typeWarn = false;
    TreeMap timeValue;
    TreeMap<String, Boolean> overwrite = new TreeMap<String, Boolean>();
    HashMap nameMap = null;
    Double dataValue;
    DataBlock toAdd;
    GeometryFactory gf = new GeometryFactory();
    Coordinate[] makeLR = new Coordinate[5];
    makeLR[0] = new Coordinate();
    makeLR[1] = new Coordinate();
    makeLR[2] = new Coordinate();
    makeLR[3] = new Coordinate();
    makeLR[4] = new Coordinate();
    
  //getting file info from XML
    infoChildren = currFile.getChildren();
    for(int i = 0; i < infoChildren.size(); i++)
    {
      currElem = (Element)infoChildren.get(i);
      if(currElem.getName().equals("data"))
      {
        nameConvention = currElem.getAttributeValue("type");
        if(nameConvention != null)
        {
          if(nameConvention.equals("prefix"))
          {//names are a prefix concatedated with the value
            Element preElem = currElem.getChild("prefix");
            prefix = preElem.getAttributeValue("value");
          } else if(nameConvention.equals("manual"))
          {//each value has a mapping to a name to use
            nameMap = new HashMap();
            List mapList = currElem.getChildren("map");
            Element currMap;
            
            for(int k = 0; k < mapList.size(); k++)
            {
              currMap = (Element)mapList.get(k);
              nameMap.put(currMap.getAttributeValue("key"), currMap.getAttributeValue("name"));
            }
            
            if(!nameMap.containsKey("null"))
            {
              nameMap.put("null", null);
            }
          } //else use natual naming
        } else
        { //everything goes in one!
          nameConvention = "single";
          dataName = currElem.getAttributeValue("value");
        }
      } else if(currElem.getName().equals("date")) {
        time = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("attribute"))
      {
        attrName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("name"))
      {
        fileName = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("storage"))
      {
        //do nothing but this is a known tag
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
    }
  //done reading from XML file
    
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataStruct.fillWorld(res);
      init = true;
    }
    
    try
    {
      File shapeFile = new File(fileName);
      URL shapeURL = shapeFile.toURL();
      ShapefileDataStore store = new ShapefileDataStore(shapeURL);
      String name = store.getTypeNames()[0];
      FeatureSource source = store.getFeatureSource(name);
      //FeatureResults fsShape = source.getFeatures();
      //FeatureCollection collection = fsShape.collection();
      FeatureCollection collection = source.getFeatures();
      //System.out.println("features in collection: "+collection.getCount());
      Iterator iter = collection.iterator();
      Feature inFeature;
      Geometry geom;
      Geometry env;
      LinearRing lr;
      Polygon holdP;
      Coordinate[] coords;
      
      /*
      try{
        System.out.println("PREFEATURE: waiting...");
        System.in.read();
        System.in.read();
        System.out.println("...going");
      } catch(IOException e) {}
      */
      
      try
      {
        //int STOPIT = 0;
        while(iter.hasNext())
        {
          /*
          STOPIT++;
          if(STOPIT == 500)
          {
            try{
              System.out.println("INFEATURE: waiting...");
              System.in.read();
              System.in.read();
              System.out.println("...going");
            } catch(IOException e) {}
          }
          */
          inFeature = (Feature)iter.next();
          geom = inFeature.getDefaultGeometry();
          //System.out.println("number of points in this geometry: "+geom.getNumPoints());
          //System.out.println("------------area of this geometry: "+geom.getArea());
          env = geom.getEnvelope();
          
          if(nameConvention.equals("single"))
          {
            target = dataName;
          } else
          {
            Object holdAttr = inFeature.getAttribute(attrName);

            if(holdAttr instanceof Long)
            {
              target = ((Long)holdAttr).toString();
            } else if(holdAttr instanceof Double)
            {
              target = ((Double)holdAttr).toString();
            } else if(holdAttr instanceof Float)
            {
              target = ((Float)holdAttr).toString();
            } else if(holdAttr instanceof Integer)
            {
              target = ((Integer)holdAttr).toString();
            } else if(holdAttr instanceof java.lang.String)
            {
              target = ((String)holdAttr);
            } else
            {
              if(!typeWarn)
              {
                log.log(Level.WARNING, "Unknown attribute data type -> "
                    +target.getClass());
                typeWarn = true;
              }

              if(nameConvention.equals("manual"))
              {
                target = (String)nameMap.get("null");
              } else
              {
                target = null;
              }
            } // this is all we need with natural naming

            if(nameConvention.equals("manual"))
            {

              target = (String)nameMap.get(target);
            } else if(nameConvention.equals("prefix"))
            {
              target = prefix+target;
            }
          } //target now has the data name we are storing this geometry in

          if(!overwrite.containsKey(target))
          { //this is the first run, check for overwrite properties now
            boolean over = false;
            if(dataAvg.containsKey(target))
            {
              over = true;
            }
            overwrite.put(target, over);
          }
          
          if(!overwrite.get(target))
          {
            //setting whether contained data is additive or averaged
            if(!dataAvg.containsKey(target))
            { //i cant believe this is the only way to do this
              //its going to take forever to test every run
              dataAvg.put(target, new Boolean(avg));
              if(ref != null)
              {
                dataRef.put(target, ref);
              }
            }
            //done settign avg/add ref and units
          }
          
          if(env instanceof Point)
          {
            dataValue = new Double(1); //sending 1 as the value, so a full overlap
            //will give the value of one to that block
            
            Point area = (Point)env;
            //normalizing to res grid
            x = area.getX();
            mult = x/res;
            mult = Math.floor(mult);
            x = mult*res;
            
            y = area.getY();
            mult = y/res;
            mult = Math.floor(mult);
            y = mult*res;
            
            toAdd = new DataBlock(x, y, res, res);
            timeValue = new TreeMap();
            timeValue.put(new Double(time), dataValue);
            
            //check overwrite bit, if so, use hold instead of dataName
            if(overwrite.get(target))
            {
              //just replace name with hold, later, we will merge hold over old data
              toAdd.data.put(("hold"+target), timeValue);
            } else
            {
              //add data as normal
              toAdd.data.put(target, timeValue);
            }
            
            //merging this data into the current tree
            dataStruct.addData(toAdd, avg);
          } else //env is a Polygon
          {
//TODO***********************check this out******************************************
            /* WHAT THE CODE DOES (or is supposed to)
             * in this section we have a polygon, its value, and what field it should
             * be stored in. We get a rectangular bound on the polygon, then iterate though
             * each resolution sized block of it. Each block is tested to see if it (or how
             * much of it) is in the polygon, assuming this is more than 0, the data is added
             * to our dataset. For small polygons this is a simple process which gets run few times.
             * However, there are some very large and very detailed or oddly shaped polys.
             * These will possibly have to test thousands of blocks, i believe this is where
             * the program grinds to a halt.
             */
            
            /* THE PROBLEM
             * ok so... it seems like objects in this section are being created but
             * never garbage collected. (according to my profiler) 
             * though, when running with verbose garbage collection there is alot of
             * memory getting picked up in this section. either way, the program significantly
             * slows down here, and memory usage balloons. Memory usage is already very high,
             * because there are huge double matricies being stored.
             * Actual objects which are holding most memory are coordinants.
             * This may be because a Polygon is defined by these coordinants, so when you have
             * a large poly with detailed boarders there are an immence number of coords.
             * However the behavior exhibited here does not look like it is just one
             * large object being created at a time. If that were the case, and they were
             * being collected normally, memory usage would fluxuate up and down, whereas
             * in actual use it pretty consistantly increases.
             */
            Polygon area = (Polygon)env;

            double minX, maxX, minY, maxY;
            coords = area.getCoordinates();
            
            minX = coords[0].x;
            maxX = coords[0].x;
            minY = coords[0].y;
            maxY = coords[0].y;
            for(int i  = 1 ; i < coords.length; i++)
            {
              if(coords[i].x > maxX)
                maxX = coords[i].x;
              if(coords[i].x < minX)
                minX = coords[i].x;
              if(coords[i].y > maxY)
                maxY = coords[i].y;
              if(coords[i].y < minY)
                minY = coords[i].y;
            }
            
            //System.out.println("next geom -> "+minX+", "+maxX+", "+minY+", "+maxY);
            //normalizes lower bounds (upper dont matter)
            mult = minX/res;
            mult = Math.floor(mult);
            minX = mult*res;
            
            mult = minY/res;
            mult = Math.floor(mult);
            minY = mult*res;
            mult = maxY/res;
            mult = Math.ceil(mult);
            maxY = mult*res;
            
            //TODO assume the fact that this gets run 1000's of times is a big deal...
            for(double X = minX; X < maxX; X+=res)
            {
              //normalize once before instead of every time after
              for(double Y = maxY; Y > minY; Y-=res)
              {
                //System.out.print(".");
                //getting the fraction of this block which is in the Geometry
                //this will be the passed data value (as we are storing fractional
                //coverages)
                makeLR[0].x = X;
                makeLR[0].y = Y;
                makeLR[1].x = X;
                makeLR[1].y = (Y-res);
                makeLR[2].x = (X+res);
                makeLR[2].y = (Y-res);
                makeLR[3].x = (X+res);
                makeLR[3].y = Y;
                makeLR[4].x = X;
                makeLR[4].y = Y;
                
                //TODO this has got to be doing something terrible right...
                lr = gf.createLinearRing(makeLR);
                holdP = gf.createPolygon(lr, null);
                if(holdP.intersects(geom))
                {
                  Geometry over = holdP.intersection(geom);
                  //TODO these three calls which are to another api could be a problem
                  
                  dataValue = new Double(over.getArea()/(res*res));
                  
                  if(dataValue.doubleValue() > 0)
                  {
                    toAdd = new DataBlock(X, Y, res, res);
                    timeValue = new TreeMap();
                    timeValue.put(new Double(time), dataValue);
                    
                    //check overwrite bit, if so, use hold instead of dataName
                    if(overwrite.get(target))
                    {
                      //just replace name with hold, later, we will merge hold over old data
                      toAdd.data.put(("hold"+target), timeValue);
                    } else
                    {
                      //add data as normal
                      toAdd.data.put(target, timeValue);
                    }
                    
                    //merging this data into the current tree
                    dataStruct.addData(toAdd, avg);
                  }
                }
                
                //if this block doesnt overlap geometry dont add it at all
              }
            }
            //System.out.println("*");
            //System.out.println("\n -> done geom");
          }
//TODO***************************stop checking it out************************************
        }
        
        //done adding all data, if overwrite, must merge with old data now
        Map.Entry me;
        Iterator overIt = overwrite.entrySet().iterator();
        while(overIt.hasNext())
        {
          me = (Map.Entry)overIt.next();
          if((Boolean)me.getValue())
          {
            //then this particular 'target' was overwritten
            dataStruct.resolveOverwrite(("hold"+(String)me.getKey()), (String)me.getKey());
          }
        }
      } finally
      {
        collection.close(iter);
      }

    } catch(ShapefileException e)
    {
      log.log(Level.WARNING, "That aint no ShapeFile! -> "+fileName);
    } catch(IOException e)
    {
      log.log(Level.WARNING, "IOException!! -> "+fileName);
      e.printStackTrace();
    }
    
    log.log(Level.FINE, "Done adding new PolyShapefileEnum");
  }
  
  private void addNASAData(Element currFile)
  {
    /*
     * (there is no .tag for this data type, who knows what NASA is doing)
     * this is the data type supplied by nasa for, initially, solar radiance
     * data. this is essentially just a text file, broken up into columns.
     * columns corespond to lat and lon, and then average values for each
     * month, then year. as such, there is an option as to wether user wants a
     * single year value or 12 month values (.01 - .12)
     */
    log.log(Level.FINER, "begin function");
    boolean avg = true;
    boolean dec = true;
    boolean byMonth = false; //should info be read in for each month or just the year
    boolean overwrite = false;
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String ref = null;
    String unit = null;
    double time = 0;
    double res = 1;
    List infoChildren;
    Element currElem;
    TreeMap timeValue;
    Double dataValue;
    DataBlock toAdd;
    
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
      } else if(currElem.getName().equals("division"))
      {
        if(currElem.getAttributeValue("value").equals("month"))
        { //byMonth is default to false
          byMonth = true;
        }
      } else if(currElem.getName().equals("res"))
      {
        res = Double.parseDouble(currElem.getAttributeValue("value"));
      } else if(currElem.getName().equals("average"))
      {
        avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
      } else if(currElem.getName().equals("reference"))
      {
        ref = currElem.getAttributeValue("value");
      } else if(currElem.getName().equals("units"))
      {
        unit = currElem.getAttributeValue("value");
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
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    
  //opening NASA file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
      System.exit(1);
    }
  //txt file opened
    
    if(dataAvg.containsKey(dataName))
    {
      //then we are overwriting this data!
      overwrite = true;
    } else
    {
      //dont want to add all this information if we already have!!!
      if(!init)
      { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
        dataStruct.fillWorld(res);
        init = true;
      }
      //setting whether contained data is additive or averaged and references
      dataAvg.put(dataName, new Boolean(avg));
      if(ref != null)
      {
        dataRef.put(dataName, ref);
      }
      if(unit != null)
      {
        dataUnits.put(dataName, unit);
      }
      //done settign avg/add and references
    }
    
    
    //reading the data from the file
    String nextWord;
    double X, Y;
    while((nextWord = readWord(input)) != null)
    {
      //we know we have a line, so read all of this line
      
      if(dec)
      { //numbers stored in decimal format
        //get the location of this data
        Y = Double.parseDouble(nextWord);
        X = Double.parseDouble(readWord(input));
        for(int i = 1; i <= 12; i++)
        {
          nextWord = readWord(input);
          if(byMonth)
          { //if we are storing info for each 
            dataValue = Double.valueOf(nextWord);
            toAdd = new DataBlock(X, Y, res, res);
            timeValue = new TreeMap();
            timeValue.put(new Double((time+(i*.01))), dataValue);
            
            //check overwrite bit, if so, use hold instead of dataName
            if(overwrite)
            {
              //just replace name with hold, later, we will merge hold over old data
              toAdd.data.put("hold", timeValue);
            } else
            {
              //add data as normal
              toAdd.data.put(dataName, timeValue);
            }
          //merging this data into the current tree
            dataStruct.addData(toAdd, avg);
          }
        }
        nextWord = readWord(input);
        if(!byMonth)
        { //if we are just reading 1 value for a year then add this
          dataValue = Double.valueOf(nextWord);
          toAdd = new DataBlock(X, Y, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), dataValue);
          
          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite)
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put("hold", timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(dataName, timeValue);
          }
        //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
        }
      } else
      { //numbers stored in scientific notation
        Y = scientificToDouble(nextWord);
        X = scientificToDouble(readWord(input));
        for(int i = 1; i <= 12; i++)
        {
          nextWord = readWord(input);
          if(byMonth)
          { //if we are storing info for each 
            dataValue = new Double(scientificToDouble(nextWord));
            toAdd = new DataBlock(X, Y, res, res);
            timeValue = new TreeMap();
            timeValue.put(new Double((time+(i*.01))), dataValue);
            
            //check overwrite bit, if so, use hold instead of dataName
            if(overwrite)
            {
              //just replace name with hold, later, we will merge hold over old data
              toAdd.data.put("hold", timeValue);
            } else
            {
              //add data as normal
              toAdd.data.put(dataName, timeValue);
            }
          //merging this data into the current tree
            dataStruct.addData(toAdd, avg);
          }
        }
        nextWord = readWord(input);
        if(!byMonth)
        { //if we are just reading 1 value for a year then add this
          dataValue = new Double(scientificToDouble(nextWord));
          toAdd = new DataBlock(X, Y, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), dataValue);
          
          //check overwrite bit, if so, use hold instead of dataName
          if(overwrite)
          {
            //just replace name with hold, later, we will merge hold over old data
            toAdd.data.put("hold", timeValue);
          } else
          {
            //add data as normal
            toAdd.data.put(dataName, timeValue);
          }
        //merging this data into the current tree
          dataStruct.addData(toAdd, avg);
        }
        dataValue = new Double(scientificToDouble(nextWord));
      }
    }
    
    //done adding all data, if overwrite, must merge with old data now
    if(overwrite)
    {
      dataStruct.resolveOverwrite("hold", dataName);
    } //else we are done already
    try{
      input.close(); //im such a good programmer closing my files and whatnot
    } catch(IOException e){}
  //done reading data from file
  }
  
  private void addRasterData(Element currFile)
  { //i hope i can do this too...
    log.log(Level.SEVERE, "Function not implemented yet");
    // throw unsupportedoperationexception?
  }
  
  private void addTxtRegion(Element currFile)
  {
    log.log(Level.FINER, "begin function");
    
    int nRegions = 0;
    int rblock;
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
        log.log(Level.WARNING, "Unknown File Tag -> "+currInfo.getName());
      }
    }
  //done getting file info from XML
    
  //opening txt file for reading
    BufferedReader input = null;
    try {
      input = new BufferedReader( new FileReader(fileName));
    } catch (FileNotFoundException ex) 
    {
      log.log(Level.SEVERE, "FileNotFoundException!!!");
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
      log.log(Level.SEVERE, "seriously, the file was just there, it really really shouldnt be not found");
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
  
  private void addNetCDFRegion(Element currFile)
  {
    /*
     * ok... here we go, basically just extract a 2D array from the netcdf variable in the file
     * then run through it just like in addTxtRegion, should be a snap
     */
    log.log(Level.FINER, "begin function");
    
    int rblock, NaN;
    String fileName = "init";
    String dataVar = "ctry";
    String holdK;
    List infoList;
    Element currInfo;
    RegionMask holdR;
    TreeMap<String, RegionMask> newRegions = new TreeMap<String, RegionMask>();
    TreeMap<String, RegionMask> nameReverseMap = new TreeMap<String, RegionMask>();
    
  //getting file info from XML file
    infoList = currFile.getChildren();
    for(int i = 0; i < infoList.size(); i++)
    {
      currInfo = (Element)infoList.get(i);
      if(currInfo.getName().equals("name"))
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
          holdK = currR.getAttributeValue("key");
	  String regionName = currR.getAttributeValue("value");
	  // have to account for numerous keys to 1 region name
	  // so we have a reverse map and have the key point to the
	  // correct single instance of the RegionMask
	  holdR = nameReverseMap.get(regionName);
	  if(holdR == null) {
		  holdR = new RegionMask(regionName, 0);
		  nameReverseMap.put(regionName, holdR);
	  }
          newRegions.put(holdK, holdR);
        }
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag in addNetCDFRegion -> "+currInfo.getName());
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
      int[] shp = data.getShape();
      /*
      for(int i = 0; i < shp.length; ++i){
	      System.out.println(i+" -- "+shp[i]);
      }
      */

      // get the lat index from the data to get correct lat coordinates
      Variable latVar = nc.findVariable(data.getDimension(shp.length-2).getName());
      Array latArray = latVar.read();
      int[] latShp = latArray.getShape();
      Index latI = latArray.getIndex();
      double startY = latArray.getDouble(latI.set(0));
      double endY = latArray.getDouble(latI.set(latShp[0]-1));
      double resY = (startY - endY) / (shp[shp.length-2]-1);
      // move from middle of the cell to lower left of the cell
      startY = startY - resY/2;
      endY = endY - resY/2;

      // get the lon index from the data to get correct lon coordinates
      Variable lonVar = nc.findVariable(data.getDimension(shp.length-1).getName());
      Array lonArray = lonVar.read();
      int[] lonShp = lonArray.getShape();
      Index lonI = lonArray.getIndex();
      double startX = lonArray.getDouble(lonI.set(0));
      double endX = lonArray.getDouble(lonI.set(lonShp[0]-1));
      double resX = (endX - startX) / (shp[shp.length-1]-1);
      //double resX = 360.0 / shp[shp.length-1];
      // move from middle of the cell to lower left of the cell
      startX = startX - resX/2;
      endX = endX - resX/2;

      /*
      System.out.println("startY: "+startY);
      System.out.println("endY: "+endY);
      System.out.println("startX: "+startX);
      System.out.println("endX: "+endX);
      System.out.println("resY: "+resY);
      System.out.println("resX: "+resX);
      for(int i = 0; i < latShp[0]; ++i) {
	      System.out.println(i+" -- "+latArray.getDouble(latI.set(i)));
      }
      System.exit(0);
      */

      // I have to reset the resolution in the regions masks now that
      // we know what it is
      for(Iterator<Map.Entry<String, RegionMask>> it = nameReverseMap.entrySet().iterator(); it.hasNext(); ) {
	      it.next().getValue().resolution = resX;
      }

      Array ma2Array = data.read();
      Index in = ma2Array.getIndex();
      int i = 0;
      int k = 0;
      NaN = (int)data.findAttribute("missing_value").getNumericValue().floatValue();

      // set up for the land fraction
      //dataStruct.setTrackSums(true);
      final DataBlock toAdd = new DataBlock();
      final TreeMap<Double, Double> timeValue = new TreeMap();
      timeValue.put(0.0, 1.0);
      toAdd.data.put("landFract", timeValue);
      dataAvg.put("landFract", new Boolean(true));

      /*
      double landArea = 0;
      double surfaceArea = 0;
      Rectangle2D.Double currBlock = new Rectangle2D.Double();
      */

      double y = startY;
      double x = startX;
      for(i = 0; i < shp[shp.length-2]; ++i )
      //for(/*double y = startY*/; y >= endY; y-=resY)
      {
        //k = 0;
	x = startX;
	for(k = 0; k < shp[shp.length-1]; ++k)
        //for(/*double x = startX*/; x <= endX; x+=resX)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
	  /*
	  currBlock.setRect(x, y, resX, resY);
	  double area = FlatIndex.getArea(currBlock);
	  surfaceArea += area;
	  */
          if(rblock != NaN)
          {
            if(rblock >= 0 && (holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)))) != null)
            { 
	      //landArea += area;
	      // add to the land fraction
	      toAdd.setRect(x, y, resX, resY);
	      dataStruct.addData(toAdd, true);

	      //updating someones bounds
              if(holdR.height==-1)
              { //this is the first block being added to this region nothing to
                // test against yet
                holdR.y = y;
                holdR.x = x;
                holdR.height = resY;
                holdR.width = resX;
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
                if((x+resX)>(holdR.x+holdR.width))
                {
                  holdR.width = ((x+resX)-holdR.x);
                }
              }
            }
          }
          //k++;
	  x += resX;
        } //these two are kindof important, thats how we iterate through the data! huzzah!
        //i++;
	y -= resY;
      }
      /*
      System.out.println("When done: "+i+" -- "+k);
      System.out.println("When done: "+y+" -- "+x);
      //DONE CREATING MASK MATRIX AND FINDING BOUNDS
      System.out.println("Surface area: "+surfaceArea);
      System.out.println("Land area: "+landArea);
      */
      
      //initializing byte matrix for each region
      Iterator it = nameReverseMap.entrySet().iterator();
      Map.Entry me;
      while(it.hasNext())
      {
        me = (Map.Entry)it.next();
	holdR = (RegionMask)me.getValue();
      //System.out.println("region "+holdR.name+" dim: "+holdR.bMask.length+"x"+holdR.bMask[0].length);
      //System.out.println("region "+holdR.name+" coord: x: "+holdR.x+" y: "+holdR.y+" h: "+holdR.height+" w: "+holdR.width);
        ((RegionMask)me.getValue()).makeMatrix();
      }
      //done initializing byte matricies
      
      //MAIN BYTEMASK CREATION LOOP
      i = 0;
      k = 0;
      y = startY;
      for(i = 0; i < shp[shp.length-2]; ++i)
      //for(/*double y = startY*/; y >= endY; y-=resY)
      {
        k=0;
	x = startX;
	for(k = 0; k < shp[shp.length-1]; ++k)
        //for(/*double x = startX*/; x <= endX; x+=resX)
        {
          rblock = (int)ma2Array.getFloat(in.set(0, 0, i, k));
          if((rblock > 0)&&(rblock != NaN)&&
			  (holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)))) != null)
          {
          	holdR.setPointTrue(x, y);
          }
          //k++;
	  x += resX;
        }
        //i++;
	y -= resY;
      }
      //DONE SETTING MASKS   
    } catch (java.io.IOException e) {
      log.log(Level.SEVERE, "Error reading NetCDF file -> "+fileName);
    }
    
    //adding these regions masks to the master list of masks
    // TODO: I could just do some addAll
    Iterator it = nameReverseMap.entrySet().iterator();
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

  private void addUNEPRegion(Element currFile)
  {
	  log.log(Level.FINER, "begin function");

	  double res = 1;
	  int rblock, NaN = 0;
	  String fileName = "init";
	  String holdK;
	  int maskArray[][];
	  List infoList;
	  Element currInfo;
	  RegionMask holdR;
	  TreeMap<String, RegionMask> newRegions = new TreeMap<String, RegionMask>();
	  int numCols = 0, numRows = 0;
	  double xLL = 0.0, yLL = 0.0;
	  double currX, currY;
	  BufferedReader input = null;

	  //getting file info from XML file
	  infoList = currFile.getChildren();
	  for(int i = 0; i < infoList.size(); i++)
	  {
		  currInfo = (Element)infoList.get(i);
		  if(currInfo.getName().equals("name"))
		  {
			  /* WARNING: the file name must come before the region list
			   * because the res info is read from the file, and that info
			   * is needed to create a new region.
			   */
			  fileName = currInfo.getAttributeValue("value");
			  //opening txt file for reading
			  try {
				  input = new BufferedReader(new FileReader(fileName));
			  } catch (FileNotFoundException ex) 
			  {
				  log.log(Level.SEVERE, "FileNotFoundException!!!");
				  System.exit(0);
			  }
			  /*
			   * read numcols
			   * numrows
			   * xll corner
			   * yll corner
			   * cellsize (res)
			   * NODATA_value
			   */

			  double shift;
			  readWord(input); //reading ncols
			  numCols = Integer.valueOf(readNumber(input));
			  //System.out.println("numCols "+numCols);
			  readWord(input); //reading nrows
			  numRows = Integer.valueOf(readNumber(input));
			  //System.out.println("numRows "+numRows);
			  readWord(input); //reading xllcorner
			  xLL = Double.valueOf(readNumber(input));
			  //System.out.println("xLL "+xLL);
			  readWord(input); //reading yllcorner
			  yLL = Double.valueOf(readNumber(input));
			  //System.out.println("yLL "+yLL);
			  readWord(input); //reading cellsize
			  res = Double.valueOf(readNumber(input));
			  res = 360.0 / numCols;
			  //System.out.println("res "+res);
			  readWord(input); //reading NODATA_value
			  //ignore = Double.valueOf(readNumber(input));
			  NaN= (int)Double.parseDouble(readNumber(input));
			  //System.out.println("ignore "+ignore);

			  //rectifying bounds if they are out of legal values
			  if(xLL < -180)
			  {
				  shift = (xLL*-1)-180;
				  xLL = -180;
				  log.log(Level.WARNING, fileName+" shifted right by "+shift+" to "+xLL);
			  } else
			  {
				  shift = (xLL+(numCols*res))-180;
				  if(shift > 0)
				  {
					  xLL -= shift;
					  log.log(Level.WARNING, fileName+" shifted left by "+shift+" to "+xLL);
				  }
			  }
			  if(yLL < -90)
			  {
				  shift = (xLL*-1)-90;
				  yLL = -90;
				  log.log(Level.WARNING, fileName+" shifted up by "+shift+" to "+yLL);
			  } else
			  {
				  shift = (yLL+((numRows+1)*res))-90;
				  if(shift > 0)
				  {
					  yLL -= shift;
					  log.log(Level.WARNING, fileName+" shifted down by "+shift+" to "+yLL);
				  }
			  }
			  //done rectifying bounds
		  } else if(currInfo.getName().equals("variable"))
		  {
			  //dataVar = currInfo.getAttributeValue("value");
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
			  log.log(Level.WARNING, "Unknown File Tag in addNetCDFRegion -> "+currInfo.getName());
		  }
	  }
	  //done getting file info from XML

	  // set up for the land fraction
	  //dataStruct.setTrackSums(true);
	  final DataBlock toAdd = new DataBlock();
	  final TreeMap<Double, Double> timeValue = new TreeMap();
	  timeValue.put(0.0, 1.0);
	  toAdd.data.put("landFract", timeValue);
	  dataAvg.put("landFract", new Boolean(true));

	  /*
	  double landArea = 0;
	  double surfaceArea = 0;
	  Rectangle2D.Double currBlock = new Rectangle2D.Double();
	  */

	  //reading the data from the file
	  maskArray = new int[numRows][numCols];
	  currY = (yLL+(numRows*res));
	  for(int i = 0; i < numRows; i++)
	  {
		  currX = xLL;
		  for(int k = 0; k <numCols; k++)
		  {
			  rblock = (int)Double.parseDouble(readNumber(input));
			  /* TODO: put in a debug construct to print this kind of thing..
			  if(currX > -77 && currX < -75 && currY > 37 && currY < 40) {
				  System.out.println("("+currX+", "+currY+") - "+rblock);
			  }
			  currBlock.setRect(currX, currY, res, res);
			  double area = FlatIndex.getArea(currBlock);
			  surfaceArea += area;
			  */
			  if(rblock == NaN)
			  {
				  maskArray[i][k] = 0;
			  } else
			  {
				  //landArea += area;
				  // add to the land fraction
				  toAdd.setRect(currX, currY, res, res);
				  dataStruct.addData(toAdd, true);

				  maskArray[i][k] = rblock;
				  // check to make sure this is a valid region and the region
				  // defs file defines this region..
				  if(rblock >= 0 && 
						  (holdR = (RegionMask)newRegions.get(String.valueOf(rblock))) != null)
				  { //updating someones bounds
					  //holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
					  if(holdR.height==-1)
					  { //this is the first block being added to this region nothing to
						  // test against yet
						  holdR.y = currY;
						  holdR.x = currX;
						  holdR.height = res;
						  holdR.width = res;
					  } else
					  { //test against old bounds, if outside them, change them
						  if(currY<holdR.y)
						  { //y will never be higher, only lower
							  holdR.height += (holdR.y-currY);
							  holdR.y = currY;
						  }
						  if(currX<holdR.x)
						  { //x may be higher or lower
							  holdR.width += (holdR.x-currX);
							  holdR.x = currX;
						  }
						  if((currX+res)>(holdR.x+holdR.width))
						  {
							  holdR.width = ((currX+res)-holdR.x);
						  }
					  }
				  }
			  }
			  currX += res;
		  }
		  currY -= res;
	  }
	  /*
	  System.out.println("Surface area: "+surfaceArea);
	  System.out.println("Land area: "+landArea);
	  */
	  try {
		  input.close();
	  } catch(IOException ioe) {
		  log.log(Level.WARNING, "Couldn't close file: "+ioe);
	  }

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

	  currY = (yLL+(numRows*res));
	  for(int i = 0; i < numRows; i++)
	  {
		  currX = xLL;
		  for(int k = 0; k <numCols; k++)
		  {
			  rblock = maskArray[i][k];
			  if((rblock > 0)&&(rblock != NaN)&&
						  (holdR = (RegionMask)newRegions.get(String.valueOf(rblock))) != null)
			  {
				  //holdR = ((RegionMask)newRegions.get(String.valueOf(rblock)));
				  holdR.setPointTrue(currX, currY);
			  }
			  currX += res;
		  }
		  currY -= res;
	  }
	  //DONE SETTING MASKS   

	  //adding these regions masks to the master list of masks
	  it = newRegions.entrySet().iterator();
	  while(it.hasNext())
	  {
		  me = (Map.Entry)it.next();
		  holdR = ((RegionMask)me.getValue());
		  regionList.add(holdR.name);
		  maskList.put(holdR.name, holdR);
	  }
	  //addLandFractRegion();
	  //done adding region masks
  }
  private void addLandFractRegion() {
	  // special region for landFract because it needs to be world wide
	  RegionMask holdR = new RegionMask("landFract", 0.0);
	  holdR.x = -180.0;
	  holdR.y = -90.0;
	  holdR.height = 180 / dataStruct.getResolution();
	  holdR.width = 360 / dataStruct.getResolution();
	  regionList.add(holdR.name);
	  maskList.put(holdR.name, holdR);
  }
  
  private void addGeoTiffFile(Element currFile)
  {
	  log.log(Level.FINER, "begin function");
	  String fileName = null;
	  Element currElem;
	  List infoChildren = currFile.getChildren();
	  double radius = 1.0;
	  Double timeDouble = new Double(0.0);
	  double res = 1;
      double resX = 1;
      double resY = 1;
	  boolean avg = true;
	  String dataName = "shutup,";
	  boolean overwrite = false;
	  for(int i = 0; i < infoChildren.size(); i++)
	  {
		  currElem = (Element)infoChildren.get(i);
		  if(currElem.getName().equals("name"))
		  {
			  fileName = currElem.getAttributeValue("value");
		  } else if(currElem.getName().equals("data"))
		  {
			  dataName = currElem.getAttributeValue("value");
		  } else if(currElem.getName().equals("date"))
		  {
			  timeDouble = Double.parseDouble(currElem.getAttributeValue("value"));
		  } else if(currElem.getName().equals("res"))
		  {
			  res = Double.parseDouble(currElem.getAttributeValue("value"));
		  } else if(currElem.getName().equals("average"))
		  {
			  avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
		  } else if(currElem.getName().equals("radius")) 
		  {
			  radius = Double.parseDouble(currElem.getAttributeValue("value"));
		  } else
		  {
			  log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
		  }

	  }
	  // TODO: put checks to make sure required input was found

	  FileSeekableStream fss = null;
	  try {
		  fss = new FileSeekableStream(fileName);
		  GeoTIFFDirectory gTiffDir = (GeoTIFFDirectory)new GeoTIFFFactory().createDirectory(fss, 0);
		  XTIFFField[] fields = gTiffDir.getFields();
		  // key 256 type 3 has width in pixels?
		  // key 257 type 3 has height in pixels?
		  // key 273 type 4 has offset strip
		  // key 279 type 4 has offset strip len
		  // key 33550 type 12 has pixel scale (in form (x,y,z)?)
		  // key 33922 type 12 has tie ponits(bounds??) in this case the 4th and 5th are (x, y) of upper left
		  // where do I get value scale info?

		  long[] offsets = gTiffDir.getField(273).getAsLongs();
		  long[] offsetSizes = gTiffDir.getField(279).getAsLongs();
		  double[] pixelScale = gTiffDir.getPixelScale();
		  double[] tiePts = gTiffDir.getTiepoints();
		  byte[] buff = null;

		  double currEasting = tiePts[3];
		  double currNorthing = tiePts[4];

		  HomolosineToDegreeConversion convInst = HomolosineToDegreeConversion.getInstance();

		  /*
		  Point2D.Double latLongInit = convInst.convert(new Point2D.Double(currEasting, currNorthing), radius);
		  Point2D.Double latLongChange = convInst.convert(new Point2D.Double(currEasting + pixelScale[1], currNorthing - pixelScale[0]), radius);
		  System.out.println("Change in lat: "+(Math.abs(latLongChange.getY() - latLongInit.getY())));
		  System.out.println("Will need to read: "+Math.ceil(res/(Math.abs(latLongChange.getY() - latLongInit.getY()))));
		  System.out.println("Change in long: "+(Math.abs(latLongChange.getX() - latLongInit.getX())));
		  System.out.println("Will need to read: "+Math.ceil(res/(Math.abs(latLongChange.getX() - latLongInit.getX()))));
		  if(1==1) {
			  throw new NullPointerException();
		  }
		  */

		  Point2D.Double latLong;
          Point2D.Double findRes;

		  double x;
		  double y;
		  double mult;
		  Double dataValue;
		  TreeMap timeValue;
		  DataBlock toAdd;

		  if(dataAvg.containsKey(dataName))
		  {
			  //then we are overwriting this data!
			  overwrite = true;
		  } else
		  {
			  //dont want to add all this information if we already have!!!
			  if(!init)
			  { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
				  dataStruct.fillWorld(res);
				  init = true;
			  }
			  //setting whether contained data is additive or averaged and references
			  dataAvg.put(dataName, new Boolean(avg));
			  /*
			  if(ref != null)
			  {
				  dataRef.put(dataName, ref);
			  }
			  if(unit != null)
			  {
				  dataUnits.put(dataName, unit);
			  }
			  //done settign avg/add and references
			  */
		  }

		  for(int i = 0; i < offsets.length; i++) {
			  fss.seek(offsets[i]);
			  if(buff == null || buff.length < offsetSizes[i]) {
				  buff = new byte[(int)offsetSizes[i]];
			  }
			  int read = fss.read(buff, 0, (int)offsetSizes[i]);
			  /*
			  if(read != (int)offsetSizes[l]) {
				  log.log(Level.WARNING, "Data read: "+read+" expected: "+offsetSizes[i]+" at offset: "+l);
			  }
			  */
			  // reset the easting back to initial
			  currEasting = tiePts[3];
			  for(int j = 0; j < read; j++) {
				  // should endianess be of concern here?
				  short temp = (short)(0x000000FF & ((int)buff[j]));
				  if(temp <= 100) {
					  latLong = convInst.convert(new Point2D.Double(currEasting, currNorthing), radius, true);
                      findRes = convInst.convert(new Point2D.Double(currEasting+pixelScale[1], currNorthing-pixelScale[0]), radius, false);
                      if(findRes == null)
                      {
                        System.out.println("problem lat: "+latLong.getX()+" lon: "+latLong.getY());
                      }
                      if(latLong == null) {
						  log.log(Level.WARNING, "Had a problem with this point: ("+currEasting+", "+currNorthing+")");
                        dataValue = Double.NaN;
					  } else {
						  dataValue = new Double(temp); // relative scale?
                          resX = findRes.getY()-latLong.getY();
                          resY = latLong.getX()-findRes.getX();
                      //System.out.println("storing value of: "+dataValue);
                      //System.out.println("with resolution of: "+res);
					  //System.out.println("Got back(lat, long): ("+latLong.getX()+", "+latLong.getY()+")");

						  // somehow the x/y or lat/long are getting flipped so I just flipped them here and
						  // it looks ok
					  toAdd = new DataBlock(latLong.getY(), latLong.getX(), resY, resX);
					  timeValue = new TreeMap();
					  timeValue.put(timeDouble, dataValue);

					  //check overwrite bit, if so, use hold instead of dataName
					  if(overwrite)
					  {
						  //just replace name with hold, later, we will merge hold over old data
						  toAdd.data.put("hold", timeValue);
					  } else
					  {
						  //add data as normal
						  toAdd.data.put(dataName, timeValue);
					  }
					  //merging this data into the current tree
					  dataStruct.addData(toAdd, avg);
					  }
				  }
				  currEasting += pixelScale[1];
			  }
			  currNorthing -= pixelScale[0];
		  }

		  /*
		  Point2D.Double topLeft = new Point2D.Double(-1998500.000, 4529500.000);
		  Point2D.Double latLong = ModelInterface.PPsource.util.HomolosineToDegreeConversion.getInstance().convert(topLeft, 6370997);
		  System.out.println("Got back(lat/long): ("+latLong.getX()+", "+latLong.getY()+")");
		  */

		  //done adding all data, if overwrite, must merge with old data now
		  if(overwrite)
		  {
			  dataStruct.resolveOverwrite("hold", dataName);
		  } //else we are done already

	  } catch(IOException ioe) {
		  ioe.printStackTrace();
		  //TODO: log this exception and exit?
	  } finally {
		  try {
			  fss.close();
		  } catch(IOException ioe) {
			  ioe.printStackTrace();
			  // TODO: log this exception
		  }
	  }

  }

public void addFLTFile(Element currFile) 
{
	log.log(Level.FINER, "begin function");
	String fileName = null;
	Element currElem;
	List infoChildren = currFile.getChildren();
	Double timeDouble = new Double(0.0);
	double res = 1;
	double resY, resX;
	boolean avg = true;
	String dataName = "shutup,";
	DataBlock toAdd;
	TreeMap timeValue;
	boolean overwrite = false;
	double NaN = 0.0;
	for(int i = 0; i < infoChildren.size(); i++)
	{
		currElem = (Element)infoChildren.get(i);
		if(currElem.getName().equals("name"))
		{
			fileName = currElem.getAttributeValue("value");
		} else if(currElem.getName().equals("data"))
		{
			dataName = currElem.getAttributeValue("value");
		} else if(currElem.getName().equals("date"))
		{
			timeDouble = Double.parseDouble(currElem.getAttributeValue("value"));
		} else if(currElem.getName().equals("res"))
		{
			res = Double.parseDouble(currElem.getAttributeValue("value"));
		} else if(currElem.getName().equals("average"))
		{
			avg = (Boolean.valueOf(currElem.getAttributeValue("value"))).booleanValue();
		} else if(currElem.getName().equals("missing-value"))
		{
			NaN = Double.parseDouble(currElem.getAttributeValue("value"));
		} else
		{
			log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
		}
	}
	if(dataAvg.containsKey(dataName))
	{
		//then we are overwriting this data!
		overwrite = true;
	} else
	{
		//dont want to add all this information if we already have!!!
		if(!init)
		{ //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
			dataStruct.fillWorld(res);
			init = true;
		}
		//setting whether contained data is additive or averaged and references
		dataAvg.put(dataName, new Boolean(avg));
	}
	try 
	{
		resY = resX = res;
		DataInputStream input = new DataInputStream(new FileInputStream(fileName));
		for(double y = (90-resY); y >= -90; y-=resY)
		{
			for(double x = -180; x < 180; x+=resX)
			{
				// this binary file seems to be the oposite endian java or my comp?? expects
				// so I am reading as int and reversing the bytes
				double dataValue = (double)Float.intBitsToFloat(Integer.reverseBytes(input.readInt()));
				if(dataValue != NaN) {
					toAdd = new DataBlock(x, y, resX, resY);
				      timeValue = new TreeMap();
				      timeValue.put(timeDouble, dataValue);

				      //check overwrite bit, if so, use hold instead of dataName
				      if(overwrite)
				      {
					      //just replace name with hold, later, we will merge hold over old data
					      toAdd.data.put("hold", timeValue);
				      } else
				      {
					      //add data as normal
					      toAdd.data.put(dataName, timeValue);
				      }

				      //merging this data into the current tree
				      dataStruct.addData(toAdd, avg);

				}
			}
		}
		//done adding all data, if overwrite, must merge with old data now
		if(overwrite)
		{
			dataStruct.resolveOverwrite("hold", dataName);
		} //else we are done already
    
		input.close(); //im such a good programmer closing my files and whatnot
		//done reading data from file
	} catch(IOException ioe) 
	{
		log.log(Level.SEVERE, "Couldn't read file "+fileName+": "+ioe);
		System.exit(1);
	}
}

//*****************************************************************************
//*********************Helper Functions****************************************
//*****************************************************************************
  /**
   * Sets up all reading and writing objects for the rest of the class.
   * These include JDOM {@link Document}s for reading in XML info, and
   * a {@link BufferedWriter} for writing the XML output file.
   */
  public void makeStreams()
  {
    log.log(Level.FINER, "begin function");
    
    //this function initializes all of the XML stream readers
    //i will add the code for additional readers as i need them
    //XMLInputFactory inputFactory = XMLInputFactory.newInstance();
    try
    {
      SAXBuilder builder = new SAXBuilder();
      iDocument = builder.build(iSource);
      rDocument = builder.build(rSource);
    } catch(FileNotFoundException e)
    {
      log.log(Level.SEVERE, "FileNotFound! in -> makeStreams : "+e.getMessage());
    } catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException encountered! in -> makeStreams : "+e.getMessage());
    } catch(JDOMException e)
    {
      log.log(Level.SEVERE, "JDOM Exception! in -> makeStreams : "+e.getMessage());
    }
  }
  private String readWord(BufferedReader input)
  {
    log.log(Level.FINEST, "begin function");
    //reads an entire word from an input stream rather than just a character
    //words delimited by any whitespace 'space, new line, tab'
    //if no word exists in the stream return... null!
    String build = new String();
    int read;
    char hold;
    try {
      do
      {
        /*flushing whitespace from the input stream*/
        read = input.read();
        hold = (char)read;
      } while((read != -1)&&(hold <= '\u0020'));
      
      if(read == -1)
      { //file is done stop before content was reached
        return null;
      }
        
      build = build.concat(String.valueOf((char)read));
      while(((read = input.read()) != -1)&&((hold = (char)read) > '\u0020'))
      {
        build = build.concat(String.valueOf(hold));
      }
      
    } catch (IOException ex)
    {
      log.log(Level.SEVERE, "IOException!");
      System.exit(1);
    }

    build = build.trim();
    
    if(build.length() > 0)
    {
      return build;
    } else
    {
      return null;
    }
      
  }
  private String readNumber(BufferedReader input)
  {
    log.log(Level.FINEST, "begin function");
    //reads a word from teh data stream, then if necessary converts to
    //a number (scientific->decimal)
    String build = readWord(input);
    
    if(build == null)
    {
      return null;
    }
    
    if(build.length() > 0)
    {
      if((build.indexOf('E') != -1)||(build.indexOf('e') != -1))
      {
        return String.valueOf(scientificToDouble(build));
      } else
      {
        return build;
      }
    } else
    {
      return null;
    }
  }
  private String readComma(BufferedReader input)
  {
    log.log(Level.FINEST, "begin function");
    //reads an entire word from an input stream rather than just a character
    //words delimited by any whitespace 'space, new line, tab' OR A COMMA
    //if no word exists in the stream return... null!
    String build = new String();
    int read;
    char hold;
    try {
      do
      {
        /*flushing whitespace from the input stream*/
        read = input.read();
        hold = (char)read;
      } while((read != -1)&&(hold <= '\u0020')&&(hold != ','));
      
      if(read == -1)
      { //file is done stop before content was reached
        return null;
      }
        
      build = build.concat(String.valueOf((char)read));
      while(((read = input.read()) != -1)&&((hold = (char)read) > '\u0020')&&(hold != ','))
      {
        build = build.concat(String.valueOf(hold));
      }
      
    } catch (IOException ex)
    {
      log.log(Level.SEVERE, "IOException!");
      System.exit(1);
    }
    
    build = build.trim();
    
    if(build.length() > 0)
    {
      return build;
    } else
    {
      return null;
    }
  }
  
  private double scientificToDouble(String sc)
  {
    log.log(Level.FINEST, "begin function");
    //takes a string of the form #.###E+### and converts it to a double
    double mantissa, exponent, expValue;
    boolean expSignPos = false;
    int E = sc.indexOf('E');
    if(E == -1)
    {
      E = sc.indexOf('e');
    }
    // if there was not any E/e at all then just try to 
    // convert the whole string to a double
    if(E == -1) {
	    return Double.parseDouble(sc);
    }
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
  /**
   * Returns a child element of the passed element which contains an attribute
   * of the passed type whos value equals the supplied value. This will only
   * search to one level (not children's children). 
   * @param currElem Element whos children will be tested.
   * @param att Name of the attribute to search in.
   * @param val Value of the attribute to test against.
   * @return Element which contains attribute value of null if DNE.
   */
  private Element attributeInChild(Element data, String att, String val)
  {
    log.log(Level.FINER, "begin function");
    List children = data.getChildren();
    Element currElem;
    String currVal;
    
    for(int i = 0; i < children.size(); i++)
    {
      currElem = (Element)children.get(i);
      currVal = currElem.getAttributeValue(att);
      if(currVal != null)
      {
        if(currVal.equals(val))
        {
          return currElem;
        }
      }
      
    }
    //if we get to this point the attribute value does not exist
    return null;
  }
  
  
  //end of class!
}
