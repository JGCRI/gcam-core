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
package source;


import java.io.*;
import java.util.*;
import java.util.logging.*;
import java.awt.geom.*;
import ucar.ma2.*;
import ucar.nc2.*;
import org.jdom.*;
import org.jdom.input.*;
import org.jdom.output.*;

import javax.xml.stream.*;

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
  public QuadBucketTree dataTree; //a tree of all the aggregated data to be used to create regions
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
  private boolean init; //whether or not the dataTree has been initialized as of yet
  private boolean URes; //whether or not the User wishes to use their own resolution
  Logger log = Logger.getLogger("Preprocess"); //log class to use for all logging output

//*********************************************************
//*****************Class Constructors**********************
//*********************************************************  
  
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
    dataTree = new QuadBucketTree(-180, 180, -90, 90);
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
    dataTree = new QuadBucketTree(-180, 180, -90, 90);
    regionList = new TreeSet();
    maskList = new TreeMap();
    printList = new TreeMap();
    dataAvg = new TreeMap();
    dataRef = new TreeMap();
    dataUnits = new TreeMap();
  }
  
//*********************************************************
//*************Begin Functions Proper**********************
//********************************************************* 
  /**
   * Wrapper function which executes all others in the correct order.
   * Allows user to only call runAll to gather input and generate
   * output. runAll correctly accounts for both a regular run and the
   * option of aggregating datafiles into a passed seed file.
   */
  public void runAll()
  {
    /*
    System.out.println("Begin: waiting...");
    System.in.read();
    System.in.read();
    System.out.println("...going");
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
      dataTree.fillWorld(userRes);
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
  }
  
  /**
   * Reads all listed data files into a {@link QuadBucketTree}.
   * Opens every file listed in the XML data file seperately and
   * creates a new QuadBucketTree from the data before merging with the
   * existing master tree.
   */
  public void buildTree()
  {
    //TODO: add new file types as needed
    Element currFile;
    Element root = iDocument.getRootElement();
    List fileChildren = root.getChildren("file");
    
    if((!init)&&(Double.parseDouble(root.getAttributeValue("resolution")) > 0))
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
        addTxtData(currFile);
      } else if(currFile.getAttributeValue("type").equals("1x1"))
      {
        add1x1Data(currFile);
      } else if(currFile.getAttributeValue("type").equals("netcdf"))
      {
        addNetCDFData(currFile);
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

      rWriter.write("\t<variableInfo>\n");
      itVar = dataAvg.entrySet().iterator();
      while(itVar.hasNext())
      {
        var = (Map.Entry)itVar.next();
        rWriter.write("\t\t<variable name=\""+var.getKey()+"\">\n");
        rWriter.write("\t\t\t<average value=\""+var.getValue()+"\" />\n");
        if(dataUnits.containsKey(var.getKey()))
        {
          rWriter.write("\t\t\t<units value=\""+dataUnits.get(var.getKey())+"\" />\n");
        }
        if(dataRef.containsKey(var.getKey()))
        {
          rWriter.write("\t\t\t<reference value=\""+dataRef.get(var.getKey())+"\" />\n");
        }
        rWriter.write("\t\t</variable>\n");
      }
      rWriter.write("\t</variableInfo>\n");
      
      
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
            if(var.getKey() == "weight")
            { //weight is output not as a regular variable
              rWriter.write("\t\t</weight>\n");
            } else
            { //normal case
              rWriter.write("\t\t</variable>\n");
            }  
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
  /**
   * Gets location of the seed data from input xml file and creates a Document
   * object from that file which is returned. Used to add a set of data to an
   * already created larger set of data.
   * @return Document which contains previous data.
   */
  public void readCurrentData()
  {
    Element root = iDocument.getRootElement();
    Element seed = root.getChild("seed");
    
    
    XMLInputFactory inputFactory = XMLInputFactory.newInstance();
    try
    {
      SAXBuilder builder = new SAXBuilder();
      sSource = seed.getAttributeValue("name");
      sDocument = builder.build(sSource);
    } catch(FileNotFoundException e)
    {
      log.log(Level.SEVERE, "FileNotFound! oh noes! in -> readCurrentData");
    } catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException encountered! oh noes! in -> readCurrentData");
    } catch(JDOMException e)
    {
      log.log(Level.SEVERE, "JDOM Exception! grarrrr! in -> readCurrentData");
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
    int msizeX, msizeY;
    double normX, normY, normW, normH; //the normalized bounds of the region (rounded to a multiple of data resolution)
    double holdMX, holdMY, work;
    Map.Entry var, time, data;
    Element root = sDocument.getRootElement();
    Element currElem;
    Element currReg, currVar;
    Element toAdd, toChild, toChild2;
    String rName, vName, tName;
    TreeMap holdToPrint;
    
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
      holdToPrint = (TreeMap)printList.get(rName);
      currReg = attributeInChild(root, "name", rName); //getting region node in currReg
      
      if(!holdToPrint.isEmpty())
      {
        //BEGIN ADDING OF SPECIFIC REGION DATA
        //calculating normalized bounds of region
        work = ((RegionMask)maskList.get(rName)).x
            +((RegionMask)maskList.get(rName)).width;
        holdMX = Math.ceil(work/dataTree.resolution)*dataTree.resolution;
        work = ((RegionMask)maskList.get(rName)).y
            +((RegionMask)maskList.get(rName)).height;
        holdMY = Math.ceil(work/dataTree.resolution)*dataTree.resolution;
        normX = Math.floor(((RegionMask)maskList.get(rName)).x/dataTree.resolution)
            *dataTree.resolution;
        normY = Math.floor(((RegionMask)maskList.get(rName)).y/dataTree.resolution)
            *dataTree.resolution;
        normW = holdMX-normX;
        normH = holdMY-normY;
        msizeY = (int)Math.ceil(normH/dataTree.resolution);
        msizeX = (int)Math.ceil(normW/dataTree.resolution);

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
              itTime = ((TreeMap)var.getValue()).entrySet().iterator();
              while(itTime.hasNext())
              {
                time = (Map.Entry)itTime.next();
                tName = ((Double)time.getKey()).toString();
                toChild = new Element("time");
                toChild.setAttribute("value", tName);

                //iterate through data, print in array style
                itData = ((TreeMap)time.getValue()).entrySet().iterator();
                while(itData.hasNext())
                {
                  data = (Map.Entry)itData.next();
                  toChild2 = new Element("data");
                  toChild2.setAttribute("y",String.valueOf(Math
                      .abs((int)(((((Point2D.Double)data.getKey()).y+dataTree.resolution)-(normY+normH))/dataTree.resolution))));
                  toChild2.setAttribute("x", String.valueOf((int)((((Point2D.Double)data
                      .getKey()).x-normX)/dataTree.resolution)));
                  toChild2.setAttribute("value", ((Double)data.getValue()).toString());
                  //adding the created data to this time
                  toChild.addContent(toChild2);
                }
                //adding the created time to this variable
                toAdd.addContent(toChild);
              }
              //adding the created variable to this region
              currReg.addContent(toAdd);
            } else
            { //this variable already exists, add the time to it
              //iterating through times
              itTime = ((TreeMap)var.getValue()).entrySet().iterator();
              while(itTime.hasNext())
              {
                time = (Map.Entry)itTime.next();
                tName = ((Double)time.getKey()).toString();
                if(attributeInChild(currVar, "value", tName) == null)
                {
                  toAdd = new Element("time");
                  toAdd.setAttribute("value", tName);

                  //iterate through data, print in array style
                  itData = ((TreeMap)time.getValue()).entrySet().iterator();
                  while(itData.hasNext())
                  {
                    data = (Map.Entry)itData.next();
                    toChild = new Element("data");
                    toChild.setAttribute("y",String.valueOf(Math
                        .abs((int)(((((Point2D.Double)data.getKey()).y+dataTree.resolution)-(normY+normH))/dataTree.resolution))));
                    toChild.setAttribute("x", String.valueOf((int)((((Point2D.Double)data
                        .getKey()).x-normX)/dataTree.resolution)));
                    toChild.setAttribute("value", ((Double)data.getValue()).toString());
                    //adding the created data to this time
                    toAdd.addContent(toChild);
                  }
                  //adding the created time to this variable
                  currVar.addContent(toAdd);
                } else
                { //this time already exists! trying to add a data set which is already present!!!
                  log.log(Level.WARNING, "Attempted to add a data set which already existed -> "+vName+" at "+tName);
                }
              }
            }
          }
        }//END VARIABLE ITERATE
      }//END WRITING REGION
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
  
//**********************************************************
//********************Component Readers*********************
//**********************************************************
  
  private void addTxtData(Element currFile)
  { /* function will add the data from the specified file of type 'txt'
     * 'txt'- defined as 180/resolution lines of 360/resolution data elements
     * tagged or untagged - basically using this to debug program as i build it
     */
    log.log(Level.FINER, "begin function");
    boolean readTags = true;
    boolean tagged = true;
    boolean avg = true;
    boolean dec = true;
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
      dataTree.fillWorld(res);
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
      //merging this data into the current tree
        dataTree.addData(toAdd, avg);
      }
    }
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
    
    boolean readTags = true;
    boolean tagged = true;
    boolean avg = true;
    boolean dec = true;
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
    
    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataTree.fillWorld(res);
      init = true;
    }
    
    //setting whether contained data is additive or averaged
    dataAvg.put(dataName, new Boolean(avg));
    if(ref != null)
    {
      dataRef.put(dataName, ref);
    }
    if(unit != null)
    {
      dataUnits.put(dataName, unit);
    }
    //done settign avg/add
  
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
        toAdd.data.put(dataName, timeValue);
        
      //merging this data into the current tree
        dataTree.addData(toAdd, avg);

        //prepping for next run
        readString = input.readLine();
      }
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
    String dataName = "shutup,";
    String fileName = "it is initialized thanks";
    String dataVar = "farea"; //what variable in the NetCDF file to get data from
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
      } else
      {
        log.log(Level.WARNING, "Unknown File Tag -> "+currElem.getName());
      }
      
    }
  //done reading from XML file
    

    if(!init)
    { //IMPORTANT CODE- if this is first file read and user didnt specify a resolution use this files res
      dataTree.fillWorld(res);
      init = true;
    }
    //setting whether contained data is additive or averaged
    dataAvg.put(dataName, new Boolean(avg));
    if(ref != null)
    {
      dataRef.put(dataName, ref);
    }
    if(unit != null)
    {
      dataUnits.put(dataName, unit);
    }
    //done settign avg/add
    
  //reading the data from the file
    try 
    {
      NetcdfFile nc = NetcdfFile.open(fileName);
      /* Read a variable named *readin* from the file, it contains the masks */
      Variable data = nc.findVariable(dataVar);
      Array ma2Array = data.read();
      Index in = ma2Array.getIndex();
      int i = 0;
      int k = 0;
      int NaN = (int)data.findAttribute("missing_value").getNumericValue().floatValue();

      for(double y = (90-res); y >= -90; y-=res)
      {
        k=0;
        for(double x = -180; x < 180; x+=res)
        {
          dataValue = new Double((double)ma2Array.getFloat(in.set(0, 0, i, k)));
          toAdd = new DataBlock(x, y, res, res);
          timeValue = new TreeMap();
          timeValue.put(new Double(time), dataValue);
          toAdd.data.put(dataName, timeValue);

        //merging this data into the current tree
          dataTree.addData(toAdd, avg);
          
          k++;
        }
        i++;
      }  
    } catch (java.io.IOException e) {
      log.log(Level.SEVERE, "Error reading NetCDF file -> "+fileName);
    }
  //done reading data from file
    
  }
  
  private void addTxtRegion(Element currFile)
  {
    log.log(Level.FINER, "begin function");
    
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
     * then run through it just like in addTxtRegion, should be snap
     */
    log.log(Level.FINER, "begin function");
    
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
      log.log(Level.SEVERE, "Error reading NetCDF file -> "+fileName);
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
    XMLInputFactory inputFactory = XMLInputFactory.newInstance();
    try
    {
      SAXBuilder builder = new SAXBuilder();
      iDocument = builder.build(iSource);
      rDocument = builder.build(rSource);
    } catch(FileNotFoundException e)
    {
      System.out.println("FileNotFound! oh noes! in -> makeStreams");
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
    log.log(Level.FINEST, "begin function");
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
    log.log(Level.FINEST, "begin function");
    //takes a string of the form #.###E+### and converts it to a double
    double mantissa, exponent, expValue;
    boolean expSignPos = false;
    int E = sc.indexOf('E');
    if(E == -1)
    {
      E = sc.indexOf('e');
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
