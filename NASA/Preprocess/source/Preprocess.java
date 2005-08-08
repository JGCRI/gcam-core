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
 * \brief Main class for data preprocessor. Wrapper type main class which just gets the actual
 *  xml files and calls the functions which run  the program.
 *
 * \author Vincent Nibali
 * \date $Date$
 * \version $Revision$
 */
/**
 * MASTER TODO LIST: *currently COMPLETE, RUNNING but INCORRECT to the best of my knowledge*
 * TODO: netcdf files are still reading in incorrect values even after the double adding
 * problem was taken care of
 * TODO: look into ways to optimize memory usage, possible replace of merge operation as this doubles
 * used memory size by building a seperate tree before the merge happens (maybe implement addDate as previously discussed)
 * 
 */
/* DEVELOPER COMMENTS
 * thoughts on addData: the idea of adding new data to an existing tree is a pretty good one, could work in
 * the same exact manner as merge does now, but on a block by block basis. oh wait, actually i guess not. hmm. Ok so,
 * if the res of the main tree is <= the adding data then your fine, because you extract and your. DAMNIT wrong again
 * ok, so the problem is that regardless of res the blocks might not line up correctly. so when you say extract data you
 * have to weight it, but can we use the same weighting algorithm as in merge? need to look into that. regarless, say we
 * can extract by block as in merge. the problem then becomes one of order. with a user defined resolution, or static res,
 * or even if the first data set read is at the sharpest res were fine. but if for some reason a data set with a lower res
 * is added first then theres a problem. there is no mechanism for reResing a tree so to speak, so we are stuck with
 * whatever resolution we start with, the only way to get around this would be to rebuild a new tree and merge in
 * key word there is merge, if you need this operation your saving absolutly no space.
 * There is the consideration of time however. building a whole new tree THEN merging every element kindof sucks. if we
 * could only merge on a lower resolution the rest could be done 'by block' this would be essentially the same bigO of
 * merge, but there would be no tree creation time (which is probably large). this is definately something to look into
 * if only for the speedup, im going to do some speed tests to see which parts of the program take the most time.
 * in the meantime this is on the bottom of the list, speed isnt of the utmost importance in this part of the tool.
 * 
 * hahaha, i think building the tree takes like... 95% of the time, soooo yeah ill def look into this
 */
package source;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

/**
 * Main class for data preprocessor. Wrapper type main class which just gets the actual 
 * xml files and calls the functions which run  the program.
 * 
 * @author Vincent Nibali
 * @version 1.0
 */
public class Preprocess
{
  public static void main(String[] args)
  {
    Document in;
    Element root, currFile;
    String dSource, rSource, dOutput;
    Logger log = Logger.getLogger("Preprocess");
    Handler fHand, cHand;
    String logLevel;
    try
    {
      SAXBuilder builder = new SAXBuilder();
      in = builder.build("PPfiles.xml");
      root = in.getRootElement();
      currFile = root.getChild("data");
      dSource = currFile.getAttributeValue("file");
      currFile = root.getChild("region");
      rSource = currFile.getAttributeValue("file");
      currFile = root.getChild("output");
      dOutput = currFile.getAttributeValue("file");
      //***setting up a logger for DataManipulator
      currFile = root.getChild("log");
      if(currFile == null)
      { //no given logging level, set to lowest output amount (regular use)
        logLevel = "WARNING";
      } else
      { //given a logging level, use that
        logLevel = currFile.getAttributeValue("level");
      }
      log.setLevel(Level.parse(logLevel));
      log.setUseParentHandlers(false);
      
      cHand = new ConsoleHandler();
      cHand.setLevel(Level.WARNING);
      log.addHandler(cHand);
      fHand = new FileHandler("PPLog.log");
      fHand.setLevel(Level.ALL);
      fHand.setFormatter(new SimpleFormatter());
      log.addHandler(fHand);
      //***done initing DM logger
      
      log.log(Level.INFO, "creating DataBuilder to run preprocessing");
      DataBuilder mainRun = new DataBuilder(dSource, rSource, dOutput);
      log.log(Level.INFO, "calling runAll in DataBuilder");
      mainRun.runAll();
      
    } catch(FileNotFoundException e)
    {
      log.log(Level.SEVERE, "FileNotFound! PPfiles.xml does not exist");
    } catch(JDOMException e)
    {
      log.log(Level.SEVERE, "JDOM Exception! in main function");
    }
    catch(IOException e)
    {
      log.log(Level.SEVERE, "IOException! for shame! in main function");
    }
    //this is gunna be silly....
    //****************testing stuff**************************
    //DataBuilder bigTest = new DataBuilder("input.xml", "inputR.xml", "out.xml");
    //bigTest.makeStreams();
    //bigTest.buildTree();
    //bigTest.readMasks();
    //bigTest.fillRegions();
    //bigTest.runAll();
    //((RegionMask)bigTest.maskList.get("Maryland")).printMask();
    //((RegionMask)bigTest.maskList.get("Texas")).printMask();
    //bigTest.dataTree.printTreeXML();
  }
}
