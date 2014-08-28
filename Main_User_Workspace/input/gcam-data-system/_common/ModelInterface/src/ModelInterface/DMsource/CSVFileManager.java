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
package ModelInterface.DMsource;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.logging.Logger;

import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.BufferedReader;
import java.io.PrintWriter;
import java.io.FileReader;
import java.io.Writer;

import org.jdom.Element;

/**
 * Manages CSV files which can be written two.  CSV files can be written
 * to by using getCSVOutputter.  They can be forced to be written back to 
 * the file by using the writeCSVFile or writeAllCSVFiles.  Note that unless
 * they are told to be writen explicity they will never be written back to
 * the files.
 * @see CSVFile For more details about how CSVFile output works.
 * @author Pralit Patel
 */
public class CSVFileManager {
	/**
	 * The instance of the mananger.
	 */
	private static final CSVFileManager instance = new CSVFileManager();

	/**
	 * The map of files which points a file name to the CSVFile object.
	 */
	private Map<String, CSVFile> files;

	/**
	 * A logger for messages.
	 */
	private final Logger log;

	/**
	 * Private constuctor so that there can only be one instance.
	 */
	private CSVFileManager() {
		files = new HashMap<String, CSVFile>();
		log = Logger.getLogger("DataManipulation");
	}

	/**
	 * Gets an instance of the mananger.
	 * @return An instance of the manager.
	 */
	public static CSVFileManager getInstance() {
		return instance;
	}

	/**
	 * Checks the cache to see if a file has already been opened.  If it
	 * has not yet then it is opened and added to the cache.
	 * If there is an error in opening the file null is returned.
	 * @param fileName The file to retrive/open.
	 * @param append If the file was supposed to be appended to.
	 * @return A CSVFile that represents fileName or null if there was
	 * 	a problem.
	 */
	private CSVFile getFileFromCache(String fileName, boolean append) {
		CSVFile ret = files.get(fileName);
		if(ret == null) {
			if(append) {
				FileReader reader = null;
				try {
					reader = new FileReader(fileName);
					ret = new CSVFile(new BufferedReader(reader));
				} catch(FileNotFoundException fnf) {
					log.warning("File "+fileName+" does not exist going to create a new file");
					ret = new CSVFile();
				} catch(IOException e) {
					log.warning("Could not open "+fileName+" due to "+e);
					return null;
				} finally {
					// ignore errors on close
					try { reader.close(); } catch(Exception ioe) {}
				}
			} else {
				ret = new CSVFile();
			}
			files.put(fileName, ret);
		}
		if(ret.wasInitialized() ^ append) {
			log.warning("Inconsistent append option found for "+fileName+
					"; Could cause loss of data.");
		}
		return ret;
	}

	/**
	 * Gets a writer that can be used to write to a CSV file based on the 
	 * configuration passed in.  Note that the required attributes are:
	 * csvfile -- The path to the CSV to be written to.
	 * colname -- The column to place data under.  If this is not specified
	 * 	it will use the value of variable.
	 * append -- If true it will append to the csvfile, otherwise it will
	 * 	overwrite.
	 * Note that null will be returned in the event of an error.
	 * @param comm The element which has the above config attributes.
	 * @return A writter which can be written to.
	 */
	public Writer getCSVOutputter(Element comm) {
		String fileName = comm.getAttributeValue("csvfile");
		String varName = comm.getAttributeValue("colname");
		if(varName == null) {
			varName = comm.getAttributeValue("variable");
		}
		boolean append = Boolean.valueOf(comm.getAttributeValue("append"));
		if(fileName == null || varName == null) {
			log.severe("Invalid CSV file settings, check csvfile and colname(or variable) attributes are set.");
			return null;
		}
		CSVFile target = getFileFromCache(fileName, append);
		return target != null ? target.getOutputter(varName) : null;
	}

	/**
	 * Writes the specified file to disk.  If no CSV file with the 
	 * supplied name exsits in this manager nothing will be done.
	 * @param fileName The file to write.
	 */
	public void writeCSVFile(String fileName) {
		CSVFile toWrite = files.get(fileName);
		if(toWrite != null) {
			writeCSVFile(fileName, toWrite);
		} else {
			log.warning("File "+fileName+" was not found.");
		}
	}

	/**
	 * Writes all of the CSV files that are being managed to disk. It will then remove
	 * them from the list of available files being managed.
	 */
	public void writeAllCSVFiles() {
		for(Iterator<Map.Entry<String, CSVFile>> it = files.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry<String, CSVFile> currFile = it.next();
			writeCSVFile(currFile.getKey(), currFile.getValue());
			it.remove();
		}
	}

	/**
	 * Write CSVFile object to disk.
	 * @param fileName Where to write the file.
	 * @param csvFile The file object that will write itself out.
	 */
	private void writeCSVFile(String fileName, CSVFile csvFile) {
		// TODO: try to check append to make sure I don't clobber stuff
		PrintWriter out = null;
		try {
			out = new PrintWriter(fileName);
			csvFile.doOutput(out);
		} catch(Exception e) {
			log.warning("Could not write "+fileName+" due to "+e);
		} finally {
			// ignore errors on close
			try { out.close(); } catch(Exception e) {}
		}
	}
}
