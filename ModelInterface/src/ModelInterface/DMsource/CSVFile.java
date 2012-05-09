/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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
import java.util.LinkedHashMap;
import java.util.Queue;
import java.util.LinkedList;
import java.util.Iterator;

import java.io.IOException;
import java.io.Writer;
import java.io.BufferedReader;
import java.io.PrintWriter;

/**
 * Represents a CSV file that can be outputted to.  Note that this class
 * is not thread safe.
 * @author Pralit Patel
 */
public class CSVFile {
	/**
	 * Data is stored as a Map of a variable(column) name to it's
	 * column data.  Note that this map should preserve the order of
	 * which data was added.  Note this would imply that there should
	 * be no commas allowed in data written to a column however this
	 * will not be enforced and will result in shifted column names.
	 */
	private Map<String, Queue<String>> data;
	
	/**
	 * Whether this file was initialized with another a read in CSV file.
	 */
	private final boolean didInitialize;

	/**
	 * Default constructor which does not need to initialize from
	 * an exsiting CSV file.
	 */
	public CSVFile() {
		data = new LinkedHashMap<String, Queue<String>>();
		didInitialize = false;
	}

	/**
	 * Constructor that will read an exsisting CSV file to initialize
	 * itself which will give the feeling of appending to that file.
	 * @param reader The reader to read the existing file from.
	 * @throws IOException If there are any issues reading the file.
	 */
	public CSVFile(BufferedReader reader) throws IOException {
		String line = reader.readLine();
		// this count does not include the header row
		int rowNum = 0;
		if(line == null) {
			// file was empty so there is nothing to do
			didInitialize = false;
			return;
		}
		line.trim();
		String[] headers = line.split(",");
		data = new LinkedHashMap<String, Queue<String>>(headers.length);
		for(int i = 0; i < headers.length; ++i) {
			data.put(headers[i], new LinkedList<String>());
		}
		while((line = reader.readLine()) != null) {
			line.trim();
			String[] currRow = line.split(",");
			for(int i = 0; i < currRow.length; ++i) {
				if(i < headers.length) {
					data.get(headers[i]).offer(currRow[i]);
				} else {
					String tempColName = String.valueOf(i);
					Queue<String> col = data.get(tempColName);
					if(col == null) {
						col =  new LinkedList<String>();
						data.put(tempColName, col);
					}
					while(col.size() != rowNum) {
						col.offer("");
					}
					col.offer(currRow[i]);
				}
				++rowNum;
			}
		}
		didInitialize = true;
	}

	/**
	 * Determine if this CSVFile was initialized from a read in file.
	 * @return True if it read data, false otherwise.
	 */
	public boolean wasInitialized() {
		return didInitialize;
	}

	/**
	 * Returns a Writer which can be used to output to this CSV file.  The output
	 * will be written under the column varName.  Note that if there are really 
	 * more than 1 column then the varName should have ',' in it to sufficiently 
	 * handle that.
	 * @param varName The variable or name of the column which is being written for.
	 * @return A Writter that will write the output in the appropriate place in the output.
	 */
	public Writer getOutputter(String varName) {
		return new CSVOutputter(varName);
	}

	/**
	 * Writes out the CSV file that is represented to the provided output stream.
	 * @param outStream The stream to write to.
	 */
	public void doOutput(PrintWriter outStream) {
		// write headers first
		for(Iterator<String> it = data.keySet().iterator(); it.hasNext(); ) {
			outStream.print(it.next());
			outStream.print(",");
		}
		outStream.println();
		// TODO: this will leave an extra row of values, figure out how to avoid this
		boolean didOutput = true;
		while(didOutput) {
			didOutput = false;
			for(Iterator<Queue<String>> it = data.values().iterator(); it.hasNext(); ) {
				String val = it.next().poll();
				if(val != null) {
					didOutput = true;
					outStream.print(val);
				}
				outStream.print(",");
			}
			outStream.println();
		}
	}

	/**
	 * Used to write to the CSV file.  In particular it will
	 * write each line under the column of the varName it was
	 * initialized with.
	 * Note that this class is not thread safe.
	 */
	private class CSVOutputter extends Writer {
		/**
		 * The column to append to.
		 */
		Queue<String> col;

		/**
		 * Constructor that gets the column to write to according to 
		 * the varName.
		 * @param varName The variable or column to write under.
		 */
		public CSVOutputter(String varName) {
			col = data.get(varName);
			if(col == null) {
				col = new LinkedList<String>();
				data.put(varName, col);
			}
		}

		public void write(String buf) throws IOException {
			String[] lines = buf.split("\n");
			for(int i = 0; i < lines.length; ++i) {
				col.offer(lines[i].trim());
			}
		}

		public void write(char[] cbuf, int off, int len) throws IOException {
			write(String.valueOf(cbuf, off, len));
		}

		public void flush() throws IOException {
		}

		public void close() throws IOException {
		}
	}
}
