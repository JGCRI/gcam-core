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
package ModelInterface.ModelGUI2.csvconv;

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Holds a set of Header which makes a complete definition of 
 * how to take a table's data and turn it into XML.
 * @author Pralit Patel
 * @see ModelInterface.ModelGUI2.csvconvDOMTreeBuider
 */
public class Headers {
	/**
	 * A list of the headers.  This list must contain 
	 * one Header which is root.  If this set of headers
	 * represents a MAP header then all header should be
	 * root.
	 */
	private List<Header> headers;

	/**
	 * Constructor takes a string representation of a complete
	 * set of Headers and converts it to an internal structure.
	 * @param headersStr A string representation of a complete set of Headers.
	 */
	public Headers(String headerStr) throws Exception {
		String[] splitHeaders = headerStr.split(",");
		headers = new ArrayList<Header>(splitHeaders.length);
		for(int i = 0; i < splitHeaders.length; ++i) {
			headers.add(new Header(splitHeaders[i], i));
		}
		// just to make sure we have a root
		getRoot();
	}

	/**
	 * Get root header. Note this is not intended for use
	 * as a set of MAP headers.
	 * @return The header which contains the root element.
	 */
	public Header getRoot() throws Exception {
		for(Iterator<Header> it = headers.iterator(); it.hasNext(); ) {
			Header currHeader = it.next();
			if(currHeader.isRoot()) {
				return currHeader;
			}
		}
		// should never get here as it would mean there is
		// not root.
		throw new Exception("Invalid set of headers: Missing root header");
	}

	/**
	 * Gets the number of headers
	 * @return The number of headers.
	 */
	public int getNumHeaders() {
		return headers.size();
	}

	/**
	 * Get the header at the specified position.
	 * @param pos The position the header requested.
	 * @return The header the the requested position.
	 */
	public Header getHeader(int pos) {
		return headers.get(pos);
	}

	/**
	 * Get a list of headers which has a parent that is the
	 * same as the child header passed in.  This just simply
	 * checks all headers if they have a parent entity with the
	 * same name as passed in Header's child name.
	 * @param parent The parent header which needs to find child matches.
	 * @return A List of Headers that have the specified parent.
	 */
	public List<Header> getChildHeaders(Header parent) {
		List<Header> ret = new ArrayList<Header>();
		for(Iterator<Header> it = headers.iterator(); it.hasNext(); ) {
			Header currHeader = it.next();
			if(currHeader.shouldBeChild(parent.getChildName())) {
				ret.add(currHeader);
			}
		}
		return ret;
	}
}
