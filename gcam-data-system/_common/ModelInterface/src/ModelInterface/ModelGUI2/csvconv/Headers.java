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
