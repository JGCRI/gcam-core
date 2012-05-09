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
package ModelInterface.ModelGUI2.csvconv;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.Element;

import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

/**
 * Provides a representation for a header entity.
 * @author Pralit Patel 
 * @see ModelInterface.ModelGUI2.csvconv.Header
 */ 
public class HeaderEntity {
	/**
	 * The node name of this entity.
	 */
	private String name;

	/**
	 * The attributes of this entity.  Note that the attr name can
	 * map to a String which is its attr value or null in which case
	 * it should read its attr value from the table.
	 */
	private Map<String, String> attributes;

	/**
	 * Whether or not this entity should attempt to read it's text data
	 * from the table.
	 */
	private boolean readData;

	/**
	 * Whether this entity has an attr that needs to read a value.
	 */
	private boolean readAttr;

	/**
	 * Whether this entity has attributes which specifies it's value.
	 */
	private boolean hasSpecificAttrVal;

	/**
	 * Constructor, takes a string representation of the entity and converts it
	 * to an internal format.  For a header similar to 
	 * "<em>(</em>{attr=val}gp<em>)</em>/.../<em>(</em>parent<em>)</em>/<em>(</em>+{attr1=val; attr2}child<em>)</em""
	 * any of the strings within the parentesis would be valid.
	 * @param headerStr A string representation that this entity should represent
	 */ 
	public HeaderEntity(String headerStr) throws Exception {
		attributes = new HashMap<String, String>();
		readData = false;
		readAttr = false;
		hasSpecificAttrVal = false;
		parseHeaderStr(headerStr);
	}

	/**
	 * Does the work to parse the headerStr and set the name, attributes, and readData
	 * accordingly.
	 * @param headerStr A string representation that this entity should represent
	 */ 
	private void parseHeaderStr(String headerStr) throws Exception {
		// should I use a StringBuilder here?
		if(headerStr.startsWith("@")) {
			// get rid of the gp path only symbol
			headerStr = headerStr.substring(1);
		}
		if(headerStr.startsWith("+")) {
			// assume read data until we find an attr
			// without a value at which time readData
			// will be reset to false
			readData = true; 
			headerStr = headerStr.substring(1);
		}
		if(headerStr.startsWith("{")) {
			// begin parsing attributes
			int findPos = headerStr.indexOf('}');
			String[] attrs = headerStr.substring(1, findPos).split(";");
			headerStr = headerStr.substring(findPos+1);
			for(String attr : attrs) {
				if((findPos = attr.indexOf('=')) == -1) {
					// must read the attr value from the table
					readData = false;
					readAttr = true;
					attributes.put(attr.trim(), null);
				} else {
					hasSpecificAttrVal = true;
					attributes.put(attr.substring(0, findPos).trim(), attr.substring(findPos+1));
				}
			}
		}
		// only the name should be left at this point.
		if(headerStr.matches("[\\+\\{\\}@=;]")) {
			// note that this does not garuntee the user did not put in characters 
			// intended to be in the node name that are not valid XML characters
		       throw new Exception("Invalid header "+headerStr+" found invalid characters after parsing");
		}
		name = headerStr.trim();
	}

	/**
	 * Get the name of this entity.
	 * @return The node name of this entity.
	 */ 
	public String getName() {
		return name;
	}

	/**
	 * Whether this entity will need to read any data at all.
	 * @return True if this entity will read data or attr value, 
	 * false otherwise. 
	 */
	public boolean willRead() {
		return readData || readAttr;
	}

	/**
	 * Whether this entity specifies any attribute values.
	 * @return True if this entity specifies an attr value,
	 * false otherwise.
	 */
	public boolean doesSpecifyAttrValue() {
		return hasSpecificAttrVal;
	}

	/**
	 * Get the set of attribute names for this entity.
	 * @return A set of attributes names
	 */
	public Set<String> getAttrNames() {
		return attributes.keySet();
	}

	/**
	 * Create the DOM Node representation of this entity.  Will read and set any
	 * attribute values or data value that should be read from the table.  The merge
	 * parameter would be used in the case where there are multiple attributes/data to
	 * be read for the same XML entity but the data is in different columns.
	 * @param doc The document which this node will belong to.
	 * @param currTableVal The current values in the table for which this entity is a header for.
	 * @param merge An initial Element to merge with. A null value means to create a new Node.
	 * @return The node representation of this element.
	 */
	public Node doCreate(Document doc, String currTableVal, Element merge) {
		Element ret;
		// If merge does not exist create a new one
		if(merge == null) {
			ret = doc.createElement(name);
		} else {
			ret = merge;
		}
		// go trough out attributes and add them on to the ret node
		for(Iterator<Map.Entry<String, String>> it = attributes.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry<String, String> currAttr = it.next();
			String currVal;
			// null value means read from table value
			if((currVal = currAttr.getValue()) == null) {
				currVal = currTableVal;
			}
			ret.setAttribute(currAttr.getKey(), currVal);
		}
		if(readData) {
			// set the test data to the value from the table
			ret.setTextContent(currTableVal);
		}
		return ret;
	}

	/**
	 * Checks to see of the given Header entity is the
	 * same as this entity. This checks the name, all attributes
	 * and read flags.
	 * @param obj The other entity that is being compared to this one.
	 * @return True if there are the same, false otherwise.
	 */
	public boolean equals(Object obj) {
		if(!(obj instanceof HeaderEntity)) {
			return false;
		}
		HeaderEntity other = (HeaderEntity)obj;
		return name.equals(other.name) && attributes.equals(other.attributes)
			&& (readData == other.readData) && (readAttr == other.readAttr) 
			&& (hasSpecificAttrVal == other.hasSpecificAttrVal);
	}

	/**
	 * Need to make sure hashCodes are equal if the objects are equal.
	 * @see Object.hashCode()
	 * @return a unique hash code.
	 */
	public int hashCode() {
		return name.hashCode() ^ attributes.hashCode() ^ Boolean.valueOf(readData).hashCode()
			^ Boolean.valueOf(readAttr).hashCode() ^ Boolean.valueOf(hasSpecificAttrVal).hashCode();
	}

	public String toString() {
		StringBuilder ret = new StringBuilder();
		if(readData || readAttr) {
			ret.append("+");
		}
		if(!attributes.isEmpty()) {
			ret.append("{");
			for(Iterator<Map.Entry<String, String>> it = attributes.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry<String, String> curr = it.next();
				ret.append(curr.getKey());
				if(curr.getValue() != null) {
					ret.append("=").append(curr.getValue());
				}
				ret.append(";");
			}
			ret.deleteCharAt(ret.length()-1).append("}");
		}
		return ret.append(name).toString();
	}
}
