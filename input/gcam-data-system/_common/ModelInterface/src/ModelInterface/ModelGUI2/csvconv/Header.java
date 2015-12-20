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

/**
 * Provides a representation of a single header.
 * @author Pralit Patel
 * @see ModelInterface.ModelGUI2.csvconv.Headers
 */
public class Header {
	/**
	 * The entity which represents this header's grand parent
	 * relationship.  Note that this value may be null which
	 * means that this header has no grand parent relationship.
	 */
	private HeaderEntity grandParent;

	/**
	 * The entity which represents this header's parent
	 * relationship.  Note this value may be null which
	 * would indicate that this header is the root.  There 
	 * should only be one such header in a set of headers
	 * unless the header set is a set of MAP.
	 */
	private HeaderEntity parent;

	/**
	 * The entity which represents the child in this header.
	 * This value may not be null.
	 */
	private HeaderEntity child;

	/**
	 * Flag wich indicates this header exists only to fill 
	 * in path structure from a grand parent to a parent.
	 */
	private boolean isGPPathOnly;

	/**
	 * The header pos in the list of headers.  Used to 
	 * determine which column to read from in the data
	 * table.
	 */
	private int headerPos;

	/**
	 * Constructor which takes a string representation of a single
	 * header and converts it to an internal format.
	 * @param headerStr String representing a single header.
	 */
	public Header(String headerStr, int headerPosIn) throws Exception {
		grandParent = null;
		parent = null;
		child = null;
		isGPPathOnly = false;
		headerPos = headerPosIn;
		parseHeaderStr(headerStr);
		// check to make sure child is no longer null
		assert(child != null);
	}

	/**
	 * Does the work in parsing a header string.
	 * @param headerStr String representing a single header.
	 */
	private void parseHeaderStr(String headerStr) throws Exception {
		String[] splitHeaderStr = headerStr.split("/");
		if(splitHeaderStr.length == 1) {
			// root header case
			child = new HeaderEntity(splitHeaderStr[0]);
		} else if(splitHeaderStr.length == 2) {
			// parent / child case
			isGPPathOnly = splitHeaderStr[0].startsWith("@");
			parent = new HeaderEntity(splitHeaderStr[0]);
			child = new HeaderEntity(splitHeaderStr[1]);
		} else if(splitHeaderStr.length == 4) {
			//  gp / ... / parent / child case
			// this can't be isGPPathOnly can it?
			grandParent = new HeaderEntity(splitHeaderStr[0]);
			parent = new HeaderEntity(splitHeaderStr[2]);
			child = new HeaderEntity(splitHeaderStr[3]);
		} else {
			// invalid header case..
			throw new Exception("Invalid header: "+headerStr);
		}
	}

	/**
	 * Determine if this header represents the root element.
	 * @return True if this is the root, false otherwise.
	 */
	public boolean isRoot() {
		return parent == null;
	}

	/**
	 * Determine if this header exists to complete grand parent
	 * path. Essentially if parent starts with an '@'.
	 * @return True if this header completes gp path, false otherwise.
	 */
	public boolean isCompleteGPPath() {
		return isGPPathOnly;
	}

	/**
	 * Determine if this header has a grand parent relationship.
	 * @return True if has a grand parent, false otherwise.
	 */
	public boolean hasGrandParent() {
		return grandParent != null;
	}

	/**
	 * Returns whether this header will specify which parent it will
	 * nest under.
	 * @return True if will, false otherwise.
	 */
	public boolean doesSpecifyParent() {
		return grandParent != null || parent.doesSpecifyAttrValue();
	}

	/**
	 * Return the name of the child entity
	 * @return The name of the child entity.
	 */
	public String getChildName() {
		return child.getName();
	}

	/**
	 * Return the column number this header should read from, or
	 * -1 if this Header is not supposed to read anything.
	 * @return Header pos if it reads, -1 otherwise.
	 */
	public int getReadColumnPos() {
		if(child.willRead()) {
			return headerPos;
		} else {
			return -1;
		}
	}

	/**
	 * Accessor function to get to the grand parent entity.  This should be
	 * used to call the doCreate method on the entity.
	 * @return The grand parent entity.
	 */
	public HeaderEntity getGrandParentEntity() {
		return grandParent;
	}

	/**
	 * Accessor function to get to the parent entity.  This should be
	 * used to call the doCreate method on the entity.
	 * @return The parent entity.
	 */
	public HeaderEntity getParentEntity() {
		return parent;
	}

	/**
	 * Accessor function to get to the child entity.  This should be
	 * used to call the doCreate method on the entity.
	 * @return The child entity.
	 */
	public HeaderEntity getChildEntity() {
		return child;
	}

	/**
	 * Determine if this header has a parent entity
	 * with node name parentName.
	 * @param parentName The name the parent should have.
	 * @return Whether this header qualifies to be a child with such as name.
	 */
	public boolean shouldBeChild(String parentName) {
		if(isRoot()) {
			// root is special case, it will never
			// be a child to anyone
			return false;
		}
		return parent.getName().equals(parentName);
	}

	public String toString() {
		String ret;
		if(isGPPathOnly) {
			ret = "@";
		} else {
			ret = "";
		}
		if(grandParent != null) {
			ret += grandParent+"/.../";
		}
		if(parent != null) {
			ret += parent+"/";
		}
		return ret+child;
	}
}
