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
package ModelInterface.ModelGUI2;
import org.w3c.dom.traversal.NodeFilter;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.w3c.dom.ls.*;

public class ParseFilter implements LSParserFilter {
	/**
	 * Determines which Nodes are wanted in our document, the empty textnodes are
	 * not wanted.
	 * @param nodeArg current node being analyzed.
	 * @return flag indicating wheter the node was accepted or rejected
	 */
	public short acceptNode(Node nodeArg) {
		if (nodeArg.getNodeValue().matches("\\s*")) {
			return FILTER_REJECT;
		}
		return FILTER_ACCEPT;
	}

	/**
	 * Determines which types of Nodes should be checked as to whether they
	 * should be filtered out.
	 * @return flag for text nodes since we only want to analyze text nodes
	 */
	public int getWhatToShow() {
		return NodeFilter.SHOW_TEXT;
	}

	/**
	 * Called when the start tags for each element is read.
	 * @param elemtnArg the new tag being analyzed
	 * @return since we only filter empty text nodes we always return accept flag
	 */ 
	public short startElement(Element elementArg) {
		return FILTER_ACCEPT;
	}
}
