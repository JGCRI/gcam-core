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
package ModelInterface.ModelGUI2;

/**
 * Simple immutabe class that holds a scenario name, date, and docName for a
 * sceanario in a XML database.  These are used as the values in the 
 * scenario list in the DbViewer.  They have been sepearted from DbViewer
 * becuase they are now required to run a query.
 * @author Pralit Patel
 */
public class ScenarioListItem {
	/**
	 * The name of the internal xmldb document that contains this
	 * scenario.
	 */
	private final String docName;

	/**
	 * The name of the scenario.
	 */
	private final String scnName;
	/**
	 * The date of the scenario.
	 */
	private final String scnDate;

	/**
	 * Constructor to get the values of our fields.
	 */
	public ScenarioListItem(String docName, String scnName, String scnDate) {
		this.docName = docName;
		this.scnName = scnName;
		this.scnDate = scnDate;
	}

	/**
	 * Returns only the name+' '+date so that it displays with that info
	 * in a JList.  The docName should be be displayed as it is only necessary
	 * for internal use.
	 * @return A String suitable for display.
	 */ 
	public String toString() {
		// do not display docName to avoid clutter
		final String displayStr = scnName+' '+scnDate;
		return displayStr;
	}

	/**
	 * Get the internal doc name.
	 * @return The internal doc name.
	 */ 
	public String getDocName() {
		return docName;
	}

	/**
	 * Get the scenario name.
	 * @return The scneario name. 
	 */
	public String getScnName() {
		return scnName;
	}

	/**
	 * Get the scenario date.
	 * @return The scenario date.
	 */
	public String getScnDate() {
		return scnDate;
	}
}
