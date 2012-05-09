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
package ModelInterface.ModelGUI2.queries;

import ModelInterface.common.DataPair;

import javax.swing.JButton;
import javax.swing.JLabel;
import com.sleepycat.dbxml.XmlValue;
import java.util.Map;
import java.util.Vector;
import java.util.EventListener;
import java.util.List;

public abstract class QueryBuilder implements java.io.Serializable {

	/* Query Fragments that will be used when generating queries.  We need to be able to 
	 * have some method for enabaling subtypes of for example sectors. Currently done by
	 * an attribute with type = the type that is subclassed from
	 */
	public static final String regionQueryPortion = "*[@type = 'region']";
	public static final String sectorQueryPortion = "*[@type = 'sector']";
	public static final String subsectorQueryPortion = "*[@type = 'subsector']";
	public static final String technologyQueryPortion = "*[@type = 'technology']";
	public static final String resourceQueryPortion = "*[@type = 'resource']";
	public static final String subresourceQueryPortion = "*[@type = 'subresource']";
	public static final String baseTechnologyQueryPortion = "*[@type = 'baseTechnology']";

	protected transient QueryGenerator qg;
	protected String queryFilter;
	protected Vector<String> queryFunctions;
	protected QueryBuilder(QueryGenerator qgIn) {
		qg = qgIn;
		queryFilter = "";
		queryFunctions = new Vector<String>();
	}
	public abstract EventListener getListSelectionListener(final JComponentAdapter list, 
			final JButton nextButton, final JButton cancelButton);
	public abstract void doFinish(JComponentAdapter list);
	public abstract JComponentAdapter doBack(JComponentAdapter list, JLabel label);
	public abstract JComponentAdapter doNext(JComponentAdapter list, JLabel label);
	// TODO: isAtEnd is no longer used, remove it
	public abstract boolean isAtEnd();
	public abstract JComponentAdapter updateList(JComponentAdapter list, JLabel label);
	public abstract void updateSelected(JComponentAdapter list);
	public abstract String createListPath(int level);
	public abstract String getCompleteXPath(Object[] regions);
	public abstract String getXMLName();

	public String getNodeLevelPath() {
		return qg.defaultGetNodeLevelPath();
	}
	public String getForNodeLevelPath(List<String> nodeLevelValue) {
		return qg.defaultGetForNodeLevelPath(nodeLevelValue);
	}
	// this gets reset when copy/pasted
	void setQueryGenerator(QueryGenerator qgIn) {
		qg = qgIn;
	}
	public abstract List<String> getDefaultCollpaseList();
	public Map addToDataTree(XmlValue currNode, Map dataTree, DataPair<String, String> axisValue, boolean isGlobal) throws Exception {
		return qg.defaultAddToDataTree(currNode, dataTree, axisValue, isGlobal);
	}
}
