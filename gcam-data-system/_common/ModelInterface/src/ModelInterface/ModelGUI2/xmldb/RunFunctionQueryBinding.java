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
package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.ScenarioListItem;
import ModelInterface.ModelGUI2.queries.QueryGenerator;

/**
 * This query binding is used when the query has a run function whose 
 * parameters should be a list of scenarios, regions, and a collection.
 * It will the replace the comment paramaters with real lists.
 * @author Pralit Patel.
 */ 
public class RunFunctionQueryBinding implements QueryBinding {
	QueryGenerator qg;
	String collection;
	public RunFunctionQueryBinding(QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		StringBuilder scenarioListBuilder = new StringBuilder("(");
		StringBuilder regionListBuilder = new StringBuilder("(");
		for(int i = 0; i < scenarios.length; ++i) {
			ScenarioListItem currScn = (ScenarioListItem)scenarios[i];
			scenarioListBuilder.append("'").append(currScn.getScnName()).append(" ")
				.append(currScn.getScnDate()).append("', ");
		}
		scenarioListBuilder.delete(scenarioListBuilder.length()-2, scenarioListBuilder.length()).append(")");
		for(int i = 0; i < regions.length; ++i) {
			String currRegion = (String)regions[i];
			regionListBuilder.append("'").append(currRegion).append("', ");
		}
		regionListBuilder.delete(regionListBuilder.length()-2, regionListBuilder.length()).append(")");
		String ret = qg.getXPath().replace("(:scenarios:)", scenarioListBuilder.toString());
		ret = ret.replace("(:regions:)", regionListBuilder.toString());
		ret = ret.replace("(:collection:)", "'"+collection+"'");
		return ret;
	}
}
