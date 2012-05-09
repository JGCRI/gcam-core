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
package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.ScenarioListItem;

import java.util.List;
import java.util.Iterator;

public class StandardQueryBinding implements QueryBinding {
	private String baseQuery;
	private String collection;
	private List<String> functions;
	public StandardQueryBinding(String baseQuery, List<String> functions, String collection) {
		this.baseQuery = baseQuery;
		this.collection = collection;
		this.functions = functions;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		return createQuery(createScenarioFilter(scenarios));
	}
	private String createQuery(String scnFilter) {
		StringBuilder queryBuff = new StringBuilder();
		String[] queries = baseQuery.split("\\s*\\|\\s*");
		if(functions != null) {
			for(Iterator i = functions.iterator(); i.hasNext(); ) {
				queryBuff.append(i.next()).append('(');
			}
		}
		for(String currQuery : queries) {
			queryBuff.append("collection('").append(collection).append("')");
			if(scnFilter != null) {
				queryBuff.append(scnFilter);
			}
			queryBuff.append(currQuery).append(" | ");
		}
		queryBuff.delete(queryBuff.length()-3, queryBuff.length());
		if(functions != null) {
			for(int i = 0; i < functions.size(); ++i) {
				queryBuff.append(')');
			}
		}
		return queryBuff.toString();
	}
	private String createScenarioFilter(Object[] scenarios) {
		if(scenarios == null) {
			return null;
		}
		StringBuilder ret = new StringBuilder("/");
		boolean added = false;
		for(int i = 0; i < scenarios.length; ++i) {
			ScenarioListItem temp = (ScenarioListItem)scenarios[i];
			if(!added) {
				ret.append("scenario[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(temp.getScnName()).append("' and @date='").append(temp.getScnDate()).append("')");
		}
		ret.append(" ]/world/");
		return ret.toString();
	}
}

