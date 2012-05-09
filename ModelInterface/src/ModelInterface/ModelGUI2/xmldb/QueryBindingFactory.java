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

import ModelInterface.ModelGUI2.queries.QueryGenerator;

import java.util.List;

/**
 * Gets the appropriate QueryBinding depending on what kind
 * of query is asking to be bound.
 * @author Pralit Patel
 */
public class QueryBindingFactory {
	// this does not need to be instantiated
	// maybe it should be a singleton and I store
	// the collections as that will likely not change. 
	private QueryBindingFactory() {
	}
	/**
	 * Get a query binding when a QueryGenerator is used as the basis for the query.
	 * If a run: with comment paramaters as (:scenarios:) and (:regions:) are found
	 * it will assume that the query will be bound by replacing those comments. 
	 * Otherwise it will assume that the query is just an XPath.  If an mi: is found
	 * it will assume it needs to import the ModelInterface XQuery Funtion Library.
	 * @param qg The QueryGenerator that is used to build the query.
	 * @param collection The current open collection.  Usually the database name.
	 * @return A QueryBinding that will appropriatly bind scenarios and regions.
	 */
	public static QueryBinding getQueryBinding(QueryGenerator qg, String collection) {
		QueryBinding ret;
		if(qg.isRunFunctionQuery()) {
			ret = new RunFunctionQueryBinding(qg, collection);
		} else {
			ret = new QueryGeneratorQueryBinding(qg, collection);
			// in case there was an old style node level filter, this happens with
			// copy/pasted single values as well
			// note that run functions will still work just fine with the old style
			// so don't worry about them
			if(qg.getXPath().matches(".*(?:text|node)\\(\\)\\[.*\\]$")) {
				ret = new NodeFilterDecoratorQueryBinding(ret);
			}
		}
		if(qg.getXPath().matches("mi:")) {
			ret = new ImportDecoratorQueryBinding(ret);
		}
		return ret;
	}
	/**
	 * Get a query binding which completes a base xpath.  This will return
	 * a simple QueryBinding that will prepend the appropriate path to the 
	 * base path.
	 * @param baseQuery The basis of the xpath.
	 * @param functions A list of functions to be prepended.
	 * @param collection The current open collection. Usually the database name. 
	 * @return A QueryBinding that will appropriatly bind scenarios and regions.
	 */ 
	public static QueryBinding getQueryBinding(String baseQuery, List<String> functions, String collection) {
		return new StandardQueryBinding(baseQuery, functions, collection);
	}
}
