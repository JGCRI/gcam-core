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

import ModelInterface.ModelGUI2.queries.QueryGenerator;

import java.util.List;
import java.util.Iterator;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class SingleQueryListQueryBinding implements QueryBinding {
	/* The XQuery Function more readable:
	declare function local:get-singlequery-name($results as node()*, $collapseList as xs:string*) as xs:string* {
		for $result in $results
			return fn:substring(fn:string-join(
			(local:get-full-singlequery-name($result/parent::node()/parent::node(), $collapseList), $result), '/'), 2)
	};
	
	declare function local:get-full-singlequery-name($curr as node(), $collapseList as xs:string*) as xs:string {
		if($curr and local-name($curr) != 'scenario')
		then
			let $type := $curr/@type,
			    $ret := local:get-full-singlequery-name($curr/parent::node(), $collapseList)
			return
				if($type and fn:empty(fn:index-of($collapseList, $type)))
				then fn:string-join(($ret, fn:concat($type,': ', $curr/@name)), '/')
				else $ret
		else ''
	};
	*/
	private static final String getFullNameXQueryFunction = " declare function local:get-singlequery-name($results as node()*, $collapseList as xs:string*) as xs:string* { for $result in $results return fn:substring(fn:string-join((local:get-full-singlequery-name($result/parent::node()/parent::node(), $collapseList), $result), '/'), 2) }; declare function local:get-full-singlequery-name($curr as node(), $collapseList as xs:string*) as xs:string { if($curr and local-name($curr) != 'scenario') then let $type := $curr/@type, $ret := local:get-full-singlequery-name($curr/parent::node(), $collapseList) return if($type and fn:empty(fn:index-of($collapseList, $type))) then fn:string-join(($ret, fn:concat($type,': ', $curr/@name)), '/') else $ret else '' }; ";

	private QueryGenerator qg;
	private String collection;
	private String collapseList;
	public SingleQueryListQueryBinding(QueryGenerator qg, String collection, List<String> collapseList) {
		this.qg = qg;
		this.collection = collection;
		// need to convert this List into an XQuery list
		StringBuilder buff = new StringBuilder("('region',");
		for(Iterator<String> it = collapseList.iterator(); it.hasNext(); ) {
			buff.append("'").append(it.next()).append("',");
		}
		this.collapseList = buff.replace(buff.length()-1, buff.length(), ")").toString();
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		// I will have to add the functions myself so that I get the correct syntax
		// need to determine if we need to do a run function
		Pattern pat = Pattern.compile("(?m)^\\s*(local:run.*)\\s*$");
		Matcher matches = pat.matcher(qg.getXPath());
		StringBuilder query = new StringBuilder(getFullNameXQueryFunction); 
		if(!matches.find()) {
			// regular format query
			query.append("fn:distinct-values(local:get-singlequery-name(");
			query.append(getNodeLevelFilterQuery(QueryBindingFactory.getQueryBinding(qg.getCompleteXPath(regions), null, collection)
					.bindToQuery(scenarios, regions), qg.getNodeLevelPath()));
			query.append(", ").append(collapseList).append("))");
		} else {
			try {
				// find the run function call and replace it with a wrapped call with our get-singlequery-name
				// call and append the NodeLevelPath as well
				RunFunctionQueryBinding runBinding = new RunFunctionQueryBinding(qg, collection);
				matches = pat.matcher(runBinding.bindToQuery(scenarios, regions));
				matches.find();
				String theCall = matches.group();
				query.append(matches.replaceFirst(Matcher.quoteReplacement("fn:distinct-values(local:get-singlequery-name("+
							getNodeLevelFilterQuery(theCall, qg.getNodeLevelPath())+", "+collapseList+"))")));
			} catch(Exception e) {
				e.printStackTrace();
				// return something that will not run so they know SOMETHING went wrong
				return "failure";
			}
		}
		return query.toString();
	}
	/**
	 * Gets the appropriate query to get node level values.  This could be as simple
	 * as concatinating the base query with the nodel level path however there seems
	 * to be issues with the way this gets optimized in BDBXML 2.4.  So will try
	 * wrapping this in a simple FLWR expression like:
	 * for $n in *baseQuery*
	 * return $n/*nodeLevelPath*
	 * @param baseQuery The base portion of the query
	 * @param nodeLevelPath The generated query portion which will give us the node level path.
	 * @return A query which will get you the node level values in a reasonable way.
	 */
	private String getNodeLevelFilterQuery(String baseQuery, String nodeLevelPath) {
		StringBuilder query = new StringBuilder("for $n in "); 
		query.append(baseQuery).append(" return $n/").append(nodeLevelPath);
		return query.toString();
	}
}
