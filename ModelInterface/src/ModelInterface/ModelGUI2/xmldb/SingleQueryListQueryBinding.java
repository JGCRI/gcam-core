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
			query.append(QueryBindingFactory.getQueryBinding(qg.getCompleteXPath(regions)+
						qg.getNodeLevelPath(), null, collection)
					.bindToQuery(scenarios, regions));
			query.append(", ").append(collapseList).append("))");
		} else {
			try {
				// find the run function call and replace it with a wrapped call with our get-singlequery-name
				// call and append the NodeLevelPath as well
				RunFunctionQueryBinding runBinding = new RunFunctionQueryBinding(qg, collection);
				matches = pat.matcher(runBinding.bindToQuery(scenarios, regions));
				matches.find();
				String theCall = matches.group();
				query.append(matches.replaceFirst("fn:distinct-values(local:get-singlequery-name("+theCall+
							qg.getNodeLevelPath()+", "+collapseList+"))"));
			} catch(Exception e) {
				e.printStackTrace();
				// return something that will not run so they know SOMETHING went wrong
				return "failure";
			}
		}
		return query.toString();
	}
}
