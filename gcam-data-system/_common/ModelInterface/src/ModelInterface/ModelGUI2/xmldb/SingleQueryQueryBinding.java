package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

import java.util.List;

public class SingleQueryQueryBinding implements QueryBinding {
	private QueryGenerator qg;
	private String collection;
	private List<String> nodeLevelValues;
	public SingleQueryQueryBinding(List<String> values, QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
		this.nodeLevelValues = values;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		String baseQuery = QueryBindingFactory.getQueryBinding(qg, collection).bindToQuery(scenarios, regions);
		String nodeLevelFilter = qg.getForNodeLevelPath(nodeLevelValues);
		// run funtion style queries will work fine with just appending the node level filter
		// but will not be correct syntax wise when using getNodeLevelFilteredQuery
		return qg.isRunFunctionQuery() ? baseQuery + nodeLevelFilter :
			getNodeLevelFilteredQuery(baseQuery, nodeLevelFilter);
	}
	/**
	 * Gets the appropriate query to get filtered node level values.  This could be
	 * as simple as concatinating the base query with the nodel level filter path 
	 * however there seems to be issues with the way this gets optimized in BDBXML 2.4.
	 * So will try wrapping this in a simple FLWR expression like:
	 * for $n in *baseQuery*
	 * return $n*nodeLevelFilter*
	 * @param baseQuery The base portion of the query
	 * @param nodeLevelFilter The generated query portion which will give filtered the results.
	 * @return A query which will get you the filtered filtered values in a reasonable way.
	 */
	private String getNodeLevelFilteredQuery(String baseQuery, String nodeLevelFilter) {
		StringBuilder query = new StringBuilder("for $n in "); 
		query.append(baseQuery).append(" return $n").append(nodeLevelFilter);
		return query.toString();
	}
}
