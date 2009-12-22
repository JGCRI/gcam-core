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
