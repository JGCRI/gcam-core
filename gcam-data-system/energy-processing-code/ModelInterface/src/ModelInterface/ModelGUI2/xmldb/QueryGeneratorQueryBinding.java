package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

public class QueryGeneratorQueryBinding implements QueryBinding {
	private QueryGenerator qg;
	private String collection;
	public QueryGeneratorQueryBinding(QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		return QueryBindingFactory.getQueryBinding(qg.getCompleteXPath(regions), null, collection)
			.bindToQuery(scenarios, regions);
	}
}
