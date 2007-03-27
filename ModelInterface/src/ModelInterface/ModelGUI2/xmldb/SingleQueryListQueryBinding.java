package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

import java.util.List;
import java.util.ArrayList;

public class SingleQueryListQueryBinding implements QueryBinding {
	private QueryGenerator qg;
	private String collection;
	public SingleQueryListQueryBinding(QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		List<String> functions = new ArrayList<String>(1);
		functions.add("distinct-values");
		return QueryBindingFactory.getQueryBinding(qg.getCompleteXPath(regions)+
				qg.getNodeLevelPath(), functions, collection)
			.bindToQuery(scenarios, regions);
	}
}
