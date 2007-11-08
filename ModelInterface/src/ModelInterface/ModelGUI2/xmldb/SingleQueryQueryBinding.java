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
		return QueryBindingFactory.getQueryBinding(qg, collection).bindToQuery(scenarios, regions)+
			qg.getForNodeLevelPath(nodeLevelValues);
	}
}
