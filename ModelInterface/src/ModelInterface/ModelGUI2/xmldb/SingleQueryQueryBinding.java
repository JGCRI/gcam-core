package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.queries.QueryGenerator;

public class SingleQueryQueryBinding implements QueryBinding {
	private QueryGenerator qg;
	private String collection;
	private String nodeLevelValue;
	public SingleQueryQueryBinding(String value, QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
		this.nodeLevelValue = value;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		return QueryBindingFactory.getQueryBinding(qg, collection).bindToQuery(scenarios, regions)+
			qg.getForNodeLevelPath(nodeLevelValue);
	}
}
