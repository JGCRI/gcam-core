package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.ScenarioListItem;
import ModelInterface.ModelGUI2.queries.QueryGenerator;

/**
 * This query binding is used when the query has a run function whose 
 * parameters should be a list of scenarios, regions, and a collection.
 * It will the replace the comment paramaters with real lists.
 * @author Pralit Patel.
 */ 
public class RunFunctionQueryBinding implements QueryBinding {
	QueryGenerator qg;
	String collection;
	public RunFunctionQueryBinding(QueryGenerator qg, String collection) {
		this.qg = qg;
		this.collection = collection;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		StringBuilder scenarioListBuilder = new StringBuilder("(");
		StringBuilder regionListBuilder = new StringBuilder("(");
		for(int i = 0; i < scenarios.length; ++i) {
			ScenarioListItem currScn = (ScenarioListItem)scenarios[i];
			scenarioListBuilder.append("'").append(currScn.getScnName()).append(" ")
				.append(currScn.getScnDate()).append("', ");
		}
		scenarioListBuilder.delete(scenarioListBuilder.length()-2, scenarioListBuilder.length()).append(")");
		for(int i = 0; i < regions.length; ++i) {
			String currRegion = (String)regions[i];
			if(currRegion.equals("Global")) {
				qg.setGlobal(true);
			}
			regionListBuilder.append("'").append(currRegion).append("', ");
		}
		regionListBuilder.delete(regionListBuilder.length()-2, regionListBuilder.length()).append(")");
		String ret = qg.getXPath().replace("(:scenarios:)", scenarioListBuilder.toString());
		ret = ret.replace("(:regions:)", regionListBuilder.toString());
		ret = ret.replace("(:collection:)", "'"+collection+"'");
		return ret;
	}
}
