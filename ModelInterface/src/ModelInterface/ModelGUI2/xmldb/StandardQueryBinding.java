package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.ModelGUI2.DbViewer;

import java.util.List;
import java.util.Iterator;

public class StandardQueryBinding implements QueryBinding {
	private String baseQuery;
	private String collection;
	private List<String> functions;
	public StandardQueryBinding(String baseQuery, List<String> functions, String collection) {
		this.baseQuery = baseQuery;
		this.collection = collection;
		this.functions = functions;
	}
	public String bindToQuery(Object[] scenarios, Object[] regions) {
		return createQuery(createScenarioFilter(scenarios));
	}
	private String createQuery(String scnFilter) {
		StringBuilder queryBuff = new StringBuilder();
		String[] queries = baseQuery.split("\\s*\\|\\s*");
		if(functions != null) {
			for(Iterator i = functions.iterator(); i.hasNext(); ) {
				queryBuff.append(i.next()).append('(');
			}
		}
		for(String currQuery : queries) {
			queryBuff.append("collection('").append(collection).append("')");
			if(scnFilter != null) {
				queryBuff.append(scnFilter);
			}
			queryBuff.append(currQuery).append(" | ");
		}
		queryBuff.delete(queryBuff.length()-3, queryBuff.length());
		if(functions != null) {
			for(int i = 0; i < functions.size(); ++i) {
				queryBuff.append(')');
			}
		}
		return queryBuff.toString();
	}
	private String createScenarioFilter(Object[] scenarios) {
		if(scenarios == null) {
			return null;
		}
		StringBuilder ret = new StringBuilder("/");
		boolean added = false;
		for(int i = 0; i < scenarios.length; ++i) {
			DbViewer.ScenarioListItem temp = (DbViewer.ScenarioListItem)scenarios[i];
			if(!added) {
				ret.append("scenario[ ");
				added = true;
			} else {
				ret.append(" or ");
			}
			ret.append("(@name='").append(temp.getScnName()).append("' and @date='").append(temp.getScnDate()).append("')");
		}
		ret.append(" ]/world/");
		return ret.toString();
	}
}

