package ModelInterface.ModelGUI2.queries;

import javax.swing.JButton;
import javax.swing.JLabel;
import com.sleepycat.dbxml.XmlValue;
import java.util.Map;
import java.util.Vector;
import java.util.EventListener;
import java.util.List;

public abstract class QueryBuilder implements java.io.Serializable {

	/* Query Fragments that will be used when generating queries.  We need to be able to 
	 * have some method for enabaling subtypes of for example sectors. Currently done by
	 * an attribute with type = the type that is subclassed from
	 */
	public static final String regionQueryPortion = "*[@type = 'region']";
	public static final String sectorQueryPortion = "*[@type = 'sector']";
	public static final String subsectorQueryPortion = "*[@type = 'subsector']";
	public static final String technologyQueryPortion = "*[@type = 'technology']";
	public static final String resourceQueryPortion = "*[@type = 'resource']";
	public static final String subresourceQueryPortion = "*[@type = 'subresource']";
	public static final String baseTechnologyQueryPortion = "*[@type = 'baseTechnology']";

	protected transient QueryGenerator qg;
	protected String queryFilter;
	protected Vector<String> queryFunctions;
	boolean isGlobal;
	protected QueryBuilder(QueryGenerator qgIn) {
		qg = qgIn;
		queryFilter = "";
		queryFunctions = new Vector<String>();
	}
	public abstract EventListener getListSelectionListener(final JComponentAdapter list, 
			final JButton nextButton, final JButton cancelButton);
	public abstract void doFinish(JComponentAdapter list);
	public abstract JComponentAdapter doBack(JComponentAdapter list, JLabel label);
	public abstract JComponentAdapter doNext(JComponentAdapter list, JLabel label);
	// TODO: isAtEnd is no longer used, remove it
	public abstract boolean isAtEnd();
	public abstract JComponentAdapter updateList(JComponentAdapter list, JLabel label);
	public abstract void updateSelected(JComponentAdapter list);
	public abstract String createListPath(int level);
	public abstract String getCompleteXPath(Object[] regions);
  	public abstract Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception; 
	public abstract Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception; 
	public abstract String getXMLName();

	public String getNodeLevelPath() {
		return qg.defaultGetNodeLevelPath();
	}
	public String getForNodeLevelPath(String nodeLevelValue) {
		return qg.defaultGetForNodeLevelPath(nodeLevelValue);
	}
	// this gets reset when copy/pasted
	void setQueryGenerator(QueryGenerator qgIn) {
		qg = qgIn;
	}
	public abstract List<String> getDefaultCollpaseList();
}
