package ModelInterface.ModelGUI2.queries;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import com.sleepycat.dbxml.XmlValue;
import java.util.Map;
import java.util.Vector;

public abstract class QueryBuilder {

	/* Query Fragments that will be used when generating queries.  We need to be able to 
	 * have some method for enabaling subtypes of for example sectors. Currently done by
	 * an attribute with type = the type that is subclassed from
	 */
	public static String regionQueryPortion = "*[@type = 'region']";
	public static String sectorQueryPortion = "*[@type = 'sector']";
	public static String subsectorQueryPortion = "*[@type = 'subsector']";
	public static String technologyQueryPortion = "*[@type = 'technology']";
	public static String resourceQueryPortion = "*[@type = 'resource']";
	public static String subresourceQueryPortion = "*[@type = 'subresource']";

	protected QueryGenerator qg;
	protected String queryFilter;
	protected Vector<String> queryFunctions;
	protected QueryBuilder(QueryGenerator qgIn) {
		qg = qgIn;
		queryFilter = "";
		queryFunctions = new Vector<String>();
	}
	public abstract ListSelectionListener getListSelectionListener(final JComponentAdapter list, 
			final JButton nextButton, final JButton cancelButton);
	public abstract void doFinish(JComponentAdapter list);
	public abstract void doBack(JComponentAdapter list, JLabel label);
	public abstract void doNext(JComponentAdapter list, JLabel label);
	public abstract boolean isAtEnd();
	public abstract void updateList(JComponentAdapter list, JLabel label);
	public abstract void updateSelected(JComponentAdapter list);
	public abstract String createListPath(int level);
	public abstract String getCompleteXPath(Object[] regions);
  	public abstract Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception; 
	public abstract Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception; 
	public abstract String getXMLName();
}
