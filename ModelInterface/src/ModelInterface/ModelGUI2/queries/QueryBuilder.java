package ModelInterface.ModelGUI2.queries;

import javax.swing.JList;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.event.ListSelectionListener;
import com.sleepycat.dbxml.XmlValue;
import java.util.Map;
import java.util.Vector;

public abstract class QueryBuilder {
	protected QueryGenerator qg;
	protected String queryFilter;
	protected Vector<String> queryFunctions;
	protected QueryBuilder(QueryGenerator qgIn) {
		qg = qgIn;
		queryFilter = "";
		queryFunctions = new Vector<String>();
	}
	public abstract ListSelectionListener getListSelectionListener(final JList list, final JButton nextButton, final JButton cancelButton);
	public abstract void doFinish(JList list);
	public abstract void doBack(JList list, JLabel label);
	public abstract void doNext(JList list, JLabel label);
	public abstract boolean isAtEnd();
	public abstract void updateList(JList list, JLabel label);
	public abstract void updateSelected(JList list);
	public abstract String createListPath(int level);
	public abstract String getCompleteXPath(Object[] regions);
  	public abstract Object[] extractAxisInfo(XmlValue n, Map filterMaps) throws Exception; 
	public abstract Map addToDataTree(XmlValue currNode, Map dataTree) throws Exception; 
	public abstract String getXMLName();
}
