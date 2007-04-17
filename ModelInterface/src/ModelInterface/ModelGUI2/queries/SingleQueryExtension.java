package ModelInterface.ModelGUI2.queries;

import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;

import javax.swing.JTree;
import javax.swing.JList;

import javax.swing.tree.TreePath;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;

import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.ModelGUI2.xmldb.SingleQueryQueryBinding;
import ModelInterface.ModelGUI2.xmldb.SingleQueryListQueryBinding;
import ModelInterface.ModelGUI2.QueryTreeModel;
import ModelInterface.ModelGUI2.DbViewer;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlException;

/**
 * This class extends a Query to create, display, and execute a
 * single y-axis level of that query.  These would be useful when
 * looking to compare a single value through scenarios or regions.
 * @author Pralit Patel
 */
public class SingleQueryExtension implements TreeSelectionListener, ListSelectionListener {
	/**
	 * A List of the current node level values which could be 
	 * used to create a Single Query.
	 */
	private List<SingleQueryValue> currValues;

	/**
	 * The current scenario in which to look.
	 */
	private DbViewer.ScenarioListItem currScenario;

	/**
	 * A Map of scenario names to node level values which 
	 * acts a a cache to avoid doing queries when we already
	 * have the values.
	 */
	private Map<String, List<SingleQueryValue>> singleLevelCache;

	/**
	 * The parent query which this class extends to 
	 * create single queries.
	 */
	private QueryGenerator qg;

	/**
	 * A boolean to let us know if the list of single
	 * queries should still be displaying. Note this
	 * value could be read/written on two different
	 * threads.
	 */
	private volatile boolean isSelected;

	/**
	 * The single query value that will be displayed while 
	 * the node level values are being gathered. This 
	 * value cannot be executed.
	 */
	private final SingleQueryValue generatingLabel = 
		new SingleQueryValue("Generating List..");

	/**
	 * A list which only contains the generatingLabel
	 */
	private final List<SingleQueryValue> generatingList = 
		new ArrayList<SingleQueryValue>(1);

	/**
	 * The single query value that will be displayed if no 
	 * results for the node level could be found. This 
	 * value cannont be executed.
	 */
	private final SingleQueryValue noResults = 
		new SingleQueryValue("Could not generate list.");

	/**
	 * A list which only contains the noResults value. 
	 */
	private final List<SingleQueryValue> noResultsList = 
		new ArrayList<SingleQueryValue>(1);

	/**
	 * A simple class which holds a single query value which
	 * could potentially be executed.
	 * @author Pralit Patel
	 */
	public class SingleQueryValue {
		/**
		 * The string which represents the single
		 * query value.
		 */
		private String displayValue;

		/**
		 * Constructor which just sets the displayValue.
		 * @param displayValue The string which represents this
		 * 	single query value.
		 */
		public SingleQueryValue(String displayValue) {
			this.displayValue = displayValue;
		}

		public String toString() {
			return displayValue;
		}

		/**
		 * Determine if this value is a real node level value and
		 * could be executed.
		 * @return True if it is not the generating or no results message.
		 */
		public boolean canExecute() {
			return !(displayValue.equals("Generating List..") 
					|| displayValue.equals("Could not generate list."));
		}

		/**
		 * Return the binding which will be ready to execute.
		 * @return A query binding that will return results for the
		 * 	single value that was selected.
		 */
		public QueryBinding getAsQueryBinding() {
			return new SingleQueryQueryBinding(displayValue, qg, DbViewer.xmlDB.getContainer());
		}

		/**
		 * Gets the QueryGenerator that this class extends.
		 * @return The QueryGenerator
		 */
		public QueryGenerator getParent() {
			return qg;
		}
	}

	public SingleQueryExtension(QueryGenerator qg) {
		System.out.println("Creating Single Query for "+qg);
		this.qg = qg;
		isSelected = false;
		singleLevelCache = new HashMap<String, List<SingleQueryValue>>();
		generatingList.add(generatingLabel);
		noResultsList.add(noResults);
	}

	/**
	 * We have to be able to set the currScenario initially.
	 * @param currScenario The scenario to set.
	 */
	public void setScenario(DbViewer.ScenarioListItem currScenario) {
		this.currScenario = currScenario;
	}

	/**
	 * Determines if this SingleQueryExtension should be displaying. If 
	 * it should select it will return the path that had qg as it's 
	 * last path component or the parent of it's last path component.
	 * @param e The selection event which will determine if this should
	 * 	be showing
	 * @return Return the path with qg if it should select, null otherwise.
	 */
	private TreePath shouldSelect(TreeSelectionEvent e) {
		TreePath[] selectedPaths = ((JTree)e.getSource()).getSelectionPaths();
		if(selectedPaths == null) {
			return null;
		}
		for(TreePath path : selectedPaths) {
			Object[] pathValues = path.getPath();
			if(pathValues[pathValues.length-1] == qg 
					|| pathValues[pathValues.length-2] == qg) {
				return path;
			}
		}
		return null;
	}

	public void valueChanged(ListSelectionEvent e) {
		Object ret = ((JList)e.getSource()).getSelectedValue();
		if(ret != null) {
			// just get the first scenario
			ret = ((JList)e.getSource()).getModel().getElementAt(0);
		}
		currScenario = (DbViewer.ScenarioListItem)ret;
	}

	public void valueChanged(TreeSelectionEvent e) {
		if(!(e.getSource() instanceof JTree)) {
			// don't care about this event..
			return;
		}
		QueryTreeModel qt = (QueryTreeModel)((JTree)e.getSource()).getModel();
		boolean wasSelected = isSelected;
		TreePath parentPath = shouldSelect(e);
		if(parentPath != null && parentPath.getLastPathComponent() instanceof SingleQueryValue) {
			parentPath = parentPath.getParentPath();
		}
		isSelected = parentPath != null;
		if(!wasSelected && isSelected) {
			showList(qt, parentPath, (JTree)e.getSource());
		} else if(wasSelected && !isSelected) {
			for(TreePath path : e.getPaths()) {
				Object[] pathValues = path.getPath();
				// should is check isAddedPath ?
				if(pathValues[pathValues.length-1] == qg 
						|| pathValues[pathValues.length-2] == qg) {
					parentPath = path;
					break;
				}
			}
			if(parentPath == null) {
				System.out.println("Trouble: wasn't able to find parent in old selection");
				// about to get some null pointer action
			}
			if(parentPath.getLastPathComponent() instanceof SingleQueryValue) {
				parentPath = parentPath.getParentPath();
			}
			hideList(qt, parentPath);
		}
		// else we don't need to do anything
	}

	/**
	 * Sets the currValues to the correct list of values and
	 * tells the query tree to update accordingly.
	 * @param qt The query tree model which will show the currValues
	 * @param parentPath The path to where we will show the currValues
	 */
	private void showList(final QueryTreeModel qt, final TreePath parentPath, final JTree tree) {
		if(currScenario == null) {
			currValues = noResultsList;
		} else if(singleLevelCache.containsKey(currScenario.toString())) {
			currValues = singleLevelCache.get(currScenario.toString());
		} else {
			currValues = generatingList;
			final SingleQueryExtension thisClass = this;
			new Thread(new Runnable() {
				public void run() {
					Object[] scenarios = { currScenario };
					Object[] regions = { "Global" };
					List<SingleQueryValue> tempValues;
					try {
						XmlResults res = DbViewer.xmlDB.createQuery(new SingleQueryListQueryBinding(qg, 
								DbViewer.xmlDB.getContainer()), scenarios, regions);
						// createQuery won't pass along the XmlException so we will
						// have to check for null
						if(res == null) {
							throw new XmlException(XmlException.XPATH_PARSER_ERROR,
								"Probably invalid syntax", null, 0);
						}
						XmlValue curr;
						if(!res.hasNext()) {
							tempValues = noResultsList;
						} else {
							tempValues =  new ArrayList<SingleQueryValue>();
							while(res.hasNext()) {
								curr = res.next();
								SingleQueryValue tempValue = new SingleQueryValue(curr.asString());
								curr.delete();
								tempValues.add(tempValue);
							}
						}
						res.delete();
					} catch(XmlException e) {
						e.printStackTrace();
						tempValues = noResultsList;
					}
					singleLevelCache.put(currScenario.toString(), tempValues);
					// make sure we are still selected
					if(isSelected) {
						// get rid of generating then add real values
						hideList(qt, parentPath);
						currValues = tempValues;
						qt.showSingleQuery(thisClass, parentPath);
						tree.makeVisible(parentPath.pathByAddingChild(getSingleQueryValueAt(
										(int)(getNumValues()/2))));
					}
				}
			}).start();
		}
		qt.showSingleQuery(this, parentPath);
		tree.makeVisible(parentPath.pathByAddingChild(getSingleQueryValueAt(
						(int)(getNumValues()/2))));
	}

	/**
	 * Tells the query tree model to stop displaying these
	 * single query values.
	 * @param qt The query tree model to update.
	 * @param parentPath The path which will need to collapse it's children.
	 */
	private void hideList(QueryTreeModel qt, TreePath parentPath) {
		qt.hideSingleQuery(this, parentPath);
	}

	/**
	 * So that we can know when to reset currValues.
	 */
	public void resetList() {
		currValues = null;
	}

	/**
	 * Get the number of single query values that currently 
	 * should be displayed.
	 * @return The number of values to display.
	 */
	public int getNumValues() {
		if(currValues == null) {
			return 0;
		} else {
			return currValues.size();
		}
	}

	/**
	 * Get the single query value at the given index.
	 * @param pos The index to get the query for.
	 * @return The requested Single query value.
	 */
	public SingleQueryValue getSingleQueryValueAt(int pos) {
		// could check if currValues is null, but that would
		// be a NullPointerException anyway
		return currValues.get(pos);
	}

	/**
	 * Get the index for the SingleQueryValue in currList.
	 * @param value The value we are looking for.
	 * @return The index in currValues.
	 */
       public int getIndexOfValue(SingleQueryValue value) {
	       return currValues.indexOf(value);
       }
}
