package ModelInterface.ModelGUI2.queries;

import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;

import javax.swing.JTree;
import javax.swing.JList;

import javax.swing.tree.TreePath;

import javax.swing.event.UndoableEditEvent;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;

import ModelInterface.common.DataPair;
import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.ModelGUI2.xmldb.SingleQueryQueryBinding;
import ModelInterface.ModelGUI2.xmldb.SingleQueryListQueryBinding;
import ModelInterface.ModelGUI2.xmldb.QueryBindingFactory;
import ModelInterface.ModelGUI2.QueryTreeModel;
import ModelInterface.ModelGUI2.DbViewer;
import ModelInterface.ModelGUI2.undo.MiUndoableEditListener;
import ModelInterface.ModelGUI2.undo.EditQueryUndoableEdit;

import com.sleepycat.dbxml.XmlResults;
import com.sleepycat.dbxml.XmlValue;
import com.sleepycat.dbxml.XmlDocument;
import com.sleepycat.dbxml.XmlException;

/**
 * This class extends a Query to create, display, and execute a
 * single y-axis level of that query.  These would be useful when
 * looking to compare a single value through scenarios or regions.
 * @author Pralit Patel
 */
public class SingleQueryExtension implements TreeSelectionListener, ListSelectionListener, MiUndoableEditListener {
	/**
	 * A List of the current node level values which could be 
	 * used to create a Single Query.
	 */
	private List<SingleQueryValue> currValues;

	/**
	 * The current scenario/regions in which to look.
	 */
	private DataPair<List<DbViewer.ScenarioListItem>, List<String>> currSelection;

	/**
	 * A Map of scenario/region names to node level values which 
	 * acts a a cache to avoid doing queries when we already
	 * have the values.
	 */
	private Map<DataPair<List<DbViewer.ScenarioListItem>, List<String>>, List<SingleQueryValue>> singleLevelCache;

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
	 * If this Single Query Extension should show a list
	 * or not.
	 */
	private boolean isEnabled;

	/**
	 * Could be run at anytime to disable this single
	 * query extension.
	 */
	private Runnable doDisable = null;

	/**
	 * Could be run at anytime to enable this single
	 * query extension.
	 */
	private Runnable doEnable = null;
	
	/**
	 * The thread that is set when gathering the single
	 * query list.  This thread will be interrupted before
	 * a new one is allowed to be set.
	 */
	private Thread gatherThread = null;

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
			if(!displayValue.equals("Total")) {
				return new SingleQueryQueryBinding(displayValue, qg, DbViewer.xmlDB.getContainer());
			} else {
				return QueryBindingFactory.getQueryBinding(qg, DbViewer.xmlDB.getContainer());
			}
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
		this.qg = qg;
		isSelected = false;
		singleLevelCache = 
			new HashMap<DataPair<List<DbViewer.ScenarioListItem>, List<String>>, List<SingleQueryValue>>();
		generatingList.add(generatingLabel);
		noResultsList.add(noResults);
		currSelection = null;
		isEnabled = true;
	}

	/**
	 * We have to be able to set the currSelection initially.
	 * @param currScenario The scenarios to set.
	 * @param currRegion The regions to set.
	 */
	public void setSelection(DbViewer.ScenarioListItem[] currScenario, String[] currRegion) {
		if(currSelection == null) {
			currSelection = new DataPair<List<DbViewer.ScenarioListItem>, List<String>>();
		}
		if(currScenario.length != 0) {
			currSelection.setKey(Arrays.asList(currScenario));
		} else {
			currSelection.setKey(null);
		}
		if(currRegion.length != 0) {
			currSelection.setValue(Arrays.asList(currRegion));
		} else {
			currSelection.setValue(null);
		}
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
			// check the selected value or its parent to see if it is
			// this extension's query generator
			if(pathValues.length > 1 && (pathValues[pathValues.length-1] == qg 
					|| pathValues[pathValues.length-2] == qg)) {
				return path;
			}
		}
		return null;
	}

	/**
	 * Listen for selection changes in a list.  The lists that we 
	 * care about are the scenario list and region list.  When 
	 * they change we need to update the currSelection and force
	 * a refresh of the list.
	 * @param e The event that represents the list selection change.
	 */
	public void valueChanged(ListSelectionEvent e) {
		/** We are not listening for these if we are just going to
		 * pull values from cache
		if(!isEnabled) {
			return;
		}
		// we want to process the event only when the user is done
		// making changes.
		if(e.getValueIsAdjusting()) {
			return;
		}

		// get the selected values, if there are none just return
		Object[] ret = ((JList)e.getSource()).getSelectedValues();
		if(ret == null) {
			return;
		}

		// figure out which list the event came from, we only care about the
		// scneario and region lists.
		if(((JList)e.getSource()).getName().equals(DbViewer.SCENARIO_LIST_NAME)) {
			DbViewer.ScenarioListItem[] selScn = new DbViewer.ScenarioListItem[ret.length];
			System.arraycopy(ret, 0, selScn, 0, ret.length);
			if(selScn.length != 0) {
				List<DbViewer.ScenarioListItem> oldList = currSelection.setKey(Arrays.asList(selScn));
			} else {
				currSelection.setKey(null);
			}
		} else if(((JList)e.getSource()).getName().equals(DbViewer.REGION_LIST_NAME)) {
			String[] selRegions = new String[ret.length];
			System.arraycopy(ret, 0, selRegions, 0, ret.length);
			if(selRegions.length != 0) {
				List<String> oldList = currSelection.setValue(Arrays.asList(selRegions));
			} else {
				currSelection.setValue(null);
			}
		}

		// only want to force the update if it is already showing
		// otherwise changing scenarios or regions could be horribly
		// slow.
		if(isSelected && isEnabled) {
			doDisable.run();
			doEnable.run();
		}
		*/
	}

	/**
	 * Listen for tree selection changes.  We only care about the
	 * changes for the query tree.  If this extension's query
	 * generator was selected and this extension was not showing
	 * we need to show the list.  If this extension was showing and
	 * the selection no longer contains the parent query generator or
	 * a single item from this list we need to hide our list.
	 * @param e The event that represents the tree selection.
	 */
	public void valueChanged(TreeSelectionEvent e) {
		if(!isEnabled) {
			return;
		}
		if(!(e.getSource() instanceof JTree)) {
			// don't care about this event..
			return;
		}
		final JTree tree = (JTree)e.getSource();
		final QueryTreeModel qt = (QueryTreeModel)tree.getModel();
		boolean wasSelected = isSelected;
		TreePath parentPath = shouldSelect(e);
		if(parentPath != null && parentPath.getLastPathComponent() instanceof SingleQueryValue) {
			parentPath = parentPath.getParentPath();
		}
		isSelected = parentPath != null;
		// wasn't selected before but is now so we need to show
		if(!wasSelected && isSelected) {
			// the doDisable/Enable need to be initialized
			// the first time we show.
			if(doDisable == null) {
				final TreePath parentPathF = parentPath;
				doDisable = new Runnable() {
					public void run() {
						isSelected = false;
						hideList(qt, parentPathF);
						resetList();
					}
				};
				doEnable = new Runnable() {
					public void run() {
						isSelected = true;
						showList(qt, parentPathF, tree);
					}
				};
			}

			showList(qt, parentPath, tree);
		} else if(wasSelected && !isSelected) {
			// was selected before but not anymore so hide
			for(TreePath path : e.getPaths()) {
				Object[] pathValues = path.getPath();
				// should is check isAddedPath ?
				if(pathValues.length > 1 && (pathValues[pathValues.length-1] == qg 
						|| pathValues[pathValues.length-2] == qg)) {
					parentPath = path;
					break;
				}
			}
			if(parentPath == null) {
				System.out.println("Trouble: wasn't able to find parent in old selection");
				// about to get some null pointer action so lets just give doDisable a try, 
				// if that has not been set we have no other choices
				doDisable.run();
				return;
			}
			if(parentPath.getLastPathComponent() instanceof SingleQueryValue) {
				parentPath = parentPath.getParentPath();
			}
			hideList(qt, parentPath);
		}
		// else we don't need to do anything
	}

	/**
	 * Listen for an undo event and if it is for and edit
	 * query undo and that query changes it's xpath or
	 * nodel level we need to reset the cache.
	 * @param e The event that represents the undo.
	 */
	public void undoPerformed(UndoableEditEvent e) {
		if(!(e.getSource() instanceof EditQueryUndoableEdit)) {
			// not intrested..
			return;
		}
		EditQueryUndoableEdit ed = (EditQueryUndoableEdit)e.getSource();
		if(ed.didXPathChange() || ed.didNodeLevelChange()) {
			resetCache();
		}
	}

	/**
	 * Listen for an redo event and if it is for and edit
	 * query and that query changes it's xpath or
	 * nodel level we need to reset the cache.
	 * @param e The event that represents the redo.
	 */
	public void redoPerformed(UndoableEditEvent e) {
		if(!(e.getSource() instanceof EditQueryUndoableEdit)) {
			// not intrested..
			return;
		}
		EditQueryUndoableEdit ed = (EditQueryUndoableEdit)e.getSource();
		if(ed.didXPathChange() || ed.didNodeLevelChange()) {
			resetCache();
		}
	}

	/**
	 * Sets the currValues to the correct list of values and
	 * tells the query tree to update accordingly.
	 * @param qt The query tree model which will show the currValues
	 * @param parentPath The path to where we will show the currValues
	 */
	private void showList(final QueryTreeModel qt, final TreePath parentPath, final JTree tree) {
		// let the gather thread know it needs to stop if it still active
		setGatherThread(null);

		// if we don't have a selection of scenario or region we can't
		// have a list
		if(currSelection == null || currSelection.getKey() == null ||
				currSelection.getValue() == null) {
			currValues = noResultsList;
		} else if(singleLevelCache.containsKey(currSelection)) {
			// we already have the list in the cache so use it
			currValues = singleLevelCache.get(currSelection);
		} else {
			// we need to query to get the list, in the mean time
			// we will set the list to a generating message which
			// will be updated with a real list when we finish 
			// querying
			currValues = generatingList;

			// make sure we only have one thread gathering at a time
			// so we don't kill the system when a user is just trying
			// to set their scenarios and regions
			//setGatherThread(new Thread(getRunnableCreateListQuery(qt, parentPath, tree)));
			setGatherThread(new Thread(getRunnableGetListQuery(qt, parentPath, tree)));
			gatherThread.start();
		}

		// make sure that the gather thread did not beat us to the punch
		// and already update the list with real values
		if(gatherThread == null || gatherThread.isAlive()) {
			qt.showSingleQuery(this, parentPath);
			tree.makeVisible(parentPath.pathByAddingChild(getSingleQueryValueAt(
							(int)(getNumValues()/2))));
		}
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
	 * Sets the gatherThread to the new thread.  If gatherThread
	 * was not null and still alive it must be interrupted first
	 * before proceeding.
	 * @param newThread The new thread to set.
	 */
       private synchronized void setGatherThread(Thread newThread) {
	       if(gatherThread != null && gatherThread.isAlive()) {
		       gatherThread.interrupt();
		       // should I join?
	       }
	       gatherThread = newThread;
       }

       /**
	* Reset the cache and force a refresh if we
	* are still visable.
	*/
       private void resetCache() {
	       singleLevelCache.clear();
	       if(isSelected && isEnabled) {
		       doDisable.run();
		       doEnable.run();
	       }
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

       /**
	* Set if this Single Query Extension should
	* be enabled or not.
	* @param enable If this extension will be enabled.
	*/
       public void setEnabled(boolean enable) {
	       if(isEnabled && !enable && doDisable != null) {
		       isEnabled = false;
		       // don't bother with the thread?
		       doDisable.run();
	       } else if(!isEnabled && enable && doEnable != null) {
		       isEnabled = true;
		       doEnable.run();
	       }
	       // else nothing has really changed.
	       isEnabled = enable;
       }
       /**
	* Gets a runnable that executes a query to create the single query list.
	* @pre currSelection must be properly set.
	* @param qt QueryTreeModel needed to update list.
	* @param parentPath The path needed to update list.
	* @param tree The tree which would be updated.
	* @return a Runnable which can be executed to create the single query list.
	*/
       private Runnable getRunnableCreateListQuery(final QueryTreeModel qt, final TreePath parentPath, final JTree tree) {
	       final SingleQueryExtension thisClass = this;
	       return (new Runnable() {
		       public void run() {
			       Object[] scenarios = currSelection.getKey().toArray();
			       Object[] regions = currSelection.getValue().toArray();
			       List<SingleQueryValue> tempValues;
			       final long startTime = System.currentTimeMillis();
			       try {
				       XmlResults res = DbViewer.xmlDB.createQuery(new SingleQueryListQueryBinding(qg, 
						       DbViewer.xmlDB.getContainer(), qg.getCollapseOnList()), scenarios, regions);
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
					       if(qg.isGroup() && qt != null) {
						       tempValues.add(new SingleQueryValue("Total"));
					       }
				       }
				       res.delete();
			       } catch(XmlException e) {
				       e.printStackTrace();
				       tempValues = noResultsList;
			       }
			       System.out.println("Time : "+(System.currentTimeMillis()-startTime));
			       singleLevelCache.put(currSelection, tempValues);

			       boolean wasInterrupted = Thread.interrupted();

			       // make sure we are still selected
			       if(isSelected && !wasInterrupted) {
				       // if qt is null we are just going to cache this so just
				       // set the currValues
				       if(qt == null) {
					       currValues = tempValues;
				       } else {
					       // get rid of generating then add real values
					       hideList(qt, parentPath);
					       currValues = tempValues;
					       qt.showSingleQuery(thisClass, parentPath);
					       tree.makeVisible(parentPath.pathByAddingChild(getSingleQueryValueAt(
									       (int)(getNumValues()/2))));
				       }
			       }
		       }
	       });
       }

       /**
	* Gets a runnable that executes a query to get the single query list from metadata.
	* @pre currSelection must be properly set.
	* @param qt QueryTreeModel needed to update list.
	* @param parentPath The path needed to update list.
	* @param tree The tree which would be updated.
	* @return a Runnable which can be executed to get the single query list.
	*/
       private Runnable getRunnableGetListQuery(final QueryTreeModel qt, final TreePath parentPath, final JTree tree) {
	       final SingleQueryExtension thisClass = this;
	       return (new Runnable() {
		       public void run() {
			       List<SingleQueryValue> tempValues = null;
			       final long startTime = System.currentTimeMillis();
			       try {
				       // I am putting a Q in from of the hash code because otherwise it complains that
				       // it is not a valid QName
				       XmlResults res = DbViewer.xmlDB.createQuery("/singleQueryListCache/dbxml:metadata('Q"
					       +qg.getStorageHashCode()+"')", null, null, null);
				       boolean hasResults = false;
				       if(res.hasNext()) {
					       XmlValue curr = res.next();
					       String[] values = curr.asString().split(";");
					       curr.delete();
					       if(!(values.length == 1 && values[0].equals(""))) {
						       hasResults = true;
						       tempValues =  new ArrayList<SingleQueryValue>(values.length+1);
						       for(String val : values) {
							       SingleQueryValue tempValue = new SingleQueryValue(val);
							       tempValues.add(tempValue);
						       }
						       if(qg.isGroup()) {
							       tempValues.add(new SingleQueryValue("Total"));
						       }
					       }
				       }
				       if(!hasResults) {
					       tempValues = noResultsList;
				       }
				       res.delete();
			       } catch(XmlException e) {
				       e.printStackTrace();
				       tempValues = noResultsList;
			       }
			       System.out.println("Time : "+(System.currentTimeMillis()-startTime));
			       singleLevelCache.put(currSelection, tempValues);

			       // make sure we are still selected
			       if(isSelected && !Thread.currentThread().isInterrupted()) {
				       // get rid of generating then add real values
				       hideList(qt, parentPath);
				       currValues = tempValues;
				       qt.showSingleQuery(thisClass, parentPath);
				       tree.makeVisible(parentPath.pathByAddingChild(getSingleQueryValueAt(
								       (int)(getNumValues()/2))));
			       }
		       }
	       });
       }

       /**
	* Set up and run whatever is needed to cache the query list.
	* @doc The cahce document that we will set the metadata on.
	* @param scenarions The list of scenarios to scan.
	* @param regions The list of regions to scan.
	*/
       public void createSingleQueryListCache(XmlDocument doc, DbViewer.ScenarioListItem[] currScenario, String[] currRegion) {
	       // set the scenarios and regions to scan
	       setSelection(currScenario, currRegion);
	       boolean isSelectedBefore = isSelected;
	       isSelected = true;

	       try {
		       // set the gather thread and run it, but we don't really need it on another thread
		       // becuase we have to wait for it anyways.
		       setGatherThread(new Thread(getRunnableCreateListQuery(null, null, null)));
		       gatherThread.start();
		       gatherThread.join();

		       // don't want to cache the no results
		       if(currValues != noResultsList) {
			       StringBuffer buff = new StringBuffer();
			       for(Iterator<SingleQueryValue> it = currValues.iterator(); it.hasNext(); ) {
				       buff.append(it.next().toString()).append(";");
			       }
			       System.out.println("About to cache Q"+qg.getStorageHashCode()+" -> "+buff.toString());
			       // I have to put a letter in front of the hash code because otherwise
			       // it says it is not a valid QName
			       doc.setMetaData("", "Q"+qg.getStorageHashCode(), new XmlValue(buff.toString()));
			       //DbViewer.xmlDB.setMetaData(doc, "Q"+qg.getStorageHashCode(), new XmlValue(buff.toString()));
		       }
	       } catch(XmlException e) {
		       // TODO: should I warn the user?
		       e.printStackTrace();
	       } catch(InterruptedException ie) {
		       // I need to interrupt myself again because getting this exception already
		       // cleared the status and the scanThread needs to know not to try to scan
		       // anymore queries.  Also let the gather thread know to stop
		       Thread.currentThread().interrupt();
		       gatherThread.interrupt();
	       }

	       // reset stuff
	       singleLevelCache.clear();
	       resetList();
	       isSelected = isSelectedBefore;
	       // do I need to reset the selection?
       }

       /**
	* Determine if the this extension has been initialized.  It is
	* initialized the first time the QueryGenerator is clicked on. It
	* is possible to create this extension and not initialize it when 
	* it is used to create the cache.
	* @return True if it has been initialized else false.
	*/
       public boolean isInitialized() {
	       // maybe I should use a more explicit way of knowing if this
	       // has been initialized
	       return doDisable != null;
       }
}
