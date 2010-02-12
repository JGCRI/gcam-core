package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.common.LRUCacheMap;
import ModelInterface.ModelGUI2.queries.*;
import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.ModelGUI2.xmldb.QueryBindingFactory;

import java.io.*;
import java.util.*;

import javax.swing.*;
import java.awt.*;

import com.sleepycat.db.*;
import com.sleepycat.dbxml.*;

public class XMLDB {
	public static final boolean lockCheck = false;
	Environment dbEnv;
	XmlManager manager;
	XmlContainer myContainer;
	XmlUpdateContext uc;
	String contName;
	private volatile int numVals;
	private static XMLDB xmldbInstance = null;

	/**
	 * Gets the instance of the xml database.
	 * @warning If the database is not open it will return null, ideally it
	 * 	would throw an exception.
	 * @return The instance of the xml database.
	 */
	public static XMLDB getInstance() {
		// TODO: not thread safe
		if(xmldbInstance == null) {
			// should throw an exception..
			System.out.println("The database is not open.");
		}
		return xmldbInstance;
	}
	/**
	 * Opens a new xml database at the given location.
	 * @param dbLocation The location of the database to open.
	 * @param parentFrame The frame that can be used to display errors on.
	 * @throws Exception If a database is already open or there was an error opening the database.
	 */ 
	public static void openDatabase(String dbLocation, JFrame parentFrame) throws Exception {
		// TODO: not thread safe
		if(xmldbInstance != null) {
			throw new Exception("Could not open databse because "+xmldbInstance.contName+
					" is still open");
		}
		xmldbInstance = new XMLDB(dbLocation, parentFrame);
	}

	/**
	 * Closes the database. Note that all errors on close are ignored.
	 */
	public static void closeDatabase() {
		// TODO: not thread safe
		if(xmldbInstance != null) {
			try {
				xmldbInstance.closeDB();
			} catch(XmlException e) {
				e.printStackTrace();
			} finally {
				xmldbInstance = null;
			}
		}
	}

	private XMLDB(String db, JFrame parentFrame) throws Exception {
		openDB(db, parentFrame);
	}
	private void openDB(String dbPath, JFrame parentFrame) throws Exception {
		EnvironmentConfig envConfig = new EnvironmentConfig();
		envConfig.setAllowCreate(true);
		envConfig.setCacheSize(100 * 1024 * 1024 );
		envConfig.setInitializeCache(true);
		envConfig.setInitializeLocking(true);
		//envConfig.setVerboseDeadlock(true);
		//envConfig.setVerboseWaitsFor(true);
		//envConfig.setNoLocking(true);

		numVals = 0;
		//XmlManager.setLogCategory(XmlManager.CATEGORY_ALL, true);
		//XmlManager.setLogLevel(XmlManager.LEVEL_ALL, true);
		String path = dbPath.substring(0, dbPath.lastIndexOf(System.getProperty("file.separator")));
		boolean didUpgradeEnv = false;
		try {
			dbEnv = new Environment(new File(path), envConfig);
		} catch(VersionMismatchException vme) {
			int ans = JOptionPane.showConfirmDialog(parentFrame, "The version of the selected database does not match the version\nof the database library. Do you want to attempt to upgrade?\n\nWarning: Upgrading could cause loss of data, it is recomended\nthat you backup your database first.", "DB Version Mismatch Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
			if(ans == JOptionPane.YES_OPTION) {
				System.out.println("Remove Env");
				Environment.remove(new File(path), true, envConfig);
				dbEnv = new Environment(new File(path), envConfig);
				System.out.println("Done Remove env");
				didUpgradeEnv = true;
			} else {
				throw vme;
			}
		}
		LockStats ls = dbEnv.getLockStats(StatsConfig.DEFAULT);
		System.out.println("Current Locks: "+ls.getNumLocks());
		System.out.println("Current Lockers: "+ls.getNumLockers());
		System.out.println("Current Deadlocks: "+ls.getNumDeadlocks());
		//System.out.println("Current Conflicts: "+ls.getNumConflicts());
		if(ls.getNumLocks() > 0) {
			Object[] opts = {"Fix", "Continue"};
			JOptionPane lockErrMess = new JOptionPane(
					"The current Database is locked.  This could be because\nanother application is using it.  You may be able to continue\nwithout writing to it.  If there are no other applications using\nit you can try to reset the locking subsystem.",
					JOptionPane.ERROR_MESSAGE, JOptionPane.YES_NO_OPTION, null, opts);
			lockErrMess.createDialog(parentFrame, "DB Locking Error").setVisible(true);
			//System.out.println(lockErrMess.getValue());
			if(lockErrMess.getValue().equals("Fix")) {
				dbEnv.close();
				System.out.println("Locking problem, attempting to reset locking subsystem");
				Environment.remove(new File(dbPath.substring(0, dbPath.lastIndexOf(System.getProperty("file.separator")))), true, envConfig);
				/*
				   boolean didDel = new File(path+System.getProperty("file.separator")+"__db.001").delete();
				   didDel = didDel && new File(path+System.getProperty("file.separator")+"__db.002").delete();
				   didDel = didDel && new File(path+System.getProperty("file.separator")+"__db.003").delete();
				   */
				dbEnv = new Environment(new File(path), envConfig);
				ls = dbEnv.getLockStats(StatsConfig.DEFAULT);
				System.out.println("Current Locks: "+ls.getNumLocks());
				System.out.println("Current Lockers: "+ls.getNumLockers());
				System.out.println("Current Deadlocks: "+ls.getNumDeadlocks());
				//System.out.println("Current Conflicts: "+ls.getNumConflicts());
			}
		}
		XmlManagerConfig mc = new XmlManagerConfig();
		mc.setAdoptEnvironment(true);
		manager = new XmlManager(dbEnv, mc);
		XmlContainerConfig cconfig = new XmlContainerConfig();
		cconfig.setAllowCreate(true);
		contName = dbPath.substring(dbPath.lastIndexOf(System.getProperty("file.separator"))+1);
		uc = manager.createUpdateContext();
		try {
			myContainer = manager.openContainer(contName, cconfig);
		} catch(XmlException ve) {
			if(ve.getErrorCode() == XmlException.VERSION_MISMATCH) {
				int ans;
				if(didUpgradeEnv) {
					ans = JOptionPane.YES_NO_OPTION;
				} else {
					ans = JOptionPane.showConfirmDialog(parentFrame, "The version of the selected database does not match the version\nof the database library. Do you want to attempt to upgrade?\n\nWarning: Upgrading could cause loss of data, it is recomended\nthat you backup your database first.", "DB Version Mismatch Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				}
				if(ans == JOptionPane.YES_OPTION) {
					System.out.println("Do upgrade");
					parentFrame.getGlassPane().setVisible(true);
					// TODO: getting this call to work would be better than the current solution.
					boolean didUpgradeWork = true;
					try {
					manager.upgradeContainer(contName, uc);
					} catch(XmlException updE) {
						updE.printStackTrace();
						didUpgradeWork = false;
					}
					//boolean didUpgradeWork = upgradeDatabase(path, contName);
					parentFrame.getGlassPane().setVisible(false);
					if(!didUpgradeWork) {
						JOptionPane.showMessageDialog(parentFrame, 
								"An error occured while upgrading the database.", 
								"DB Upgrade Error", JOptionPane.ERROR_MESSAGE);
						throw new Exception("Could not upgrade database.");
					}
					System.out.println("Done upgrade");
					myContainer = manager.openContainer(contName, cconfig);
					System.out.println("Done open");
				} else {
					throw ve;
				}
			} else {
				throw ve;
			}
		}
		// TODO: auto indexing leads to a ton of indexes most of which would not be utilized so
		// turn it off by default for now util farther evaulations
		myContainer.setAutoIndexing(false);
		printLockStats("openDB");
		// will just get any that has already been cached, users will have to 
		// tell use the gui search manually since they will rarely be used now
		getVarMetaData(); 
	}
	public void printLockStats(String where) {
		if(!lockCheck) {
			return;
		}
		//System.out.println("At: "+where+" with numVals: "+numVals);
		try {
			System.out.println("At: "+where);
			LockStats ls = manager.getEnvironment().getLockStats(StatsConfig.DEFAULT);
			System.out.println("Current Locks: "+ls.getNumLocks());
			System.out.println("Current Lockers: "+ls.getNumLockers());
			System.out.println("Current Deadlocks: "+ls.getNumDeadlocks());
			//System.out.println("Current Conflicts: "+ls.getNumConflicts());
			System.out.println("Locks Requested: "+ls.getNumRequests());
			System.out.println("Locks Released: "+ls.getNumReleases());
		} catch (XmlException e) {
			e.printStackTrace();
		} catch (DatabaseException dbe) {
			dbe.printStackTrace();
		}
	}
	public void addFile(String fileName) {
	    XmlDocumentConfig docConfig = new XmlDocumentConfig();
	    docConfig.setGenerateName(true);
	    try {
		    myContainer.putDocument("run", manager.createLocalFileInputStream(fileName), uc, docConfig);
		    printLockStats("addFile");
		    //myContainer.sync();
	    } catch(XmlException e) {
		    e.printStackTrace();
	    }
	}
	public void removeDoc(String docName) {
		try {
			System.out.println("Removing :"+docName);
			myContainer.deleteDocument(docName, uc);
			printLockStats("removeDoc");
		} catch(XmlException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Export a document to a text file.
	 * @param aDocName Name of the document to export.
	 * @param aLocation Location at which to save the document.
	 * @return Whether the scenario was saved successfully.
	 */
	public boolean exportDoc(final String aDocName, final File aLocation) {
		try {
			XmlDocument doc = myContainer.getDocument(aDocName);
			OutputStream fileOutput = new BufferedOutputStream(new FileOutputStream(aLocation));
			fileOutput.write(doc.getContent());
			doc.delete();
			fileOutput.close();
			return true;
		}
		catch(XmlException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	public XmlDocument getDocument(String docName) throws XmlException {
		return myContainer.getDocument(docName);
	}

	public XmlDocument createDocument(String docName, String content) {
		XmlDocumentConfig docConfig = new XmlDocumentConfig();
		docConfig.setGenerateName(false);
		try {
			myContainer.putDocument("cache", content, uc, docConfig);
			return getDocument(docName);
		} catch(XmlException e) {
			e.printStackTrace();
			return null;
		}
	}

	public void updateDocument(XmlDocument doc) {
		try {
			//uc.setApplyChangesToContainers(true);
			myContainer.updateDocument(doc, uc);
			//uc.setApplyChangesToContainers(false);
		} catch(XmlException e) {
			e.printStackTrace();
			// TODO: warn the users
		}
	}

	public String getContainer() {
		return contName;
	}

	public XmlResults createQuery(String query, Vector<String> queryFunctions, 
			Object[] scenarios, Object[] regions) {
		return createQuery(QueryBindingFactory.getQueryBinding(query, queryFunctions, contName),
				scenarios, regions);
	}
	public XmlResults createQuery(String query, Vector<String> queryFunctions, 
			Object[] scenarios, Object[] regions, XmlQueryContext context) throws XmlException {
		return createQuery(QueryBindingFactory.getQueryBinding(query, queryFunctions, contName),
				scenarios, regions, context);
	}

	public XmlResults createQuery(QueryGenerator qg, Object[] scenarios, Object[] regions) {
		return createQuery(QueryBindingFactory.getQueryBinding(qg, contName), scenarios,
				regions);
	}
	public XmlResults createQuery(QueryGenerator qg, Object[] scenarios, Object[] regions, XmlQueryContext context) throws XmlException {
		return createQuery(QueryBindingFactory.getQueryBinding(qg, contName), scenarios,
				regions,context);
	}
	
	public XmlQueryContext createQueryContext(){
		try {
			return manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
		} catch (XmlException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}
	
	public XmlResults createQuery(QueryBinding queryBinding, Object[] scenarios, Object[] regions, XmlQueryContext context) throws XmlException {
		String queryComplete = queryBinding.bindToQuery(scenarios, regions);
		System.out.println("About to perform query: "+queryComplete);
		if(context == null){
			return createQuery(queryBinding, scenarios, regions);
		}
		//XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
		/*
		   XmlQueryExpression qeTemp = manager.prepare(queryBuff.toString(), qc);
		   System.out.println("Query Plan: "+qeTemp.getQueryPlan());
		   return qeTemp.execute(qc);
		   */
		/*
		   System.out.println("Number of values are currently: "+numVals);
		   return new XmlResultsWrapper(manager.query(queryComplete, qc));
		   */
		return manager.query(queryComplete, context);
	}
	
	public XmlResults createQuery(QueryBinding queryBinding, Object[] scenarios, Object[] regions) {
		String queryComplete = queryBinding.bindToQuery(scenarios, regions);
		System.out.println("About to perform query: "+queryComplete);
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
			/* Use when you need to see the query plan
			XmlQueryExpression qeTemp = manager.prepare(queryComplete, qc);
			System.out.println("Query Plan: "+qeTemp.getQueryPlan());
			return qeTemp.execute(qc);
			*/
			/* Use to debug non-deleted Xml References
			System.out.println("Number of values are currently: "+numVals);
			return new XmlResultsWrapper(manager.query(queryComplete, qc));
			*/
			return manager.query(queryComplete, qc);
		} catch(XmlException e) {
			e.printStackTrace();
			return null;
		}
	}

	private void closeDB() throws XmlException {
		System.out.println("Closing DB");
		printLockStats("closeDB");
		if(myContainer == null) {
			// didn't open sucessfully
			return;
		}
		System.out.println("Number of values are currently: "+numVals);
		System.out.println("Hits: "+hits+", misses: "+misses);
		myContainer.close();
		manager.close();
	}

	public static boolean hasAttr(XmlValue val) throws XmlException {
		XmlResults attrRes = val.getAttributes();
		boolean ret = attrRes.hasNext();
		attrRes.delete();
		if(!ret) {
			//printLockStats("hasNext"); should be lock safe, now can make it static
			return ret;
		} else {
			// have to make sure the values getAllAttr ignores
			// are ignored here as well
			if(getAllAttr(val).equals("")) {
				return false;
			} else {
				return true;
			}
		}
	}

	public static String getAttr(XmlValue node) throws XmlException {
		XmlResults attrRes = node.getAttributes();
		XmlValue temp;
		while(attrRes.hasNext()) {
			temp = attrRes.next();
			if(!temp.getNodeName().equals("fillout")) {
				//String ret = temp.getNodeName()+"="+temp.getNodeValue();
				String ret = temp.getNodeValue();
				temp.delete();
				attrRes.delete();
				//printLockStats("getAttr(XmlValue)1"); should be lock safe, now can make it static
				return ret;
			}
			temp.delete();
		}
		attrRes.delete();
		//printLockStats("getAttr(XmlValue)2"); should be lock safe, now can make it static 
		return null;
	}

	public static String getAttr(XmlValue node, String attrKey) throws XmlException {
		XmlResults attrRes = node.getAttributes();
		XmlValue temp = null;
		while(attrRes.hasNext()) {
			temp = attrRes.next();
			if(temp.getNodeName().equals(attrKey)) {
				String ret = temp.getNodeValue();
				//temp.delete();
				attrRes.delete();
				//printLockStats("getAttr(XmlValue, String)1"); should be lock safe, now can make it static
				return ret;
				//return temp.getNodeValue();
			}
			temp.delete();
		}
		//temp.delete();
		attrRes.delete();
		//printLockStats("getAttr(XmlValue, String)2"); should be lock safe, now can make it static
		return null;
	}
	private static Map<String, Map<String, String>> attrCache = new LRUCacheMap<String, Map<String, String>>(500);
	//private static Map<String, Map<String, String>> attrCache = new TreeMap<String, Map<String, String>>();
	private static int misses = 0;
	private static int hits = 0;
	public static Map<String, String> getAttrMap(XmlValue node) throws XmlException {
		XmlResults attrRes = node.getAttributes();
		XmlValue temp;
		Map<String, String> ret = new TreeMap<String, String>();
		while(attrRes.hasNext()) {
			temp = attrRes.next();
			ret.put(temp.getNodeName(), temp.getNodeValue());
			temp.delete();
		}
		attrRes.delete();
		return ret;
	}
	public static Map<String, String> getAttrMapWithCache(XmlValue node) throws XmlException {
		String nodeId = node.getNodeHandle();
		Map<String, String> ret = attrCache.get(nodeId);
		++hits;
		// null means a cahce miss so we will need to go back to the database
		if(ret == null) {
			++misses;
			--hits;
			ret = getAttrMap(node);
			// put the result in the cache for the next time
			attrCache.put(nodeId, ret);
		}
		return ret;
	}
	public static String getAllAttr(Map<String, String> attrMap) {
		String ret;
		if((ret = attrMap.get("name")) != null) {
			ret = "," + ret;
		} else {
			ret = "";
		}
		for(Iterator<Map.Entry<String, String>> it = attrMap.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry<String, String> currAttr = it.next();
			if(currAttr.getKey().indexOf(":") == -1 && !currAttr.getKey().equals("name") &&
					!currAttr.getKey().equals("type") && 
					!currAttr.getKey().equals("unit") && 
					!currAttr.getKey().equals("year")) {
				ret += ","+currAttr.getKey()+"="+currAttr.getValue();
			}
		}
		return !ret.equals("") ? ret.substring(1) : ret;
	}
	public static String getAllAttr(XmlValue node) throws XmlException {
		XmlResults attrRes = node.getAttributes();
		XmlValue temp;
		String ret;
		if((ret = getAttr(node, "name")) != null) {
			ret = "," + ret;
		} else {
			ret = "";
		}
		while(attrRes.hasNext()) {
			temp = attrRes.next();
			if(temp.getNodeName().indexOf(":") == -1 && !temp.getNodeName().equals("name") &&
					!temp.getNodeName().equals("type") && 
					!temp.getNodeName().equals("unit") && 
					!temp.getNodeName().equals("year")) {
				ret += ","+temp.getNodeName()+"="+temp.getNodeValue();
			}
			temp.delete();
		}
		attrRes.delete();
		//printLockStats("getAllAtr"); should be lock safe, now can make it static
		if(!ret.equals("")) {
			return ret.substring(1);
		} else {
			return ret;
		}
	}
	public String getQueryFunctionAsDistinctNames() {
		/* Does not work anymore since BDBXML 2.3
		return "declare function local:distinct-node-names ($arg as node()*, $before_a as xs:string*) as xs:string* {    for $a at $apos in $arg  let $b := fn:local-name($a), $c_before := fn:count($before_a) + $apos - $apos, $before_a := fn:distinct-values(fn:insert-before($before_a, 0, $b))  where not(fn:count($before_a) = $c_before)  return $b }; local:distinct-node-names";
		*/
		return "declare function local:distinct-node-names ($args as node()*) as xs:string* { fn:distinct-values(for $nname in $args return fn:local-name($nname)) }; local:distinct-node-names";
	}
	/**
	 * Sets the value of the given node to the passed in string content back into the
	 * database.  WARNING: the given val must an Eager result otherwise there will be
	 * a deadlock when the query executes.
	 * @param val The node for which to set the value.
	 * @param content The new value to set.
	 */
	public void setValue(XmlValue val, String content) {
		// simple XQuery update query to replace the value of the given val
		final String setValueXQuery = "replace value of node self::node() with $newValue";
		XmlQueryContext qc = null;
		XmlQueryExpression qe = null;
		XmlResults res = null;
		try {
			qc = manager.createQueryContext();
			// setting the new value through the variable is safer and allows us to use
			// the same query
			qc.setVariableValue("newValue", new XmlValue(content));
			qe = manager.prepare(setValueXQuery, qc);
			// not expecting anything to be in the results but we should still get it
			// so that we can make sure it gets deleted
			res = qe.execute(val, qc);
		} catch(XmlException e) {
			e.printStackTrace();
		} finally {
			if(res != null) {
				res.delete();
			}
			if(qe != null) {
				qe.delete();
			}
			if(qc != null) {
				qc.delete();
			}
			printLockStats("XMLDB.setValue");
		}
	}
	public void addVarMetaData(final Frame parentFrame) {
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Eager);
			printLockStats("Before check md query");
			final XmlResults res = manager.query(
					"collection('"+contName+"')/*[fn:empty(dbxml:metadata('var'))]", qc);
			printLockStats("after check md query");
			//uc.setApplyChangesToContainers(true);
			// instead of having to determine the size of prog bar, should be figured out
			// by the number of query builders..
			final JProgressBar progBar = new JProgressBar(0, res.size()*7);
			final JDialog jd = createProgressBarGUI(parentFrame, progBar, "Getting Variables", "Finding new variables");
			(new Thread(new Runnable() {
				public void run() {
					Runnable incProgress = (new Runnable() {
						public void run() {
							progBar.setValue(progBar.getValue() + 1);
						}
					});
					try {
					XmlResults tempRes;
					XmlResults getNodeRes;
					XmlValue tempVal;
					XmlValue delVal;
					//XmlDocument docTemp; 
					java.util.List<String> getVarFromDoucmentNames = new ArrayList<String>();
					boolean gotVars = false;
					while(res.hasNext()) {
						gotVars = true;
						tempVal = res.next();
						//docTemp = tempVal.asDocument();
						getVarFromDoucmentNames.add(tempVal.getNodeHandle());
						//docTemp.delete();
						tempVal.delete();
					}
					res.delete();
					for(Iterator<String> it = getVarFromDoucmentNames.iterator(); it.hasNext(); ) {
						getNodeRes = myContainer.getNode(it.next());
						tempVal = getNodeRes.next();
						System.out.println("Getting new MetaData");
						String path = "local:distinct-node-names(/scenario/world/*[@type='region']/demographics//*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						StringBuffer strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
							delVal.delete();
							strBuff.append(';');
						}
						printLockStats("after first var look-up");
						XmlDocument docTemp = tempVal.asDocument();
						printLockStats("after get document");
						docTemp.setMetaData("", "demographicsVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);
						printLockStats("After done adding demoVar");

						path = "local:distinct-node-names(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							//System.out.println("adding");
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
							delVal.delete();
							strBuff.append(';');
						}
						tempRes.delete();
						docTemp.setMetaData("", "var", new XmlValue(strBuff.toString()));
						SwingUtilities.invokeLater(incProgress);

						XmlQueryContext qcL = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
						XmlQueryExpression qe = manager.prepare("distinct-values(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[@type='GHG']/@name)", qcL);
						tempRes = qe.execute(tempVal, qcL);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString()).append(';');
							delVal.delete();
						}
						docTemp.setMetaData("", "ghgNames", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						qe = manager.prepare("distinct-values(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[@type='GHG']/emissions/@fuel-name)", qcL);
						tempRes = qe.execute(tempVal, qcL);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString()).append(';');
							delVal.delete();
						}
						docTemp.setMetaData("", "fuelNames", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/climate-model/*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
							delVal.delete();
							strBuff.append(';');
						}
						docTemp.setMetaData("", "ClimateVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(//LandLeaf/*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
							delVal.delete();
							strBuff.append(';');
						}
						docTemp.setMetaData("", "LandAllocationVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/*[@type='region']/GDP/*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
							delVal.delete();
							strBuff.append(';');
						}
						docTemp.setMetaData("", "GDPVar", new XmlValue(strBuff.toString()));
						myContainer.updateDocument(docTemp, uc);
						docTemp.delete();
						tempVal.delete();
						tempRes.delete();
						getNodeRes.delete();
						SwingUtilities.invokeLater(incProgress);
					}
					res.delete();
					printLockStats("addVarMetaData1");
					getVarMetaData();
					if(jd != null) {
						SwingUtilities.invokeLater(new Runnable() {
							public void run(){
								jd.setVisible(false);
							}
						});
					}
					String message = gotVars ? "Finished getting new variables." : "There were no new variables.";
					JOptionPane.showMessageDialog(parentFrame, message, "Get Variables", 
							JOptionPane.INFORMATION_MESSAGE);
					} catch(XmlException e) {
						e.printStackTrace();
					}
				}
			})).start();
		} catch(XmlException e) {
			e.printStackTrace();
			//closeDB();
		}
		printLockStats("addVarMetaData2");
	}

	protected XmlResults getVars(XmlValue contextVal, String path) {
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);

			/* The xQuery more readable:
			* declare function local:distinct-node-names ($arg as node()*, $before_a as xs:string*) as xs:string* { 
			*	for $a at $apos in $arg
			*	let $b := fn:local-name($a),
			*	    $c_before := fn:count($before_a) + $apos - $apos,
			*	    $before_a := fn:distinct-values(fn:insert-before($before_a, 0, $b))
			*	where not(fn:count($before_a) = $c_before)
			*	return $b
			* };
			* local:distinct-node-names(/scenario/world/region/supplysector/subsector/technology/*[fn:count(child::text()) = 1], ())
			*/
			/* The above seems to be broken with the new XQuery lib in dbxml 2.3
			 * so here goes another shot..
			 * declare function local:distinct-node-names ($args as node()*) as xs:string* { 
			 *	fn:distinct-values(for $nname in $args return fn:local-name($nname))
			 * }; 
			 * local:distinct-node-names(/scenario/world/region/supplysector/subsector/technology/*[fn:count(child::text()) = 1])
			 */ 
			/* Does not work since BDBXML 2.3
			String queryStr = "declare function local:distinct-node-names ($arg as node()*, $before_a as xs:string*) as xs:string* {    for $a at $apos in $arg  let $b := fn:local-name($a), $c_before := fn:count($before_a) + $apos - $apos, $before_a := fn:distinct-values(fn:insert-before($before_a, 0, $b))  where not(fn:count($before_a) = $c_before)  return $b }; "+path;
			*/
			String queryStr = "declare function local:distinct-node-names ($args as node()*) as xs:string* { fn:distinct-values(for $nname in $args return fn:local-name($nname)) }; "+path;
			XmlQueryExpression qe = manager.prepare(queryStr, qc);
			return qe.execute(contextVal, qc);
		} catch(XmlException e) {
			e.printStackTrace();
			//closeDB();
			return null;
		}
	}
	protected void getVarMetaData() {
		XmlResults res = createQuery("/*[fn:exists(dbxml:metadata('var'))]", null, null, null);
		// TODO: is is safe to assume if one is null they are all null?
		if(SupplyDemandQueryBuilder.varList == null) {
			SupplyDemandQueryBuilder.varList = new LinkedHashMap();
			DemographicsQueryBuilder.varList = new LinkedHashMap();
			EmissionsQueryBuilder.ghgList = new LinkedHashMap();
			EmissionsQueryBuilder.fuelList = new LinkedHashMap();
			GDPQueryBuilder.varList = new LinkedHashMap();
			ClimateQueryBuilder.varList = new LinkedHashMap();
			LandAllocatorQueryBuilder.varList = new LinkedHashMap();
		}
		XmlMetaData md;
		try {
			while(res.hasNext()) {
				XmlValue vt = res.next();
				XmlDocument docTemp = vt.asDocument();
				System.out.println("Gathering metadata from a doc "+docTemp.getName());
				XmlMetaDataIterator it = docTemp.getMetaDataIterator();
				while((md = it.next()) != null) {
					XmlValue mdVal = md.get_value();
					if(mdVal.isNull()) {
						System.out.println("Got null metadata value for "+md.get_name());
					} else if(md.get_name().equals("var")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							SupplyDemandQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("demographicsVar")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							DemographicsQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("ghgNames")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							EmissionsQueryBuilder.ghgList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("fuelNames")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							EmissionsQueryBuilder.fuelList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("GDPVar")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							GDPQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("ClimateVar")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							ClimateQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("LandAllocationVar")) {
						String[] vars = mdVal.asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							LandAllocatorQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					}
					mdVal.delete();
					//md.delete();
				}
				//it.delete();
				docTemp.delete();
				vt.delete();
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		res.delete();
		// maybe this should be somewhere else..
		// maybe summable list should be cached..
		Vector funcTemp = new Vector<String>(1,0);
		funcTemp.add("distinct-values");
		res = createQuery("/scenario/output-meta-data/summable/@var", funcTemp, null, null);
		QueryGenerator.sumableList = new Vector();
		QueryGenerator.hasYearList = new Vector<String>();
		XmlValue delValue;
		try {
			while(res.hasNext()) {
				delValue = res.next();
				QueryGenerator.sumableList.add(delValue.asString());
				delValue.delete();
			}
			res.delete();
			res = createQuery("/scenario/output-meta-data/has-year/@var", funcTemp, null, null);
			while(res.hasNext()) {
				delValue = res.next();
				QueryGenerator.hasYearList.add(delValue.asString());
				delValue.delete();
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		printLockStats("getVarMetaData");
	}
	/**
	 * Trys to upgrade the database at the passed in location.  This will
	 * rely on the db_upgrade database utility to be located in the same directory
	 * as the jar.
	 * @param path The path to the directory of where the database is.
	 * @param name The file name of the database.
	 * @return True if it worked false otherwise.
	 */
	private static boolean upgradeDatabase(String path, String name) {
		// do I need to worry about escaping the path?
		final String command = System.getProperty("ModelInterface.ModelGUI2.dbxml.location")+" -h "+path;
		//System.out.println("Command is: "+command);
		final String[] env = new String[1];
		final String upgradeCommand = "upgradeContainer "+name;
		final String exitCommand = "exit";
		env[0] = "DYLD_LIBRARY_PATH="+System.getProperty("java.library.path");
		//System.out.println("env is: "+env[0]);
		boolean ret = false;
		try {
			final Process proc = Runtime.getRuntime().exec(command, env);
			java.io.BufferedWriter writer = new java.io.BufferedWriter(new java.io.OutputStreamWriter(proc.getOutputStream()));
			writer.write(upgradeCommand, 0, upgradeCommand.length());
			writer.newLine();
			writer.flush();
			writer.write(exitCommand, 0, exitCommand.length());
			writer.newLine();
			writer.flush();
			/*
			new Thread(new Runnable() {
				public void run() {
					for(int i = 0; i < 5; ++i) {
						try {
							System.out.println("Doing stuff");
					java.io.BufferedReader buff = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getErrorStream()));
					String read;
					while(buff.ready() && (read = buff.readLine()) != null) {
						System.out.println(read);
					}
					buff.close();
					buff = new java.io.BufferedReader(new java.io.InputStreamReader(proc.getInputStream()));
					while(buff.ready() && (read = buff.readLine()) != null) {
						System.out.println(read);
					}
					buff.close();
					System.out.println("Before sleep");
					Thread.sleep(10000);
					System.out.println("After sleep");
						} catch(Exception e) {
							e.printStackTrace();
						}
					}
					System.out.println("Before destroy");
					proc.destroy();
					System.out.println("After destroy");
				}
			}).start();
			*/

			ret = proc.waitFor() == 0;
			//System.out.println("return value: "+proc.exitValue());
		} catch(Exception e) {
			e.printStackTrace();
		}
		return ret;
	}
	// TODO: this is a util method and should be moved somewhere else
	public static JDialog createProgressBarGUI(Frame parentFrame, JProgressBar progBar, String title, String labelStr) {
		if(progBar.getMaximum() == 0) {
			return null;
		}
		JDialog filterDialog = new JDialog(parentFrame, title, false);
		filterDialog.setAlwaysOnTop(true);
		JPanel all = new JPanel();
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		progBar.setPreferredSize(new Dimension(200, 20));
		JLabel label = new JLabel(labelStr);
		Container contentPane = filterDialog.getContentPane();
		all.add(label, BorderLayout.PAGE_START);
		all.add(Box.createVerticalStrut(10));
		all.add(progBar);
		all.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		contentPane.add(all, BorderLayout.PAGE_START);
		filterDialog.pack();
		filterDialog.setVisible(true);
		return filterDialog;
	}
	// the following classes can be used in order to figure out if any 
	// references managed to avoid manual deletion which could lead to
	// references still alive at db close time which is bad. Use in
	// conjunction with createQuery so it will create the wrapped
	// version and the wrapped version will also create wrapped versions
	public class XmlResultsWrapper extends XmlResults {
		private boolean didDelete = false;
		public XmlResultsWrapper(XmlResults in) throws XmlException {
			// it should copy it right? If so I can delete it.
			super(in);
			++numVals;
			in.delete();
		}
		public XmlValue next() throws XmlException {
			return new XmlValueWrapper(super.next());
		}
		public void delete() {
			// only allow deleting one time to avoid double counting
			if(!didDelete) {
				--numVals;
				super.delete();
				didDelete = true;
			}
		}
		public void finalize() {
			// do nothing so I know I don't have to rely on gc
		}
	}
	public class XmlValueWrapper extends XmlValue {
		private boolean didDelete = false;
		public XmlValueWrapper(XmlValue in) throws XmlException {
			// again copies right?
			super(in);
			++numVals;
			in.delete();
		}
		public void delete() {
			// only allow deleting one time to avoid double counting
			if(!didDelete) {
				--numVals;
				super.delete();
				didDelete = true;
			}
		}
		public XmlResults getAttributes() throws XmlException {
			return new XmlResultsWrapper(super.getAttributes());
		}
		public XmlValue getFirstChild() throws XmlException {
			return new XmlValueWrapper(super.getFirstChild());
		}
		public XmlValue getLastChild() throws XmlException {
			return new XmlValueWrapper(super.getLastChild());
		}
		public XmlValue getNextSibling() throws XmlException {
			return new XmlValueWrapper(super.getNextSibling());
		}
		public XmlValue getOwnerElement() throws XmlException {
			return new XmlValueWrapper(super.getOwnerElement());
		}
		public XmlValue getParentNode() throws XmlException {
			return new XmlValueWrapper(super.getParentNode());
		}
		public XmlValue getPreviousSibling() throws XmlException {
			return new XmlValueWrapper(super.getPreviousSibling());
		}
		public void finalize() {
			// do nothing so I know I don't have to rely on gc
		}
	}
}
