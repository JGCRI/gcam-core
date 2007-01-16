package ModelInterface.ModelGUI2;

import ModelInterface.ModelGUI2.queries.*;

import java.io.*;
import java.util.*;
import java.awt.Frame;

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
	/*
	String queryFilter;
	String queryFunction;
	*/
	public XMLDB(String db, Frame parentFrame) {
		openDB(db, parentFrame);
	}
	public void openDB(String dbPath, Frame parentFrame) {
		EnvironmentConfig envConfig = new EnvironmentConfig();
		envConfig.setAllowCreate(true);
		envConfig.setCacheSize(100 * 1024 * 1024 );
		envConfig.setInitializeCache(true);
		envConfig.setInitializeLocking(true);
		//envConfig.setVerboseDeadlock(true);
		//envConfig.setVerboseWaitsFor(true);
		//envConfig.setNoLocking(true);

		try {
			String path = dbPath.substring(0, dbPath.lastIndexOf(System.getProperty("file.separator")));
			boolean didUpgradeEnv = false;
			//try {
				dbEnv = new Environment(new File(path), envConfig);
				/* This code is avaibale in 2.3.8 which is not working right..
			} catch(VersionMismatchException vme) {
				int ans = JOptionPane.showConfirmDialog(parentFrame, "The version of the selected database does not match the version\nof the database library. Do you want to attempt to upgrade?\n\nWarning: Upgrading could cause loss of data, it is recomended\nthat you backup your database first.", "DB Version Mismatch Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if(ans == JOptionPane.YES_OPTION) {
					System.out.println("Remove Env");
					Environment.remove(new File(path), true, envConfig);
					dbEnv = new Environment(new File(path), envConfig);
					System.out.println("Done Remove env");
					didUpgradeEnv = true;
				} else {
					return;
				}
			}
			*/
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
					// should not always be true.. something should be done, but if the remove failed it would
					// have probably thrown an exception anyways
					boolean didDel = true;
					if(didDel) {
						dbEnv = new Environment(new File(path), envConfig);
						ls = dbEnv.getLockStats(StatsConfig.DEFAULT);
						System.out.println("Current Locks: "+ls.getNumLocks());
						System.out.println("Current Lockers: "+ls.getNumLockers());
						System.out.println("Current Deadlocks: "+ls.getNumDeadlocks());
						//System.out.println("Current Conflicts: "+ls.getNumConflicts());
					} else {
						System.out.println("Couldn't remove DB Environment");
						JOptionPane.showMessageDialog(parentFrame, "Couldn't remove DB Environment", "DB Fix Error",
								JOptionPane.ERROR_MESSAGE);
						return;
					}
				} else {
					// want a better way to do this anyway
					//((FileChooserDemo)parentFrame).setEnableManageDB(false);
				}
			}
			XmlManagerConfig mc = new XmlManagerConfig();
			mc.setAdoptEnvironment(true);
			manager = new XmlManager(dbEnv, mc);
			XmlContainerConfig cconfig = new XmlContainerConfig();
			cconfig.setAllowCreate(true);
			//cconfig.setIndexNodes(true);
			contName = dbPath.substring(dbPath.lastIndexOf(System.getProperty("file.separator"))+1);
			uc = manager.createUpdateContext();
			try {
				/*
				System.out.println("Before reindex");
				manager.reindexContainer(contName, uc, cconfig);
				System.out.println("After reindex");
				closeDB();
				System.exit(0);
				*/
				System.out.println("Is node container: "+cconfig.getNodeContainer());

				myContainer = manager.openContainer(contName, cconfig);
				// testing index code here should be REMOVED
				System.out.println("Default Index: "+myContainer.getIndexSpecification().getDefaultIndex());
				XmlIndexSpecification is = myContainer.getIndexSpecification();
				//is.addIndex("", "name", "edge-attribute-equality-string");
				//is.addIndex("", "type", "edge-attribute-equality-string");
				//myContainer.setIndexSpecification(is, uc);
				XmlIndexDeclaration dec;
				while((dec = is.next()) != null) {
					System.out.println("Index Name: "+dec.uri+":"+dec.name+" Index: "+dec.index);
				}
				System.out.println("Is node indexed: "+myContainer.getIndexNodes());

				/*
				XmlQueryContext qc = manager.createQueryContext();
				XmlIndexLookup xil = manager.createIndexLookup(myContainer, "", "type", "edge-attribute-equality-string");
				XmlResults res =  xil.execute(qc);
				XmlValue val;
				while((val = res.next()) != null) {
					System.out.print("Node Name: "+val.getNodeName()+"\t");
					System.out.println("Node Value: "+val.getNodeValue());
					val.delete();
				}
				*/

				//System.out.println("The Index: "+myContainer.getIndexSpecification().find("", "type").index);
				// end code to be REMOVED
			} catch(XmlException ve) {
				if(ve.getErrorCode() == XmlException.VERSION_MISMATCH) {
					int ans;
					if(didUpgradeEnv) {
						ans = JOptionPane.YES_NO_OPTION;
					} else {
						ans = JOptionPane.showConfirmDialog(parentFrame, "The version of the selected database does not match the version\nof the database library. Do you want to attempt to upgrade?\n\nWarning: Upgrading could cause loss of data, it is recomended\nthat you backup your database first.", "DB Version Mismatch Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
					}
					if(ans == JOptionPane.YES_OPTION) {
						/*
						JOptionPane.showMessageDialog(parentFrame, "Couldn't Upgrade the database.", 
								"DB Upgrade Error", JOptionPane.ERROR_MESSAGE);
								*/
						System.out.println("Do upgrade");
						manager.upgradeContainer(contName, uc);
						System.out.println("Done upgrade");
						myContainer = manager.openContainer(contName, cconfig);
						System.out.println("Done open");
					} else {
						return;
					}
				} else {
					// print error db not open with exepction
					ve.printStackTrace();
					return;
				}
			}
			//int rejected = manager.getEnvironment().detectDeadlocks(LockDetectMode.OLDEST);
			//System.out.println("Deadlock Detection found "+rejected+" rejected locks");
			printLockStats("openDB");
			/*
			LockStats ls = manager.getEnvironment().getLockStats(StatsConfig.DEFAULT);
			System.out.println("Current Locks: "+ls.getNumLocks());
			System.out.println("Current Lockers: "+ls.getNumLockers());
			System.out.println("Current Deadlocks: "+ls.getNumDeadlocks());
			System.out.println("Current Conflicts: "+ls.getNumConflicts());
			*/
			addVarMetaData(parentFrame);
			//getVarMetaData(); moved because of threading issues
		} catch (XmlException e) {
			// TODO: Tell the user that the database wasn't opened
			e.printStackTrace();
		} catch (DatabaseException dbe) {
			dbe.printStackTrace();
		} catch (FileNotFoundException fe) {
			System.out.println("Could not find path to dbXML Container: "+dbPath);
		}
	}
	public void printLockStats(String where) {
		if(!lockCheck) {
			return;
		}
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
	    //docConfig.setLockMode(LockMode.DEGREE_2);
	    try {
		    /*
		    FileReader is = new FileReader(fileName);
		    StringWriter sw = new StringWriter();
		    char[] buff = new char[4 * 1024];
		    int len;
		    while((len = is.read(buff, 0, 4 * 1024)) != -1) {
			    sw.write(buff, 0, len);
		    }
		    myContainer.putDocument("", sw.toString(), uc, docConfig);
		    */
		    myContainer.putDocument("run", manager.createLocalFileInputStream(fileName), uc, docConfig);
		    printLockStats("addFile");
		    //myContainer.sync();
	    } catch(XmlException e) {
		    e.printStackTrace();
		    /*
	    } catch(FileNotFoundException fe) {
		    fe.printStackTrace();
	    } catch(IOException ioe) {
		    ioe.printStackTrace();
		    */
	    }
	}
	public void removeDoc(String docName) {
		try {
			System.out.println("Removing :"+docName);
			/*
			XmlResults res = createQuery("/scenario");
			while(res.hasNext()) {
				myContainer.deleteDocument(res.next().asDocument(), uc);
			}
			*/
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
	
	public XmlResults createQuery(String query, String queryFilter, Vector<String> queryFunctions) {
		StringBuilder queryBuff = new StringBuilder();
		String[] queries = query.split("\\s*\\|\\s*");
		if(queryFunctions != null) {
			for(Iterator i = queryFunctions.iterator(); i.hasNext(); ) {
				queryBuff.append(i.next()).append('(');
			}
		}
		for(String currQuery : queries) {
			queryBuff.append("collection('").append(contName).append("')");
			if(queryFilter != null) {
				queryBuff.append(queryFilter);
			}
			queryBuff.append(currQuery).append(" | ");
		}
		queryBuff.delete(queryBuff.length()-3, queryBuff.length());
		if(queryFunctions != null) {
			for(int i = 0; i < queryFunctions.size(); ++i) {
				if(queryFunctions.get(i).startsWith("declare")) {
					queryBuff.append(", ())");
				} else {
					queryBuff.append(')');
				}
			}
		}
		/*
		query = queryFunction + "collection('"+contName+"')" + queryFilter + query;
		if(!queryFunction.equals("")) {
			if(queryFunction.startsWith("declare")) {
				query += ", ())";
			} else {
				query += ")";
			}
		}
		*/
		System.out.println("About to perform query: "+queryBuff.toString());
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
			/*
			XmlQueryExpression qeTemp = manager.prepare(queryBuff.toString(), qc);
			System.out.println("Query Plan: "+qeTemp.getQueryPlan());
			return qeTemp.execute(qc);
			*/
			return manager.query(queryBuff.toString(), qc);
		} catch(XmlException e) {
			e.printStackTrace();
			return null;
		}
	}

	public void closeDB() {
		try {
			System.out.println("Closing DB");
			printLockStats("closeDB");
			if(myContainer == null) {
				// didn't open sucessfully
				return;
			}
			myContainer.close();
			manager.close();
		} catch (XmlException e) {
			e.printStackTrace();
		}
	}

	public static boolean hasAttr(XmlValue val) throws XmlException {
		XmlResults attrRes = val.getAttributes();
		boolean ret = attrRes.hasNext();
		attrRes.delete();
		//printLockStats("hasNext"); should be lock safe, now can make it static
		return ret;
		//return val.getAttributes().hasNext();
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
				temp.delete();
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
	public static String getAllAttr(XmlValue node) throws XmlException {
		XmlResults attrRes = node.getAttributes();
		XmlValue temp;
		String ret = "";
		while(attrRes.hasNext()) {
			temp = attrRes.next();
			if(temp.getNodeName().indexOf(":") == -1) {
				ret += ","+temp.getNodeName()+"="+temp.getNodeValue();
			}
			temp.delete();
		}
		attrRes.delete();
		//printLockStats("getAllAtr"); should be lock safe, now can make it static
		return ret.substring(1);
	}
	public String getQueryFunctionAsDistinctNames() {
		return "declare function local:distinct-node-names ($arg as node()*, $before_a as xs:string*) as xs:string* {    for $a at $apos in $arg  let $b := fn:local-name($a), $c_before := fn:count($before_a) + $apos - $apos, $before_a := fn:distinct-values(fn:insert-before($before_a, 0, $b))  where not(fn:count($before_a) = $c_before)  return $b }; local:distinct-node-names";
	}
	public void setValue(XmlValue val, String content) {
		try {
			XmlModify mod = manager.createModify();
			//XmlUpdateContext uc = manager.createUpdateContext();
			uc.setApplyChangesToContainers(false);
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
			mod.addUpdateStep(manager.prepare("./node()", qc), content);
			mod.execute(val.getParentNode(), qc, uc);
		} catch(XmlException e) {
			e.printStackTrace();
		}
	}
	public void addVarMetaData(Frame parentFrame) {
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Eager);
			final XmlResults res = manager.query("collection('"+contName+"')/*[fn:empty(dbxml:metadata('var'))]", qc);
			uc.setApplyChangesToContainers(true);
			// instead of having to determine the size of prog bar, should be figured out
			// by the number of query builders..
			final JProgressBar progBar = new JProgressBar(0, res.size()*7);
			final JDialog jd = createProgressBarGUI(parentFrame, progBar);
			(new Thread(new Runnable() {
				public void run() {
					Runnable incProgress = (new Runnable() {
						public void run() {
							progBar.setValue(progBar.getValue() + 1);
						}
					});
					try {
					XmlResults tempRes;
					XmlValue tempVal;
					//int numDone = 0;
					while(res.hasNext()) {
						System.out.println("Getting new MetaData");
						tempVal = res.next();
						String path = "local:distinct-node-names(/scenario/world/*[@type='region']/demographics//*[fn:count(child::text()) = 1], ())";
						tempRes = getVars(tempVal, path);
						StringBuffer strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString());
							strBuff.append(';');
						}
						XmlDocument docTemp = tempVal.asDocument();
						docTemp.setMetaData("", "demographicsVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[fn:count(child::text()) = 1], ())";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString());
							strBuff.append(';');
						}
						docTemp.setMetaData("", "var", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						XmlQueryContext qcL = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);
						XmlQueryExpression qe = manager.prepare("distinct-values(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[@type='GHG']/@name)", qcL);
						tempRes = qe.execute(tempVal, qcL);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString()).append(';');
						}
						docTemp.setMetaData("", "ghgNames", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						qe = manager.prepare("distinct-values(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[@type='GHG']/emissions/@fuel-name)", qcL);
						tempRes = qe.execute(tempVal, qcL);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString()).append(';');
						}
						docTemp.setMetaData("", "fuelNames", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/climate-model/*[fn:count(child::text()) = 1], ())";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString());
							strBuff.append(';');
						}
						docTemp.setMetaData("", "ClimateVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(//LandLeaf/*[fn:count(child::text()) = 1], ())";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString());
							strBuff.append(';');
						}
						docTemp.setMetaData("", "LandAllocationVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/*[@type='region']/GDP/*[fn:count(child::text()) = 1], ())";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							strBuff.append(tempRes.next().asString());
							strBuff.append(';');
						}
						docTemp.setMetaData("", "GDPVar", new XmlValue(strBuff.toString()));
						myContainer.updateDocument(docTemp, uc);
						docTemp.delete();
						tempVal.delete();
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);
					}
					res.delete();
					printLockStats("addVarMetaData1");
					getVarMetaData();
					//waiting = false;
					//makeWait();
					if(jd != null) {
						SwingUtilities.invokeLater(new Runnable() {
							public void run(){
								jd.setVisible(false);
							}
						});
					}
					} catch(XmlException e) {
						e.printStackTrace();
					}
				}
			})).start();
		} catch(XmlException e) {
			e.printStackTrace();
			closeDB();
		}
		//waiting = true;
		//makeWait();
		printLockStats("addVarMetaData2");
	}

	protected XmlResults getVars(XmlValue contextVal, String path) {
		try {
			//System.out.println("Doesn't have metadata: "+contextVal.asDocument().getName());
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
			String queryStr = "declare function local:distinct-node-names ($arg as node()*, $before_a as xs:string*) as xs:string* {    for $a at $apos in $arg  let $b := fn:local-name($a), $c_before := fn:count($before_a) + $apos - $apos, $before_a := fn:distinct-values(fn:insert-before($before_a, 0, $b))  where not(fn:count($before_a) = $c_before)  return $b }; "+path;
			XmlQueryExpression qe = manager.prepare(queryStr, qc);
			return qe.execute(contextVal, qc);
		} catch(XmlException e) {
			e.printStackTrace();
			closeDB();
			return null;
		}
	}
	protected void getVarMetaData() {
		XmlResults res = createQuery("/*[fn:exists(dbxml:metadata('var'))]", null, null);
		SupplyDemandQueryBuilder.varList = new LinkedHashMap();
		DemographicsQueryBuilder.varList = new LinkedHashMap();
		EmissionsQueryBuilder.ghgList = new LinkedHashMap();
		EmissionsQueryBuilder.fuelList = new LinkedHashMap();
		GDPQueryBuilder.varList = new LinkedHashMap();
		ClimateQueryBuilder.varList = new LinkedHashMap();
		LandAllocatorQueryBuilder.varList = new LinkedHashMap();
		XmlMetaData md;
		try {
			while(res.hasNext()) {
				XmlValue vt = res.next();
				XmlDocument docTemp = vt.asDocument();
				System.out.println("Gathering metadata from a doc "+docTemp.getName());
				XmlMetaDataIterator it = docTemp.getMetaDataIterator();
				while((md = it.next()) != null) {
					if(md.get_name().equals("var")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							SupplyDemandQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("demographicsVar")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							DemographicsQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("ghgNames")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							EmissionsQueryBuilder.ghgList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("fuelNames")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							EmissionsQueryBuilder.fuelList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("GDPVar")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							GDPQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("ClimateVar")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							ClimateQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					} else if(md.get_name().equals("LandAllocationVar")) {
						String[] vars = md.get_value().asString().split(";");
						for(int i = 0; i < vars.length; ++i) {
							//System.out.println(vars[i]);
							LandAllocatorQueryBuilder.varList.put(vars[i], new Boolean(false));
						}
					}
				}
				it.delete();
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
		res = createQuery("/scenario/output-meta-data/summable/@var", null, funcTemp);
		QueryGenerator.sumableList = new Vector();
		QueryGenerator.hasYearList = new Vector<String>();
		try {
			while(res.hasNext()) {
				QueryGenerator.sumableList.add(res.next().asString());
			}
			res.delete();
			res = createQuery("/scenario/output-meta-data/has-year/@var", null, funcTemp);
			while(res.hasNext()) {
				QueryGenerator.hasYearList.add(res.next().asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
		printLockStats("getVarMetaData");
	}
	private JDialog createProgressBarGUI(Frame parentFrame, JProgressBar progBar) {
		if(progBar.getMaximum() == 0) {
			return null;
		}
		JDialog filterDialog = new JDialog(parentFrame, "Getting Variables", false);
		JPanel all = new JPanel();
		all.setLayout( new BoxLayout(all, BoxLayout.Y_AXIS));
		progBar.setPreferredSize(new Dimension(200, 20));
		JLabel label = new JLabel("Finding new variables");
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
}
