/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
*/
package ModelInterface.ModelGUI2.xmldb;

import ModelInterface.InterfaceMain;
import ModelInterface.common.LRUCacheMap;
import ModelInterface.ModelGUI2.queries.*;
import ModelInterface.ModelGUI2.xmldb.QueryBinding;
import ModelInterface.ModelGUI2.xmldb.QueryBindingFactory;

import java.io.*;
import java.util.*;
import java.text.SimpleDateFormat;

import javax.swing.*;
import java.awt.Frame;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.BorderLayout;

import org.basex.core.BaseXException;
import org.basex.core.Context;
import org.basex.core.MainOptions;
import org.basex.core.cmd.Check;
import org.basex.core.cmd.Open;
import org.basex.core.cmd.CreateDB;
import org.basex.core.cmd.Close;
import org.basex.core.cmd.Add;
import org.basex.core.cmd.Delete;
import org.basex.query.QueryProcessor;
import org.basex.query.QueryException;
import org.basex.query.value.node.ANode;
import org.basex.query.value.node.DBNode;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.basex.io.out.PrintOutput;
import org.basex.io.serial.Serializer;

public class XMLDB {
    /**
     * The static instance of the XMLDB.
     */
	private static XMLDB xmldbInstance = null;

    /**
     * The database context need to run commands on the DB.
     */
    private Context context = null;

    /**
     * The name of the currently open database.
     */
    private String contName = null;

	/**
	 * Gets the instance of the xml database.
	 * @warning If the database is not open it will return null, ideally it
	 * 	would throw an exception.
	 * @return The instance of the xml database.
	 */
	public static XMLDB getInstance() {
		// WARNING: not thread safe
		if(xmldbInstance == null) {
			// should throw an exception..
			System.out.println("The database is not open.");
		}
		return xmldbInstance;
	}
	/**
	 * Opens a new xml database at the given location.
	 * @param dbLocation The location of the database to open.
	 * @throws Exception If a database is already open or there was an error opening the database.
	 */ 
	public static void openDatabase(String dbLocation) throws Exception {
        openDatabase(dbLocation, null);
    }
	/**
	 * Opens a new xml database at the given location and potentially an existing context.
	 * @param dbLocation The location of the database to open.
	 * @param contextIn An existing daabase context to use an existing connection and support
     *                  using an in memory database that has been loaded already.  If null a
     *                  new default context will be created to open the data base at location dbLocation
     *                  from disk.
	 * @throws Exception If a database is already open or there was an error opening the database.
	 */ 
	public static void openDatabase(String dbLocation, Context contextIn) throws Exception {
		// WARNING: not thread safe
		if(xmldbInstance != null) {
			throw new Exception("Could not open databse because "+xmldbInstance.contName+
					" is still open");
		}
		xmldbInstance = new XMLDB(dbLocation, contextIn);
	}

	/**
	 * Closes the database. Note that all errors on close are ignored.
	 */
	public static void closeDatabase() {
		// WARNING: not thread safe
		if(xmldbInstance != null) {
			try {
                new Close().execute(xmldbInstance.context);
            } catch (BaseXException e) {
                e.printStackTrace();
			} finally {
                xmldbInstance.context = null;
                xmldbInstance.contName = null;
				xmldbInstance = null;
			}
		}
	}

	private XMLDB(String db, Context contextIn) throws Exception {
		openDB(db, contextIn);
	}
	private void openDB(String dbPath, Context contextIn) throws Exception {
		String path = dbPath.substring(0, dbPath.lastIndexOf(System.getProperty("file.separator")));
        // The db Context will check the org.basex.DBPATH property when it is created
        // and use it as the base path for finding all collections/containers
        System.setProperty("org.basex.DBPATH", path);

        if(contextIn == null) {
            context = new Context();
            // Set some default behaviors such as no indexing etc
            // TODO: experiment with these
            context.options.set(MainOptions.ATTRINDEX, false);
            context.options.set(MainOptions.FTINDEX, false);
            context.options.set(MainOptions.UPDINDEX, false);
            context.options.set(MainOptions.CHOP, true);
            context.options.set(MainOptions.ADDCACHE, true);
            context.options.set(MainOptions.INTPARSE, true);
        } else {
            context = contextIn;
        }

        // TODO: worry about spaces?
		// spaces in a database name is illegal for a URI and must be escaped by a %20
		// TODO: any other invalid URI characters are not taken care of here,  need to find a 
		// good util to take care of this but there does not seem to be one in the standard
		// java api
		final String contNameUnmodified = dbPath.substring(dbPath.lastIndexOf(System.getProperty("file.separator"))+1);
		contName = contNameUnmodified.replaceAll(" ", "%20");

        // In memory databases can not be re-opened so just return now
        if(context.options.get(MainOptions.MAINMEM)) {
            return;
        }

        /*
        // The Check command will opent the container if it already exists or create
        // an empty one if it does not.
        new Check(contName).execute(context);
        */
        // Check does not seem to check if the db is simply an empty directory so we will have
        // to do that explicitly here.
        try {
            new Open(contName).execute(context);
        }
        catch(BaseXException e) {
            e.printStackTrace();
            // Ask the user if we should attempt to create a new database since doing so
            // will delete all files in the directory.
            int ans = InterfaceMain.getInstance().showConfirmDialog(
                    "Could not open the database.  Attempt to create a new one?\nWARNING doing so will delete all files in the directory.",
                    "Open DB Error", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, JOptionPane.NO_OPTION);
            if(ans == JOptionPane.YES_OPTION) {
                // If this generates and exception let it pass along.
                new CreateDB(contName).execute(context);
            } else {
                throw e;
            }
        }

		//getVarMetaData(); 
	}
	public void addFile(String fileName) {
        // try to generate a unique name for this file to be added
        SimpleDateFormat format = new SimpleDateFormat("ddMMyy-hhmmss");
        addFile("run_"+format.format(new Date())+".xml", fileName);
	}
    public void addFile(String docName, String fileName) {
	    try {
            new Add(docName, fileName).execute(context);
	    } catch(BaseXException e) {
		    e.printStackTrace();
	    }
	}
	public void removeDoc(String docName) {
		try {
			System.out.println("Removing :"+docName);
            new Delete(docName).execute(context);
		} catch(BaseXException e) {
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
            final int docPre = context.data().resources.doc(aDocName);
            final PrintOutput out = new PrintOutput(aLocation.getPath());
            final Serializer outputter = Serializer.get(out);
            outputter.serialize(new DBNode(context.data(), docPre));
            outputter.close();
            out.close();
			return true;
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	public String getContainer() {
		return contName;
	}

	public QueryProcessor createQuery(String query, Vector<String> queryFunctions, 
			Object[] scenarios, Object[] regions) {
		return createQuery(QueryBindingFactory.getQueryBinding(query, queryFunctions, contName),
				scenarios, regions);
	}
	public QueryProcessor createQuery(QueryGenerator qg, Object[] scenarios, Object[] regions) {
        return createQuery(qg, scenarios, regions, null);
    }
	public QueryProcessor createQuery(QueryGenerator qg, Object[] scenarios, Object[] regions, DbProcInterrupt interrupt) {
		return createQuery(QueryBindingFactory.getQueryBinding(qg, contName), scenarios,
				regions, interrupt);
	}
	public QueryProcessor createQuery(QueryBinding queryBinding, Object[] scenarios, Object[] regions) {
        return createQuery(queryBinding, scenarios, regions, null);
    }
	public QueryProcessor createQuery(QueryBinding queryBinding, Object[] scenarios, Object[] regions, DbProcInterrupt interrupt) {
		String queryComplete = queryBinding.bindToQuery(scenarios, regions);
		System.out.println("About to perform query: "+queryComplete);
        QueryProcessor ret = new QueryProcessor(queryComplete, context);
        if(interrupt != null) {
            interrupt.setProc(ret);
        }
        return ret;
	}
	
    /*
	private static ThreadLocal<Map<Integer, Map<String, String>>> attrCache = 
        new ThreadLocal<Map<Integer, Map<String, String>>>() {
            protected Map<Integer, Map<String, String>> initialValue() {
                final int cacheSize = 500;
                return new LRUCacheMap<Integer, Map<String, String>>(cacheSize);
            }
        };
        */
	//private static Map<String, Map<String, String>> attrCache = new TreeMap<String, Map<String, String>>();
	public static Map<String, String> getAttrMap(Node node) {
		final NamedNodeMap attrs = node.getAttributes();
        final Map<String, String> ret = new TreeMap<String, String>();
        for(int i = 0; i < attrs.getLength(); ++i) {
				Node temp = attrs.item(i);
				ret.put(temp.getNodeName(), temp.getNodeValue());
        }
        return ret;
	}
	public static Map<String, String> getAttrMapWithCache(Node node) {
        /*
		int nodeId = node.getNode().id;
		Map<String, String> ret = attrCache.get().get(nodeId);
		// null means a cahce miss so we will need to go back to the database
		if(ret == null) {
			ret = getAttrMap(node);
			// put the result in the cache for the next time
			attrCache.get().put(nodeId, ret);
		}
		return ret;
        */
        return getAttrMap(node);
	}
	public static String getAllAttr(Map<String, String> attrMap, List<String> showAttrList) {
		String ret;
		if((ret = attrMap.get("name")) != null) {
			ret = "," + ret;
		} else {
			ret = "";
		}
		for(Iterator<Map.Entry<String, String>> it = attrMap.entrySet().iterator(); it.hasNext(); ) {
			Map.Entry<String, String> currAttr = it.next();
			if((!currAttr.getKey().equals("name") &&
					!currAttr.getKey().equals("type") && 
					!currAttr.getKey().equals("unit") && 
					!currAttr.getKey().equals("year")) || (showAttrList != null && showAttrList.contains(currAttr.getKey()))) {
				ret += ","+currAttr.getKey()+"="+currAttr.getValue();
			}
		}
		return !ret.equals("") ? ret.substring(1) : ret;
	}
	public String getQueryFunctionAsDistinctNames() {
		return "declare function local:distinct-node-names ($args as node()*) as xs:string* { fn:distinct-values(for $nname in $args return fn:local-name($nname)) }; local:distinct-node-names";
	}
	/**
	 * Sets the value of the given node to the passed in string content back into the
	 * database.
	 * @param val The node for which to set the value.
	 * @param content The new value to set.
	 */
	public void setValue(ANode val, String content) {
		// simple XQuery update query to replace the value of the given val
		final String setValueXQuery = "declare variable $newValue as xs:string external; replace value of node self::node() with $newValue";
        QueryProcessor queryProc = new QueryProcessor(setValueXQuery, context);
		try {
            // set val as the context so that it knows which node to update
            queryProc.context(val);
			// setting the new value through the variable is safer and allows us to use
			// the same query
			queryProc.bind("newValue", content, "xs:string");
			// not expecting anything to be in the results
			queryProc.iter();
		} catch(QueryException e) {
			e.printStackTrace();
		} finally {
            queryProc.close();
		}
	}
	/**
	 * Inserts a single query cache onto the cache document.
     * @param doc The cache document to add the cache value.
	 * @param hash The hash value to use as an id
	 * @param content The new value to set.
	 */
	public void updateSingleCacheValue(ANode doc, int hash, String content) {
		// simple XQuery update query to replace the value of the given val
		final String setValueXQuery = "declare variable $hashId as xs:integer external; declare variable $newCacheValue as xs:string external; if(exists(self::node()/cache[@id=$hashId])) then replace value of node self::node()/cache[@id=$hashId] with $newCacheValue else insert node element cache{ attribute id { $hashId }, text { $newCacheValue } } into self::node()";
        QueryProcessor queryProc = new QueryProcessor(setValueXQuery, context);
		try {
            // set doc as the context so that it knows which node to update
            queryProc.context(doc);
			// setting the new value through the variable is safer and allows us to use
			// the same query
			queryProc.bind("hashId", hash, "xs:integer");
			queryProc.bind("newCacheValue", content, "xs:string");
			// not expecting anything to be in the results
			queryProc.iter();
		} catch(QueryException e) {
			e.printStackTrace();
		} finally {
            queryProc.close();
		}
	}
	public void addVarMetaData() {
        /*
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Eager);
			final XmlResults res = manager.query(
					"collection('"+contName+"')/*[fn:empty(dbxml:metadata('var'))]", qc);
			// instead of having to determine the size of prog bar, should be figured out
			// by the number of query builders..
			final JProgressBar progBar = new JProgressBar(0, res.size()*7);
			final JDialog jd = createProgressBarGUI(progBar, "Getting Variables", "Finding new variables");
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
					java.util.List<String> getVarFromDoucmentNames = new ArrayList<String>();
					boolean gotVars = false;
					while(res.hasNext()) {
						gotVars = true;
						tempVal = res.next();
						getVarFromDoucmentNames.add(tempVal.getNodeHandle());
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
							strBuff.append(';');
						}
						XmlDocument docTemp = tempVal.asDocument();
						docTemp.setMetaData("", "demographicsVar", new XmlValue(strBuff.toString()));
						tempRes.delete();
						SwingUtilities.invokeLater(incProgress);

						path = "local:distinct-node-names(/scenario/world/*[@type='region']/*[@type='sector']/*[@type='subsector']/*[@type='technology']/*[fn:count(child::text()) = 1])";
						tempRes = getVars(tempVal, path);
						strBuff = new StringBuffer();
						while(tempRes.hasNext()) {
							//System.out.println("adding");
							delVal = tempRes.next();
							strBuff.append(delVal.asString());
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
							strBuff.append(';');
						}
						docTemp.setMetaData("", "GDPVar", new XmlValue(strBuff.toString()));
						final XmlUpdateContext uc = manager.createUpdateContext();
						myContainer.updateDocument(docTemp, uc);
						tempRes.delete();
						getNodeRes.delete();
						SwingUtilities.invokeLater(incProgress);
					}
					res.delete();
					getVarMetaData();
					if(jd != null) {
						SwingUtilities.invokeLater(new Runnable() {
							public void run(){
								jd.setVisible(false);
							}
						});
					}
					String message = gotVars ? "Finished getting new variables." : "There were no new variables.";
					InterfaceMain.getInstance().showMessageDialog(message, "Get Variables", 
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
        */
        // TODO: still necessary?
	}

    /*
	protected XmlResults getVars(XmlValue contextVal, String path) {
		try {
			XmlQueryContext qc = manager.createQueryContext(XmlQueryContext.LiveValues, XmlQueryContext.Lazy);

			/* The xQuery more readable:
			 * declare function local:distinct-node-names ($args as node()*) as xs:string* { 
			 *	fn:distinct-values(for $nname in $args return fn:local-name($nname))
			 * }; 
			 * local:distinct-node-names(/scenario/world/region/supplysector/subsector/technology/*[fn:count(child::text()) = 1])
			 * / 
			String queryStr = "declare function local:distinct-node-names ($args as node()*) as xs:string* { fn:distinct-values(for $nname in $args return fn:local-name($nname)) }; "+path;
			XmlQueryExpression qe = manager.prepare(queryStr, qc);
			return qe.execute(contextVal, qc);
		} catch(XmlException e) {
			e.printStackTrace();
			return null;
		}
	}
    */
    /*
	protected void getVarMetaData() {
		XmlResults res = createQuery("/*[fn:exists(dbxml:metadata('var'))]", null, null, null);
		// TODO: is is safe to assume if one is null they are all null?
		if(SupplyDemandQueryBuilder.varList == null) {
			SupplyDemandQueryBuilder.varList = new LinkedHashMap<String, Boolean>();
			DemographicsQueryBuilder.varList = new LinkedHashMap<String, Boolean>();
			EmissionsQueryBuilder.ghgList = new LinkedHashMap<String, Boolean>();
			EmissionsQueryBuilder.fuelList = new LinkedHashMap<String, Boolean>();
			GDPQueryBuilder.varList = new LinkedHashMap<String, Boolean>();
			ClimateQueryBuilder.varList = new LinkedHashMap<String, Boolean>();
			LandAllocatorQueryBuilder.varList = new LinkedHashMap<String, Boolean>();
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
				}
			}
		} catch(XmlException e) {
			e.printStackTrace();
		}
		res.delete();
		// maybe this should be somewhere else..
		// maybe summable list should be cached..
		Vector<String> funcTemp = new Vector<String>(1,0);
		funcTemp.add("distinct-values");
		res = createQuery("/scenario/output-meta-data/summable/@var", funcTemp, null, null);
		QueryGenerator.sumableList = new Vector<String>();
		QueryGenerator.hasYearList = new Vector<String>();
		XmlValue delValue;
		try {
			while(res.hasNext()) {
				delValue = res.next();
				QueryGenerator.sumableList.add(delValue.asString());
			}
			res.delete();
			res = createQuery("/scenario/output-meta-data/has-year/@var", funcTemp, null, null);
			while(res.hasNext()) {
				delValue = res.next();
				QueryGenerator.hasYearList.add(delValue.asString());
			}
			res.delete();
		} catch(XmlException e) {
			e.printStackTrace();
		}
	}
    */
	// TODO: this is a util method and should be moved somewhere else
	public static JDialog createProgressBarGUI(JProgressBar progBar, String title, String labelStr) {
        final JFrame parentFrame = InterfaceMain.getInstance().getFrame();
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
}
